use std::{
    cell::RefCell,
    ffi::CString,
    rc::Rc,
    sync::mpsc::{channel, sync_channel, Receiver, SyncSender},
    thread::JoinHandle,
};

use font_kit::handle::Handle as FontHandle;
use gleam::gl::{self, Gl};
use glutin::{
    self,
    dpi::{LogicalSize, PhysicalPosition},
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoopProxy},
    monitor::MonitorHandle,
    window::Window,
    ContextWrapper, PossiblyCurrent,
};

#[cfg(unix)]
use glutin::platform::unix::EventLoopExtUnix;
#[cfg(windows)]
use glutin::platform::windows::EventLoopExtUnix;

use webrender::{self, api::units::*, api::*};

use crate::{lisp::ExternalPtr, remacs_sys::wr_output};

use super::display_info::DisplayInfoRef;
use super::font::FontRef;
use super::texture::TextureResourceManager;

pub enum EmacsGUIEvent {
    Flush(SyncSender<()>),
    ReadBytes(DeviceIntRect, SyncSender<ImageKey>),
}

pub type GUIEvent = Event<'static, EmacsGUIEvent>;

pub struct Output {
    // Extend `wr_output` struct defined in `wrterm.h`
    pub output: wr_output,

    pub font: FontRef,
    pub fontset: i32,

    pub render_api: RenderApi,
    pub loop_thread: JoinHandle<()>,
    pub document_id: DocumentId,

    pub display_list_builder: Option<DisplayListBuilder>,

    pub background_color: ColorF,
    pub cursor_color: ColorF,
    pub cursor_foreground_color: ColorF,

    window: Window,

    event_loop_proxy: EventLoopProxy<EmacsGUIEvent>,
    color_bits: u8,

    pub keyboard_fd: i32,
    event_rx: Receiver<GUIEvent>,
}

impl Output {
    pub fn new() -> Self {
        let (
            api,
            window,
            document_id,
            loop_thread,
            event_loop_proxy,
            color_bits,
            event_rx,
            keyboard_fd,
        ) = Self::create_webrender_window();

        Self {
            output: wr_output::default(),
            font: FontRef::default(),
            fontset: 0,
            render_api: api,
            loop_thread,
            document_id,
            display_list_builder: None,
            background_color: ColorF::WHITE,
            cursor_color: ColorF::BLACK,
            cursor_foreground_color: ColorF::WHITE,
            window,
            event_loop_proxy,
            color_bits,
            keyboard_fd,
            event_rx,
        }
    }

    fn create_webrender_window() -> (
        RenderApi,
        Window,
        DocumentId,
        JoinHandle<()>,
        EventLoopProxy<EmacsGUIEvent>,
        u8,
        std::sync::mpsc::Receiver<GUIEvent>,
        i32,
    ) {
        let (webrender_tx, webrender_rx) = sync_channel(1);

        let (event_tx, event_rx) = channel::<GUIEvent>();
        let mut pipes: [i32; 2] = [-1, -1];
        unsafe {
            libc::pipe(&mut pipes[0]);
            libc::fcntl(pipes[0], libc::F_SETFL, libc::O_NONBLOCK);
            libc::fcntl(pipes[1], libc::F_SETFL, libc::O_NONBLOCK)
        };

        let window_loop_thread = std::thread::spawn(move || {
            let events_loop = glutin::event_loop::EventLoop::new_any_thread();
            let window_builder = glutin::window::WindowBuilder::new().with_maximized(true);

            let window_context = glutin::ContextBuilder::new()
                .build_windowed(window_builder, &events_loop)
                .unwrap();

            let current_context = unsafe { window_context.make_current() }.unwrap();
            let (current_context, window) = unsafe { current_context.split() };

            std::thread::sleep_ms(100);

            let gl = Self::get_gl_api(&current_context);

            gl.clear_color(1.0, 1.0, 1.0, 1.0);
            gl.clear(self::gl::COLOR_BUFFER_BIT);
            gl.flush();
            current_context.swap_buffers().ok();

            let events_loop_proxy = events_loop.create_proxy();

            std::thread::sleep_ms(100);

            let device_pixel_ratio = window.scale_factor() as f32;

            let device_size = {
                let size = window.inner_size();
                DeviceIntSize::new(size.width as i32, size.height as i32)
            };

            let webrender_opts = webrender::RendererOptions {
                device_pixel_ratio,
                clear_color: None,
                ..webrender::RendererOptions::default()
            };

            let notifier = Box::new(Notifier::new(events_loop_proxy.clone()));

            let (mut renderer, sender) =
                webrender::Renderer::new(gl.clone(), notifier, webrender_opts, None, device_size)
                    .unwrap();

            let texture_resources = Rc::new(RefCell::new(TextureResourceManager::new(
                gl.clone(),
                sender.create_api(),
            )));

            let external_image_handler =
                texture_resources.borrow_mut().new_external_image_handler();

            renderer.set_external_image_handler(external_image_handler);

            let api = sender.create_api();

            let document_id = api.add_document(device_size, 0 /* layer */);

            let color_bits = current_context.get_pixel_format().color_bits;

            webrender_tx
                .send((api, window, document_id, color_bits, events_loop_proxy))
                .unwrap();

            events_loop.run(move |e, _, control_flow| {
                *control_flow = ControlFlow::Wait;

                match e {
                    Event::WindowEvent {
                        event: WindowEvent::KeyboardInput { .. },
                        ..
                    }
                    | Event::WindowEvent {
                        event: WindowEvent::ReceivedCharacter(_),
                        ..
                    }
                    | Event::WindowEvent {
                        event: WindowEvent::ModifiersChanged(_),
                        ..
                    }
                    | Event::WindowEvent {
                        event: WindowEvent::Focused(_),
                        ..
                    } => {
                        event_tx.send(e.to_static().unwrap()).unwrap();
                        unsafe {
                            libc::write(
                                pipes[1],
                                CString::new("0").unwrap().as_ptr() as *const libc::c_void,
                                2,
                            )
                        };
                    }
                    Event::UserEvent(EmacsGUIEvent::Flush(s)) => {
                        renderer.update();
                        renderer.render(device_size).unwrap();
                        let _ = renderer.flush_pipeline_info();
                        current_context.swap_buffers().ok();

                        texture_resources.borrow_mut().clear();

                        s.send(()).unwrap();
                    }
                    Event::UserEvent(EmacsGUIEvent::ReadBytes(device_rect, sender)) => {
                        let mut fb_rect =
                            FramebufferIntRect::from_untyped(&device_rect.to_untyped());

                        if !renderer.device.surface_origin_is_top_left() {
                            fb_rect.origin.y =
                                device_size.height - fb_rect.origin.y - fb_rect.size.height;
                        }

                        let need_flip = !renderer.device.surface_origin_is_top_left();

                        let (image_key, texture_id) = texture_resources.borrow_mut().new_image(
                            document_id,
                            fb_rect.size,
                            need_flip,
                        );

                        sender.send(image_key).unwrap();

                        gl.bind_texture(gl::TEXTURE_2D, texture_id);

                        gl.copy_tex_sub_image_2d(
                            gl::TEXTURE_2D,
                            0,
                            0,
                            0,
                            fb_rect.origin.x,
                            fb_rect.origin.y,
                            fb_rect.size.width,
                            fb_rect.size.height,
                        );

                        gl.bind_texture(gl::TEXTURE_2D, 0);
                    }
                    _ => {}
                };
            })
        });

        let (api, window, document_id, color_bits, event_loop_proxy) = webrender_rx.recv().unwrap();

        let pipeline_id = PipelineId(0, 0);

        let mut txn = Transaction::new();
        txn.set_root_pipeline(pipeline_id);
        api.send_transaction(document_id, txn);

        (
            api,
            window,
            document_id,
            window_loop_thread,
            event_loop_proxy,
            color_bits,
            event_rx,
            pipes[0],
        )
    }

    fn get_gl_api(window_context: &ContextWrapper<PossiblyCurrent, ()>) -> Rc<dyn Gl> {
        match window_context.get_api() {
            glutin::Api::OpenGl => unsafe {
                gl::GlFns::load_with(|symbol| window_context.get_proc_address(symbol) as *const _)
            },
            glutin::Api::OpenGlEs => unsafe {
                gl::GlesFns::load_with(|symbol| window_context.get_proc_address(symbol) as *const _)
            },
            glutin::Api::WebGl => unimplemented!(),
        }
    }

    fn get_size(window: &Window) -> (DeviceIntSize, LayoutSize) {
        let device_pixel_ratio = window.scale_factor() as f32;

        let physical_size = window.inner_size();

        let logical_size = physical_size.to_logical::<f32>(device_pixel_ratio as f64);

        let layout_size = LayoutSize::new(logical_size.width as f32, logical_size.height as f32);
        let device_size =
            DeviceIntSize::new(physical_size.width as i32, physical_size.height as i32);

        (device_size, layout_size)
    }

    pub fn show_window(&self) {
        self.window.set_visible(true);
    }

    pub fn hide_window(&self) {
        self.window.set_visible(false);
    }

    pub fn set_display_info(&mut self, mut dpyinfo: DisplayInfoRef) {
        self.output.display_info = dpyinfo.as_mut();
    }

    pub fn display_info(&self) -> DisplayInfoRef {
        self.output.display_info.into()
    }

    pub fn get_inner_size(&self) -> LogicalSize<f32> {
        let scale_factor = self.window.scale_factor();

        self.window.inner_size().to_logical(scale_factor)
    }

    pub fn display<F>(&mut self, f: F)
    where
        F: Fn(&mut DisplayListBuilder, SpaceAndClipInfo),
    {
        let pipeline_id = PipelineId(0, 0);
        if self.display_list_builder.is_none() {
            let (_, layout_size) = Self::get_size(&self.window);
            let builder = DisplayListBuilder::new(pipeline_id, layout_size);

            self.display_list_builder = Some(builder);
        }

        if let Some(builder) = &mut self.display_list_builder {
            let space_and_clip = SpaceAndClipInfo::root_scroll(pipeline_id);

            f(builder, space_and_clip);
        }
    }

    pub fn flush(&mut self) {
        let builder = std::mem::replace(&mut self.display_list_builder, None);

        if let Some(builder) = builder {
            let (_, layout_size) = Self::get_size(&self.window);

            let epoch = Epoch(0);
            let mut txn = Transaction::new();

            txn.set_display_list(epoch, None, layout_size, builder.finalize(), true);

            txn.generate_frame();

            self.render_api.send_transaction(self.document_id, txn);

            self.render_api.flush_scene_builder();

            let (sender, receiver) = sync_channel(1);

            let _ = self
                .event_loop_proxy
                .send_event(EmacsGUIEvent::Flush(sender));

            receiver.recv().unwrap()
        }
    }

    pub fn read_pixels_rgba8_into_image(&mut self, device_rect: DeviceIntRect) -> ImageKey {
        let (texture_sender, texture_receiver) = sync_channel(1);

        let _ = self
            .event_loop_proxy
            .send_event(EmacsGUIEvent::ReadBytes(device_rect, texture_sender));

        texture_receiver.recv().unwrap()
    }

    pub fn clear_display_list_builder(&mut self) {
        let _ = std::mem::replace(&mut self.display_list_builder, None);
    }

    pub fn add_font_instance(&self, font_key: FontKey, pixel_size: i32) -> FontInstanceKey {
        let mut txn = Transaction::new();

        let font_instance_key = self.render_api.generate_font_instance_key();

        txn.add_font_instance(
            font_instance_key,
            font_key,
            app_units::Au::from_px(pixel_size),
            None,
            None,
            vec![],
        );

        self.render_api.send_transaction(self.document_id, txn);
        font_instance_key
    }

    pub fn add_font(&self, font_handle: &FontHandle) -> FontKey {
        let mut txn = Transaction::new();

        let font_key = self.render_api.generate_font_key();
        match font_handle {
            FontHandle::Path { path, font_index } => {
                let font = NativeFontHandle {
                    path: path.clone().into_os_string().into(),
                    index: *font_index,
                };
                txn.add_native_font(font_key, font);
            }
            FontHandle::Memory { bytes, font_index } => {
                txn.add_raw_font(font_key, bytes.to_vec(), *font_index);
            }
        }

        self.render_api.send_transaction(self.document_id, txn);

        font_key
    }

    pub fn get_color_bits(&self) -> u8 {
        self.color_bits
    }

    pub fn get_glyph_dimensions(
        &self,
        font: FontInstanceKey,
        glyph_indices: Vec<GlyphIndex>,
    ) -> Vec<Option<GlyphDimensions>> {
        self.render_api.get_glyph_dimensions(font, glyph_indices)
    }

    pub fn get_available_monitors(&self) -> impl Iterator<Item = MonitorHandle> {
        self.window.available_monitors()
    }

    pub fn get_primary_monitor(&self) -> MonitorHandle {
        self.window.primary_monitor()
    }

    pub fn get_position(&self) -> Option<PhysicalPosition<i32>> {
        self.window.outer_position().ok()
    }
    pub fn poll_events<F>(&mut self, mut f: F)
    where
        F: FnMut(GUIEvent),
    {
        let mut buffer: [i32; 10] = Default::default();

        let _ = unsafe {
            libc::read(
                self.keyboard_fd,
                &mut buffer[0] as *mut _ as *mut libc::c_void,
                10,
            )
        };
        for e in self.event_rx.try_iter() {
            f(e);
        }
    }
}

pub type OutputRef = ExternalPtr<Output>;

impl From<*mut wr_output> for OutputRef {
    fn from(ptr: *mut wr_output) -> OutputRef {
        OutputRef::new(ptr as *mut Output)
    }
}

struct Notifier {
    event_loop_proxy: EventLoopProxy<EmacsGUIEvent>,
}

impl Notifier {
    fn new(event_loop_proxy: EventLoopProxy<EmacsGUIEvent>) -> Notifier {
        Notifier { event_loop_proxy }
    }
}

impl RenderNotifier for Notifier {
    fn clone(&self) -> Box<dyn RenderNotifier> {
        Box::new(Notifier {
            event_loop_proxy: self.event_loop_proxy.clone(),
        })
    }

    fn wake_up(&self) {}

    fn new_frame_ready(
        &self,
        _: DocumentId,
        _scrolled: bool,
        _composite_needed: bool,
        _render_time: Option<u64>,
    ) {
    }
}

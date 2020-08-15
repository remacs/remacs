use font_kit::handle::Handle as FontHandle;
use gleam::gl;
use glutin::{
    self,
    dpi::LogicalSize,
    event_loop::{EventLoop, EventLoopProxy},
    window::Window,
    ContextWrapper, PossiblyCurrent,
};
use webrender::{self, api::units::*, api::*, Renderer};

use crate::{lisp::ExternalPtr, remacs_sys::wr_output};

use super::display_info::DisplayInfoRef;
use super::font::FontRef;

pub struct Output {
    // Extend `wr_output` struct defined in `wrterm.h`
    pub output: wr_output,

    pub font: FontRef,
    pub fontset: i32,

    pub window_context: ContextWrapper<PossiblyCurrent, Window>,
    pub renderer: Renderer,
    pub render_api: RenderApi,
    pub events_loop: EventLoop<()>,
    pub document_id: DocumentId,

    pub display_list_builder: Option<DisplayListBuilder>,
    pub current_txn: Option<Transaction>,
}

impl Output {
    pub fn new() -> Self {
        let (api, renderer, window_context, events_loop, document_id) =
            Self::create_webrender_window();

        Self {
            output: wr_output::default(),
            font: FontRef::default(),
            fontset: 0,
            window_context,
            renderer,
            render_api: api,
            events_loop,
            document_id,
            display_list_builder: None,
            current_txn: None,
        }
    }

    fn create_webrender_window() -> (
        RenderApi,
        Renderer,
        ContextWrapper<PossiblyCurrent, Window>,
        EventLoop<()>,
        DocumentId,
    ) {
        let events_loop = glutin::event_loop::EventLoop::new();
        let window_builder = glutin::window::WindowBuilder::new().with_maximized(true);

        let window_context = glutin::ContextBuilder::new()
            .build_windowed(window_builder, &events_loop)
            .unwrap();

        let window_context = unsafe { window_context.make_current() }.unwrap();

        let gl = match window_context.get_api() {
            glutin::Api::OpenGl => unsafe {
                gl::GlFns::load_with(|symbol| window_context.get_proc_address(symbol) as *const _)
            },
            glutin::Api::OpenGlEs => unsafe {
                gl::GlesFns::load_with(|symbol| window_context.get_proc_address(symbol) as *const _)
            },
            glutin::Api::WebGl => unimplemented!(),
        };

        gl.clear_color(1.0, 1.0, 1.0, 1.0);
        gl.clear(self::gl::COLOR_BUFFER_BIT);
        gl.flush();
        window_context.swap_buffers().ok();

        let gl_window = window_context.window();

        let device_pixel_ratio = gl_window.scale_factor() as f32;

        let device_size = {
            let size = gl_window.inner_size();
            DeviceIntSize::new(size.width as i32, size.height as i32)
        };

        let webrender_opts = webrender::RendererOptions {
            device_pixel_ratio,
            clear_color: Some(ColorF::WHITE),
            ..webrender::RendererOptions::default()
        };

        let notifier = Box::new(Notifier::new(events_loop.create_proxy()));
        let (renderer, sender) =
            webrender::Renderer::new(gl.clone(), notifier, webrender_opts, None, device_size)
                .unwrap();

        let api = sender.create_api();

        let (device_size, _layout_size) = Self::get_size(&gl_window);

        let document_id = api.add_document(device_size, 0 /* layer */);

        let pipeline_id = PipelineId(0, 0);

        let mut txn = Transaction::new();
        txn.set_root_pipeline(pipeline_id);
        api.send_transaction(document_id, txn);

        (api, renderer, window_context, events_loop, document_id)
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
        self.window_context.window().set_visible(true);
    }

    pub fn hide_window(&self) {
        self.window_context.window().set_visible(false);
    }

    pub fn set_display_info(&mut self, mut dpyinfo: DisplayInfoRef) {
        self.output.display_info = dpyinfo.as_mut();
    }

    pub fn display_info(&self) -> DisplayInfoRef {
        self.output.display_info.into()
    }

    pub fn get_inner_size(&self) -> LogicalSize<f32> {
        let window = self.window_context.window();
        let scale_factor = window.scale_factor();

        window.inner_size().to_logical(scale_factor)
    }

    pub fn display<F>(&mut self, f: F)
    where
        F: Fn(&mut DisplayListBuilder, &mut RenderApi, &mut Transaction, SpaceAndClipInfo),
    {
        let pipeline_id = PipelineId(0, 0);
        if self.display_list_builder.is_none() {
            let (_, layout_size) = Self::get_size(&self.window_context.window());
            let builder = DisplayListBuilder::new(pipeline_id, layout_size);

            self.display_list_builder = Some(builder);
            self.current_txn = Some(Transaction::new());
        }

        match (&mut self.display_list_builder, &mut self.current_txn) {
            (Some(builder), Some(txn)) => {
                let space_and_clip = SpaceAndClipInfo::root_scroll(pipeline_id);

                f(builder, &mut self.render_api, txn, space_and_clip);
            }
            _ => {}
        };
    }

    pub fn flush(&mut self) {
        let (device_size, layout_size) = Self::get_size(&self.window_context.window());

        // hard code epoch now
        let epoch = Epoch(0);

        let builder = std::mem::replace(&mut self.display_list_builder, None);
        let txn = std::mem::replace(&mut self.current_txn, None);

        match (builder, txn) {
            (Some(builder), Some(mut txn)) => {
                txn.set_display_list(epoch, None, layout_size, builder.finalize(), true);

                txn.generate_frame();

                self.render_api.send_transaction(self.document_id, txn);

                self.render_api.flush_scene_builder();

                self.renderer.update();
                self.renderer.render(device_size).unwrap();
                let _ = self.renderer.flush_pipeline_info();
                self.window_context.swap_buffers().ok();
            }
            _ => {}
        }
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
}

pub type OutputRef = ExternalPtr<Output>;

impl From<*mut wr_output> for OutputRef {
    fn from(ptr: *mut wr_output) -> OutputRef {
        OutputRef::new(ptr as *mut Output)
    }
}

struct Notifier {
    event_loop_proxy: EventLoopProxy<()>,
}

impl Notifier {
    fn new(event_loop_proxy: EventLoopProxy<()>) -> Notifier {
        Notifier { event_loop_proxy }
    }
}

impl RenderNotifier for Notifier {
    fn clone(&self) -> Box<dyn RenderNotifier> {
        Box::new(Notifier {
            event_loop_proxy: self.event_loop_proxy.clone(),
        })
    }

    fn wake_up(&self) {
        let _ = self.event_loop_proxy.send_event(());
    }

    fn new_frame_ready(
        &self,
        _: DocumentId,
        _scrolled: bool,
        _composite_needed: bool,
        _render_time: Option<u64>,
    ) {
        self.wake_up();
    }
}

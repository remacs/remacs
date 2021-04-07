use std::{cell::RefCell, collections::HashMap, rc::Rc};

use gleam::gl::{self, Gl};
use webrender::{self, api::units::*, api::*};

type TextureTable = HashMap<gl::GLuint, (FramebufferIntSize, bool)>;

pub struct TextureResourceManager {
    textures: Rc<RefCell<TextureTable>>,
    images: Vec<(ImageKey, DocumentId)>,
    gl: Rc<dyn Gl>,
    render_api: RenderApi,
}

impl TextureResourceManager {
    pub fn new(gl: Rc<dyn Gl>, render_api: RenderApi) -> TextureResourceManager {
        Self {
            textures: Rc::new(RefCell::new(HashMap::new())),
            images: Vec::new(),
            gl,
            render_api,
        }
    }

    pub fn new_image(
        &mut self,
        document_id: DocumentId,
        size: FramebufferIntSize,
        need_flip: bool,
    ) -> (ImageKey, gl::GLuint) {
        let texture_id = self.new_texture_in_gl(size);
        let image_key =
            self.add_image_in_webrender(document_id, texture_id, size.width, size.height);

        self.insert(document_id, texture_id, image_key, size, need_flip);

        (image_key, texture_id)
    }

    fn new_texture_in_gl(&mut self, size: FramebufferIntSize) -> gl::GLuint {
        let texture_id = self.gl.gen_textures(1)[0];

        self.gl.bind_texture(gl::TEXTURE_2D, texture_id);

        self.gl.tex_parameter_i(
            gl::TEXTURE_2D,
            gl::TEXTURE_MAG_FILTER,
            gl::LINEAR as gl::GLint,
        );

        self.gl.tex_parameter_i(
            gl::TEXTURE_2D,
            gl::TEXTURE_MIN_FILTER,
            gl::LINEAR as gl::GLint,
        );

        self.gl.tex_parameter_i(
            gl::TEXTURE_2D,
            gl::TEXTURE_WRAP_S,
            gl::CLAMP_TO_EDGE as gl::GLint,
        );

        self.gl.tex_parameter_i(
            gl::TEXTURE_2D,
            gl::TEXTURE_WRAP_T,
            gl::CLAMP_TO_EDGE as gl::GLint,
        );

        self.gl.tex_image_2d(
            gl::TEXTURE_2D,
            0,
            gl::RGBA as gl::GLint,
            size.width,
            size.height,
            0,
            gl::BGRA,
            gl::UNSIGNED_BYTE,
            None,
        );

        self.gl.bind_texture(gl::TEXTURE_2D, 0);

        texture_id
    }

    fn add_image_in_webrender(
        &mut self,
        document_id: DocumentId,
        texture_id: gl::GLuint,
        width: i32,
        height: i32,
    ) -> ImageKey {
        let mut txn = Transaction::new();

        let image_key = self.render_api.generate_image_key();

        txn.add_image(
            image_key,
            ImageDescriptor::new(
                width as i32,
                height as i32,
                ImageFormat::RGBA8,
                ImageDescriptorFlags::IS_OPAQUE,
            ),
            ImageData::External(ExternalImageData {
                id: ExternalImageId(texture_id as u64),
                channel_index: 0,
                image_type: ExternalImageType::TextureHandle(TextureTarget::Default),
            }),
            None,
        );

        self.render_api.send_transaction(document_id, txn);

        image_key
    }

    pub fn insert(
        &mut self,
        document_id: DocumentId,
        texture_id: gl::GLuint,
        image_key: ImageKey,
        size: FramebufferIntSize,
        need_flip: bool,
    ) {
        self.textures
            .borrow_mut()
            .insert(texture_id, (size, need_flip));

        self.images.push((image_key, document_id));
    }

    pub fn new_external_image_handler(&self) -> Box<dyn ExternalImageHandler> {
        let handler = ExternalHandler {
            texture: self.textures.clone(),
        };

        Box::new(handler)
    }

    pub fn clear(&mut self) {
        self.clear_textures();
        self.clear_images();
    }

    fn clear_textures(&mut self) {
        let mut textures = self.textures.borrow_mut();

        let texture_ids: Vec<gl::GLuint> = textures.keys().copied().collect();

        self.gl.delete_textures(&texture_ids);

        textures.clear();
    }

    fn clear_images(&mut self) {
        for (image_key, document_id) in self.images.iter() {
            let mut txn = Transaction::new();
            txn.delete_image(*image_key);
            self.render_api.send_transaction(*document_id, txn);
        }

        self.images.clear();
    }
}

struct ExternalHandler {
    texture: Rc<RefCell<TextureTable>>,
}

impl ExternalImageHandler for ExternalHandler {
    fn lock(
        &mut self,
        key: ExternalImageId,
        _channel_index: u8,
        _rendering: ImageRendering,
    ) -> ExternalImage {
        let texture_id = key.0;

        let textures = self.texture.borrow();
        let (size, need_filp) = textures.get(&(texture_id as u32)).unwrap();

        let uv = if *need_filp {
            TexelRect::new(0.0, size.height as f32, size.width as f32, 0.0)
        } else {
            TexelRect::new(0.0, 0.0, size.width as f32, size.height as f32)
        };

        ExternalImage {
            uv,
            source: ExternalImageSource::NativeTexture(texture_id as u32),
        }
    }
    fn unlock(&mut self, _key: ExternalImageId, _channel_index: u8) {}
}

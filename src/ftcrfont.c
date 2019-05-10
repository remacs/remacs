/* ftcrfont.c -- FreeType font driver on cairo.
   Copyright (C) 2015-2019 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


#include <config.h>
#include <stdio.h>
#include <math.h>
#include <cairo-ft.h>

#include "lisp.h"
#include "xterm.h"
#include "blockinput.h"
#include "composite.h"
#include "font.h"
#include "ftfont.h"
#include "pdumper.h"

#define METRICS_NCOLS_PER_ROW	(128)

enum metrics_status
  {
    METRICS_INVALID = -1,    /* metrics entry is invalid */
  };

#define METRICS_STATUS(metrics)	((metrics)->ascent + (metrics)->descent)
#define METRICS_SET_STATUS(metrics, status) \
  ((metrics)->ascent = 0, (metrics)->descent = (status))

static int
ftcrfont_glyph_extents (struct font *font,
                        unsigned glyph,
                        struct font_metrics *metrics)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;
  int row, col;
  struct font_metrics *cache;

  row = glyph / METRICS_NCOLS_PER_ROW;
  col = glyph % METRICS_NCOLS_PER_ROW;
  if (row >= ftcrfont_info->metrics_nrows)
    {
      ftcrfont_info->metrics =
	xrealloc (ftcrfont_info->metrics,
		  sizeof (struct font_metrics *) * (row + 1));
      memset (ftcrfont_info->metrics + ftcrfont_info->metrics_nrows, 0,
	      (sizeof (struct font_metrics *)
	       * (row + 1 - ftcrfont_info->metrics_nrows)));
      ftcrfont_info->metrics_nrows = row + 1;
    }
  if (ftcrfont_info->metrics[row] == NULL)
    {
      struct font_metrics *new;
      int i;

      new = xmalloc (sizeof (struct font_metrics) * METRICS_NCOLS_PER_ROW);
      for (i = 0; i < METRICS_NCOLS_PER_ROW; i++)
	METRICS_SET_STATUS (new + i, METRICS_INVALID);
      ftcrfont_info->metrics[row] = new;
    }
  cache = ftcrfont_info->metrics[row] + col;

  if (METRICS_STATUS (cache) == METRICS_INVALID)
    {
      cairo_glyph_t cr_glyph = {.index = glyph};
      cairo_text_extents_t extents;

      FT_Activate_Size (ftcrfont_info->ft_size_draw);
      cairo_scaled_font_glyph_extents (ftcrfont_info->cr_scaled_font,
				       &cr_glyph, 1, &extents);
      cache->lbearing = floor (extents.x_bearing);
      cache->rbearing = ceil (extents.width + extents.x_bearing);
      cache->width = lround (extents.x_advance);
      cache->ascent = ceil (- extents.y_bearing);
      cache->descent = ceil (extents.height + extents.y_bearing);
    }

  if (metrics)
    *metrics = *cache;

  return cache->width;
}

static Lisp_Object
ftcrfont_list (struct frame *f, Lisp_Object spec)
{
  Lisp_Object list = ftfont_list (f, spec), tail;

  for (tail = list; CONSP (tail); tail = XCDR (tail))
    ASET (XCAR (tail), FONT_TYPE_INDEX, Qftcr);
  return list;
}

static Lisp_Object
ftcrfont_match (struct frame *f, Lisp_Object spec)
{
  Lisp_Object entity = ftfont_match (f, spec);

  if (VECTORP (entity))
    ASET (entity, FONT_TYPE_INDEX, Qftcr);
  return entity;
}

static Lisp_Object
ftcrfont_open (struct frame *f, Lisp_Object entity, int pixel_size)
{
  Lisp_Object font_object;

  FT_UInt size = XFIXNUM (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;
  font_object = font_build_object (VECSIZE (struct font_info),
				   Qftcr, entity, size);
  block_input ();
  font_object = ftfont_open2 (f, entity, pixel_size, font_object);
  if (FONT_OBJECT_P (font_object))
    {
      struct font *font = XFONT_OBJECT (font_object);
      struct font_info *ftcrfont_info = (struct font_info *) font;
      FT_Face ft_face = ftcrfont_info->ft_size->face;

      font->driver = &ftcrfont_driver;
      FT_New_Size (ft_face, &ftcrfont_info->ft_size_draw);
      FT_Activate_Size (ftcrfont_info->ft_size_draw);
      if (ftcrfont_info->bitmap_strike_index < 0)
	FT_Set_Pixel_Sizes (ft_face, 0, font->pixel_size);
      else
	FT_Select_Size (ft_face, ftcrfont_info->bitmap_strike_index);
      cairo_font_face_t *font_face =
	cairo_ft_font_face_create_for_ft_face (ft_face, 0);
      cairo_matrix_t font_matrix, ctm;
      cairo_matrix_init_scale (&font_matrix, pixel_size, pixel_size);
      cairo_matrix_init_identity (&ctm);
      cairo_font_options_t *options = cairo_font_options_create ();
      ftcrfont_info->cr_scaled_font =
	cairo_scaled_font_create (font_face, &font_matrix, &ctm, options);
      cairo_font_face_destroy (font_face);
      cairo_font_options_destroy (options);
      ftcrfont_info->metrics = NULL;
      ftcrfont_info->metrics_nrows = 0;
      if (ftcrfont_info->bitmap_strike_index >= 0)
	{
	  /* Several members of struct font/font_info set by
	     ftfont_open2 are bogus.  Recalculate them with cairo
	     scaled font functions.  */
	  cairo_font_extents_t extents;
	  cairo_scaled_font_extents (ftcrfont_info->cr_scaled_font, &extents);
	  font->ascent = lround (extents.ascent);
	  Lisp_Object val = assq_no_quit (QCminspace,
					  AREF (entity, FONT_EXTRA_INDEX));
	  if (!(CONSP (val) && NILP (XCDR (val))))
	    {
	      font->descent = lround (extents.descent);
	      font->height = font->ascent + font->descent;
	    }
	  else
	    {
	      font->height = lround (extents.height);
	      font->descent = font->height - font->ascent;
	    }

	  cairo_glyph_t stack_glyph;
	  int n = 0;
	  font->min_width = font->average_width = font->space_width = 0;
	  for (char c = 32; c < 127; c++)
	    {
	      cairo_glyph_t *glyphs = &stack_glyph;
	      int num_glyphs = 1;
	      cairo_status_t status =
		cairo_scaled_font_text_to_glyphs (ftcrfont_info->cr_scaled_font,
						  0, 0, &c, 1,
						  &glyphs, &num_glyphs,
						  NULL, NULL, NULL);

	      if (status == CAIRO_STATUS_SUCCESS)
		{
		  if (glyphs != &stack_glyph)
		    cairo_glyph_free (glyphs);
		  else
		    {
		      int this_width =
			ftcrfont_glyph_extents (font, stack_glyph.index, NULL);

		      if (this_width > 0
			  && (! font->min_width
			      || font->min_width > this_width))
			font->min_width = this_width;
		      if (c == 32)
			font->space_width = this_width;
		      font->average_width += this_width;
		      n++;
		    }
		}
	    }
	  if (n > 0)
	    font->average_width /= n;

	  font->underline_position = -1;
	  font->underline_thickness = 0;
	}
    }
  unblock_input ();

  return font_object;
}

static void
ftcrfont_close (struct font *font)
{
  if (font_data_structures_may_be_ill_formed ())
    return;

  struct font_info *ftcrfont_info = (struct font_info *) font;
  int i;

  block_input ();
  for (i = 0; i < ftcrfont_info->metrics_nrows; i++)
    if (ftcrfont_info->metrics[i])
      xfree (ftcrfont_info->metrics[i]);
  if (ftcrfont_info->metrics)
    xfree (ftcrfont_info->metrics);
  FT_Done_Size (ftcrfont_info->ft_size_draw);
  cairo_scaled_font_destroy (ftcrfont_info->cr_scaled_font);
  unblock_input ();

  ftfont_close (font);
}

static void
ftcrfont_text_extents (struct font *font,
                       const unsigned *code,
                       int nglyphs,
                       struct font_metrics *metrics)
{
  int width, i;

  block_input ();
  width = ftcrfont_glyph_extents (font, code[0], metrics);
  for (i = 1; i < nglyphs; i++)
    {
      struct font_metrics m;
      int w = ftcrfont_glyph_extents (font, code[i], metrics ? &m : NULL);

      if (metrics)
	{
	  if (width + m.lbearing < metrics->lbearing)
	    metrics->lbearing = width + m.lbearing;
	  if (width + m.rbearing > metrics->rbearing)
	    metrics->rbearing = width + m.rbearing;
	  if (m.ascent > metrics->ascent)
	    metrics->ascent = m.ascent;
	  if (m.descent > metrics->descent)
	    metrics->descent = m.descent;
	}
      width += w;
    }
  unblock_input ();

  if (metrics)
    metrics->width = width;
}

static int
ftcrfont_get_bitmap (struct font *font, unsigned int code,
		     struct font_bitmap *bitmap, int bits_per_pixel)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;

  if (ftcrfont_info->bitmap_strike_index < 0)
    return ftfont_get_bitmap (font, code, bitmap, bits_per_pixel);

  return -1;
}

static int
ftcrfont_anchor_point (struct font *font, unsigned int code, int idx,
		       int *x, int *y)
{
  struct font_info *ftcrfont_info = (struct font_info *) font;

  if (ftcrfont_info->bitmap_strike_index < 0)
    return ftfont_anchor_point (font, code, idx, x, y);

  return -1;
}

static Lisp_Object
ftcrfont_shape (Lisp_Object lgstring)
{
#if defined HAVE_M17N_FLT && defined HAVE_LIBOTF
  struct font *font = CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring));
  struct font_info *ftcrfont_info = (struct font_info *) font;

  if (ftcrfont_info->bitmap_strike_index < 0)
    return ftfont_shape (lgstring);
#endif

  return make_fixnum (0);
}

static int
ftcrfont_draw (struct glyph_string *s,
               int from, int to, int x, int y, bool with_background)
{
  struct frame *f = s->f;
  struct face *face = s->face;
  struct font_info *ftcrfont_info = (struct font_info *) s->font;
  cairo_t *cr;
  cairo_glyph_t *glyphs;
  cairo_surface_t *surface;
  cairo_surface_type_t surface_type;
  int len = to - from;
  int i;

  block_input ();

  cr = x_begin_cr_clip (f, s->gc);

  if (with_background)
    {
      x_set_cr_source_with_gc_background (f, s->gc);
      cairo_rectangle (cr, x, y - FONT_BASE (face->font),
		       s->width, FONT_HEIGHT (face->font));
      cairo_fill (cr);
    }

  glyphs = alloca (sizeof (cairo_glyph_t) * len);
  for (i = 0; i < len; i++)
    {
      glyphs[i].index = s->char2b[from + i];
      glyphs[i].x = x;
      glyphs[i].y = y;
      x += (s->padding_p ? 1 : ftcrfont_glyph_extents (s->font,
                                                       glyphs[i].index,
                                                       NULL));
    }

  x_set_cr_source_with_gc_foreground (f, s->gc);
  cairo_set_scaled_font (cr, ftcrfont_info->cr_scaled_font);

  FT_Activate_Size (ftcrfont_info->ft_size_draw);
  cairo_show_glyphs (cr, glyphs, len);
  surface = cairo_get_target (cr);
  /* XXX: It used to be necessary to flush when exporting.  It might
     be the case that this is no longer necessary.  */
  surface_type = cairo_surface_get_type (surface);
  if (surface_type != CAIRO_SURFACE_TYPE_XLIB
      && (surface_type != CAIRO_SURFACE_TYPE_IMAGE
	  || cairo_image_surface_get_format (surface) != CAIRO_FORMAT_ARGB32))
    cairo_surface_flush (surface);

  x_end_cr_clip (f);

  unblock_input ();

  return len;
}



static void syms_of_ftcrfont_for_pdumper (void);

struct font_driver const ftcrfont_driver =
  {
  .type = LISPSYM_INITIALLY (Qftcr),
  .get_cache = ftfont_get_cache,
  .list = ftcrfont_list,
  .match = ftcrfont_match,
  .list_family = ftfont_list_family,
  .open = ftcrfont_open,
  .close = ftcrfont_close,
  .has_char = ftfont_has_char,
  .encode_char = ftfont_encode_char,
  .text_extents = ftcrfont_text_extents,
  .draw = ftcrfont_draw,
  .get_bitmap = ftcrfont_get_bitmap,
  .anchor_point = ftcrfont_anchor_point,
#ifdef HAVE_LIBOTF
  .otf_capability = ftfont_otf_capability,
#endif
  .shape = ftcrfont_shape,
#ifdef HAVE_OTF_GET_VARIATION_GLYPHS
  .get_variation_glyphs = ftfont_variation_glyphs,
#endif
  .filter_properties = ftfont_filter_properties,
  .combining_capability = ftfont_combining_capability,
  };

void
syms_of_ftcrfont (void)
{
  DEFSYM (Qftcr, "ftcr");
  pdumper_do_now_and_after_load (syms_of_ftcrfont_for_pdumper);
}

static void
syms_of_ftcrfont_for_pdumper (void)
{
  register_font_driver (&ftcrfont_driver, NULL);
}

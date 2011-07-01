#include <config.h>

#include <signal.h>

#include <stdio.h>
#include <setjmp.h>
#ifdef HAVE_X_WINDOWS

#include "lisp.h"
#include "blockinput.h"
#include "syssignal.h"

#include "xterm.h"
#include <X11/cursorfont.h>

#ifndef makedev
#include <sys/types.h>
#endif /* makedev */

#ifdef BSD_SYSTEM
#include <sys/ioctl.h>
#endif /* ! defined (BSD_SYSTEM) */

#include "systime.h"

#ifndef INCLUDED_FCNTL
#include <fcntl.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>

#include "charset.h"
#include "character.h"
#include "coding.h"
#include "ccl.h"
#include "frame.h"
#include "dispextern.h"
#include "fontset.h"
#include "termhooks.h"
#include "termopts.h"
#include "termchar.h"
#include "emacs-icon.h"
#include "disptab.h"
#include "buffer.h"
#include "window.h"
#include "keyboard.h"
#include "intervals.h"
#include "process.h"
#include "atimer.h"
#include "keymap.h"


#ifdef USE_X_TOOLKIT
#include <X11/Shell.h>
#endif
#include <X11/extensions/Xcomposite.h>
#include <X11/extensions/Xrender.h>
#include <cairo.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gtkutil.h"
#include "font.h"
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>

#ifdef HAVE_GTK3
//for gtk3; sockets and plugs
#include <gtk/gtkx.h>
#include "emacsgtkfixed.h"
#endif

#include <librsvg/rsvg.h>

#ifdef HAVE_GOOCANVAS
#include <goocanvas.h>
#endif

#ifdef HAVE_CLUTTER
#include <clutter/clutter.h>
#include <clutter-gtk/clutter-gtk.h>
#endif


#ifdef HAVE_WEBKIT
#include <webkitgtk.h>
#endif


#ifdef HAVE_WEBKIT_OSR
#include <webkit/webkitwebview.h>
#endif



#include "xwidget.h"

//TODO should of course not be a hardcoded array but I can't be bothered atm
//just a fixed array of xwidgets for now
//would need to be hashtables or something

#define MAX_XWIDGETS 100
struct xwidget xwidgets[MAX_XWIDGETS];
struct xwidget_view xwidget_views[MAX_XWIDGETS]; 




Lisp_Object Qxwidget;
Lisp_Object Qxwidget_id;
Lisp_Object Qtitle;
Lisp_Object Qxwidget_set_keyboard_grab;
Lisp_Object Qxwidget_embed_steal_window;
Lisp_Object Qxwidget_info;
Lisp_Object Qxwidget_resize_internal;
Lisp_Object Qxwidget_send_keyboard_event;

Lisp_Object Qbutton, Qtoggle, Qslider, Qsocket, Qcairo, Qwebkit,
  Qwebkit_osr, QCplist;


extern Lisp_Object  QCtype;   
extern Lisp_Object QCwidth, QCheight;  

struct xwidget_view* xwidget_view_lookup(struct xwidget* xw,     struct window *w);

int
xwidget_hidden(struct xwidget_view *xv)
{
  return  xv->hidden;
}


static void
buttonclick_handler (GtkWidget * widget, gpointer data)
{
  struct xwidget *xw = (struct xwidget *) data;
  struct input_event event;
  Lisp_Object frame;
  FRAME_PTR f = NULL;//(FRAME_PTR) g_object_get_data (G_OBJECT (xw->widget), XG_FRAME_DATA); //TODO
  printf ("button clicked xw:%d id:%d\n", xw, xw->id);

  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;

  XSETFRAME (frame, f);

  event.frame_or_window = Qnil;	//frame; //how to get the frame here?


  event.arg = Qnil;
  event.arg = Fcons (make_number (xw->id), event.arg);
  event.arg = Fcons (intern ("buttonclick"), event.arg);

  kbd_buffer_store_event (&event);


}


static void
send_xembed_ready_event (int xwid, int xembedid)
{
  struct input_event event;
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;	//frame; //how to get the frame here? //TODO i store it in the xwidget now

  event.arg = Qnil;
  event.arg = Fcons (make_number (xembedid), event.arg);
  event.arg = Fcons (intern ("xembed-ready"), event.arg);
  event.arg = Fcons (make_number (xwid), event.arg);


  kbd_buffer_store_event (&event);

}

int xwidget_query_composition_called = 0;
int hasNamePixmap = 0;







void
xwidget_show_view (struct xwidget_view *xv)
{
  //printf("xwidget %d shown\n",xw->id);
  xv->hidden = 0;
  gtk_widget_show(GTK_WIDGET(xv->widgetwindow));
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), GTK_WIDGET (xv->widgetwindow),  xv->x, xv->y);
}


/* hide an xvidget view */
void
xwidget_hide_view (struct xwidget_view *xv)
{
  //printf("xwidget %d hidden\n",xw->id);
  xv->hidden = 1;
  //gtk_widget_hide(GTK_WIDGET(xw->widgetwindow));
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), GTK_WIDGET (xv->widgetwindow),
                  10000, 10000);
}


void xwidget_plug_added(GtkSocket *socket,
                        gpointer   user_data)
{
  //hmm this doesnt seem to get called for foreign windows
  printf("xwidget_plug_added\n");
}

gboolean xwidget_plug_removed(GtkSocket *socket,
                        gpointer   user_data)
{
  printf("xwidget_plug_removed\n");
  return TRUE; /* dont run the default handler because that kills the socket and we want to reuse it*/
}


void xwidget_slider_changed (GtkRange *range,
                             gpointer  user_data)
{
  //slider value changed. change value of siblings
  //correspondingly. but remember that changing value will again
  //trigger signal

  //TODO MVC view storage wont be an array futureish so the loop needs to change eventually
  //TODO MVC it would be nice if this code could be reusable but, alas, C is not a functional language
  //issues are:
  // - the type of the controllers value (double, boolean etc)
  // - the getter and setter (but they can be func pointers)
  // a behemoth macro is always an option.
  double v;
  printf("slider changed val:%f\n", v=gtk_range_get_value(range));

  struct xwidget_view* xvp = g_object_get_data (G_OBJECT (range), XG_XWIDGET_VIEW);
  struct xwidget_view* xv;
  //block sibling views signal handlers
  for (int i = 0; i < MAX_XWIDGETS; i++)
    {
      xv = &xwidget_views[i];
      if(xvp->model == xv->model){
        g_signal_handler_block( xv->widget,xv->handler_id);
      }
    }
  //set values of sibling views and unblock
  for (int i = 0; i < MAX_XWIDGETS; i++)
    {
      xv = &xwidget_views[i];
      if(xvp->model == xv->model){
        gtk_range_set_value(xv->widget, v);
        g_signal_handler_unblock( xv->widget,xv->handler_id);
      }
    }

}

double osr_dbg_color=0;
void webkit_osr_redraw_child (  struct xwidget* xw, GtkWidget *widget)
{

  //this stuff is different in gtk3
#ifndef HAVE_GTK3  
  cairo_t *cr;


  GdkPixmap *src_pixmap;
  src_pixmap = gtk_offscreen_window_get_pixmap(xw->widgetwindow_osr);

  //g_object_ref(src_pixmap);//TODO needs to be unrefed eventually, if we are to use his method

  
  printf("webkit_osr_redraw_child xw.id:%d xw.type:%d window:%d\n", xw->id,xw->type, gtk_widget_get_window (widget));

  cr = gdk_cairo_create (gtk_widget_get_window (widget));

  cairo_rectangle(cr, 0,0, xw->width, xw->height);
  cairo_clip(cr);

  // debugging redraw:
  //  - the bg colors always change, so theres no error in signal handling
  //  - i get this error now and then:
  //(emacs:7109): GLib-GObject-WARNING **: invalid cast from `GdkOffscreenWindow' to `GdkDrawableImplX11'
  // seems to happen in webkit actually. see README
  
  if(1){ //redraw debug hack
    cairo_set_source_rgb(cr, osr_dbg_color, 1.0, 0.2);
    cairo_rectangle(cr, 0,0, xw->width, xw->height);
    cairo_fill(cr);
    osr_dbg_color+=0.1;
    if(osr_dbg_color>1.0)
      osr_dbg_color=0.0;
    
  }
  
  gdk_cairo_set_source_pixmap (cr, src_pixmap, 0,0); //deprecated. use gdk_cairo_set_source_window
  //gdk_cairo_set_source_window(cr, src_pixmap, 0,0);
  
  cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
  cairo_paint_with_alpha (cr, 0.7);
  //cairo_paint(cr);


  cairo_destroy (cr);
#elseif
  cairo_t *cr;
  cairo_surface_t * *src_pixmap;
  src_pixmap =    gtk_offscreen_window_get_surface (xw->widgetwindow_osr);

  //g_object_ref(src_pixmap);//TODO needs to be unrefed eventually, if we are to use his method

  
  printf("webkit_osr_redraw_child gtk3 xw.id:%d xw.type:%d window:%d\n", xw->id,xw->type, gtk_widget_get_window (widget));

  cr = gdk_cairo_create (gtk_widget_get_window (widget));

  cairo_rectangle(cr, 0,0, xw->width, xw->height);
  cairo_clip(cr);

  // debugging redraw:
  //  - the bg colors always change, so theres no error in signal handling
  //  - i get this error now and then:
  //(emacs:7109): GLib-GObject-WARNING **: invalid cast from `GdkOffscreenWindow' to `GdkDrawableImplX11'
  // seems to happen in webkit actually. see README
  
  if(1){ //redraw debug hack
    cairo_set_source_rgb(cr, osr_dbg_color, 1.0, 0.2);
    cairo_rectangle(cr, 0,0, xw->width, xw->height);
    cairo_fill(cr);
    osr_dbg_color+=0.1;
    if(osr_dbg_color>1.0)
      osr_dbg_color=0.0;
    
  }
  
  cairo_set_source_surface (cr, src_pixmap, 0,0); 

  
  cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
  cairo_paint_with_alpha (cr, 0.7);
  //cairo_paint(cr);
  cairo_destroy (cr);
#endif
}

/* when the on-screen webkit peer view gets exposed this signal is called.
   it copies the bitmap from the off-screen webkit instance */
gboolean webkit_osr_expose_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data) 
 {                                                                               
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);  
  webkit_osr_redraw_child(xw, widget);
  return FALSE;
 }                                                                               

/* when the off-screen webkit master view changes this signal is called.
   it copies the bitmap from the off-screen webkit instance */
gboolean webkit_osr_damage_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data) 
 {
   //TODO this is wrong! should just oueu a redraw of onscreen widget
   struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
   struct xwidget_view* xv;
   //webkit_osr_redraw_child(xw, widget);
   for (int i = 0; i < MAX_XWIDGETS; i++)//todo mvc refactor
    {
      xv = &xwidget_views[i];
      if(xv->model == xw){
        gtk_widget_queue_draw (xv->widget); //redraw all views, the master has changed
      }
    }

   return TRUE;
 }                                                                               


//for gtk3
gboolean
xwidget_osr_draw_callback (GtkWidget *widget, cairo_t *cr, gpointer data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  struct xwidget_view* xv = (struct xwidget_viev*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET_VIEW);    
  cairo_surface_t* src_pixmap;
  //src_pixmap =    gtk_offscreen_window_get_surface (xw->widgetwindow_osr);

  
  printf("xwidget_osr_draw_callback gtk3 xw.id:%d xw.type:%d window:%d srcpix:%d vis:%d\n",
         xw->id,xw->type, gtk_widget_get_window (widget),   src_pixmap, gtk_widget_get_visible (xw->widget_osr));

  //  cr = gdk_cairo_create (gtk_widget_get_window (widget));

  cairo_rectangle(cr, 0,0, xv->clipx, xv->clipy);//xw->width, xw->height);
  cairo_clip(cr);

  // debugging redraw:
  //  - the bg colors always change, so theres no error in signal handling
  //  - i get this error now and then:
  //(emacs:7109): GLib-GObject-WARNING **: invalid cast from `GdkOffscreenWindow' to `GdkDrawableImplX11'
  // seems to happen in webkit actually. see README
  
  if(1){ //redraw debug hack. 
    cairo_set_source_rgb(cr, osr_dbg_color, 1.0, 0.2);
    cairo_rectangle(cr, 0,0, xw->width, xw->height);
    cairo_fill(cr);
    osr_dbg_color+=0.1;
    if(osr_dbg_color>1.0)
      osr_dbg_color=0.0;
    
  }

  //maybe use below instead?
  gtk_widget_draw                     (xw->widget_osr,  cr);
  
  //cairo_set_source_surface (cr, src_pixmap, 0,0); 
  cairo_set_operator (cr, CAIRO_OPERATOR_OVER);

  cairo_paint_with_alpha (cr, 0.7);
  //cairo_paint(cr);

  
  return FALSE;
}



int xwidget_view_index=0;

/* initializes and does initial placement of an xwidget view on screen */
struct xwidget_view*
xwidget_init_view ( 
                   struct xwidget *xww,
                   struct glyph_string *s,
                   int x, int y)
{
  struct xwidget_view *xv = &xwidget_views[xwidget_view_index++];
  GdkColor color;
  
  xv->initialized = 1;
  xv->w = s->w;
  xv->model = xww;
  
  //widget creation
  if(EQ(xww->type, Qbutton))
    {
      xv->widget = gtk_button_new_with_label (xww->title);
      g_signal_connect (G_OBJECT (xv->widget), "clicked",
                        G_CALLBACK (buttonclick_handler), xww); //the model rather than the view
    } else if (EQ(xww->type, Qtoggle)) {
      xv->widget = gtk_toggle_button_new_with_label (xww->title);
    } else if (EQ(xww->type, Qsocket)) {
      xv->widget = gtk_socket_new ();
      //gtk_widget_set_app_paintable (xw->widget, TRUE); //workaround for composited sockets

      //gdk_color_parse("blue",&color); //the blue color never seems to show up. something else draws a grey bg
      //gtk_widget_modify_bg(xv->widget, GTK_STATE_NORMAL, &color);
      g_signal_connect_after(xv->widget, "plug-added", G_CALLBACK(xwidget_plug_added), "plug added");
      g_signal_connect_after(xv->widget, "plug-removed", G_CALLBACK(xwidget_plug_removed), "plug removed");              
    } else if (EQ(xww->type, Qslider)) {
      xv->widget =
        //gtk_hscale_new (GTK_ADJUSTMENT(gtk_adjustment_new (0.0, 0.0, 100.0, 1.0, 10.0, 10.0)));
        gtk_hscale_new_with_range ( 0.0, 100.0, 10.0);
      gtk_scale_set_draw_value (GTK_SCALE (xv->widget), FALSE);	//i think its emacs role to show text and stuff, so disable the widgets own text
      xv->handler_id = g_signal_connect_after(xv->widget, "value-changed", G_CALLBACK(xwidget_slider_changed), "slider changed");
    } else if (EQ(xww->type, Qcairo)) {
      //Cairo view
      //uhm cairo is differentish in gtk 3. 
      //gdk_cairo_create (gtk_widget_get_window (f->gwfixed));
#ifdef HAVE_GOOCANVAS
    xv->widget = goo_canvas_new();
    GooCanvasItem *root, *rect_item, *text_item;
    goo_canvas_set_bounds (GOO_CANVAS (xv->widget), 0, 0, 1000, 1000);
     root = goo_canvas_get_root_item (GOO_CANVAS (xv->widget));
      rect_item = goo_canvas_rect_new (root, 100, 100, 400, 400,
                                   "line-width", 10.0,
                                   "radius-x", 20.0,
                                   "radius-y", 10.0,
                                   "stroke-color", "yellow",
                                   "fill-color", "red",
                                   NULL);

  text_item = goo_canvas_text_new (root, "Hello World", 300, 300, -1,
                                   GTK_ANCHOR_CENTER,
                                   "font", "Sans 24",
                                   NULL);
  goo_canvas_item_rotate (text_item, 45, 300, 300);
  
#endif
#ifdef HAVE_CLUTTER
        xv->widget = gtk_clutter_embed_new ();;
        ClutterActor *stage = NULL;
        stage = gtk_clutter_embed_get_stage (GTK_CLUTTER_EMBED (        xv->widget));
        ClutterColor stage_color = { 0xaa, 0xaa, 0xaa, 0xff }; /* Black */
        clutter_stage_set_color (CLUTTER_STAGE (stage), &stage_color);

        ClutterActor *  texture =  clutter_cairo_texture_new (1000, 1000);
        clutter_container_add_actor(stage, texture);
        clutter_actor_set_position(texture, 0,0);
        clutter_actor_show(texture);

        cairo_t *cr;
        cr = clutter_cairo_texture_create (CLUTTER_CAIRO_TEXTURE (texture));

        /* draw on the context */
        RsvgHandle *h =  rsvg_handle_new_from_file  ("/tmp/tst.svg",
                                                     NULL);
        
        rsvg_handle_render_cairo(h, cr);
        cairo_destroy (cr);
          
        /* Show the stage: */
        clutter_actor_show (stage);        
#endif        
        
  } else if (EQ(xww->type, Qwebkit)) {
#ifdef HAVE_WEBKIT
        xv->widget = webkit_web_view_new();
        webkit_web_view_load_uri(xv->widget, "http://www.fsf.org"); 
#endif        
  } else if (EQ(xww->type, Qwebkit_osr)) {
#ifdef HAVE_WEBKIT_OSR
    xv->widget = gtk_drawing_area_new();
    gtk_widget_set_app_paintable (    xv->widget, TRUE); //because expose event handling
#endif        
#ifdef HAVE_GTK3 //and webkit_osr
    g_signal_connect (G_OBJECT (    xv->widget), "draw",                    
                      G_CALLBACK (xwidget_osr_draw_callback), NULL);                  
    
#else
    g_signal_connect (G_OBJECT (    xv->widget), "expose_event",                    
                      G_CALLBACK (webkit_osr_expose_event_callback), NULL);                  
#endif


  } else return NULL;
  
  //widget realization
  //make container widget 1st, and put the actual widget inside the container
  //later, drawing should crop container window if necessary to handle case where xwidget
  //is partially obscured by other emacs windows
  xv->emacswindow = GTK_CONTAINER (s->f->gwfixed);
  //xw->widgetwindow = GTK_CONTAINER (gtk_layout_new (NULL, NULL));
  //xw->widgetwindow = GTK_CONTAINER (gtk_offscreen_window_new ());

  //xv->widgetwindow = GTK_CONTAINER (gtk_fixed_new ()); //works well for clipping on gtk2 not gtk3
  //xv->widgetwindow = GTK_CONTAINER (gtk_event_box_new ()); //doesnt help clipping gtk3
  //xv->widgetwindow = GTK_CONTAINER (gtk_scrolled_window_new (NULL, NULL)); //clips in gtk3
  xv->widgetwindow = GTK_CONTAINER (gtk_viewport_new (NULL, NULL)); //clips in gtk3

  
  gtk_widget_set_size_request (GTK_WIDGET (xv->widgetwindow), xww->width, xww->height);
  /* GtkAllocation a; */
  /* a.x=0;  a.y=0;   a.width=xww->width;  a.height=xww->height; */
  /* gtk_widget_set_allocation (GTK_WIDGET (xv->widget), &a); */

  //gtk_widget_set_has_window(GTK_WIDGET (  xv->widgetwindow), TRUE);
  //if gtk_fixed doesnt have a window it will surprisingly not honor
  //setsize so that children gets clipped later. the documentation is
  //not consistent regarding if its legal to call this method.

  //doesnt help on gtk3, and the docs seem clearer there that this is
  //an internal function



  //gtk_layout_set_size (GTK_LAYOUT (xw->widgetwindow), xw->width, xw->height);
  gtk_container_add (xv->widgetwindow, xv->widget);

  //gtk_scrolled_window_add_with_viewport (xv->widgetwindow, xv->widget); // when using scrollw

  //store some xwidget data in the gtk widgets
  g_object_set_data (G_OBJECT (xv->widget), XG_FRAME_DATA, (gpointer) (s->f)); //the emacs frame
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET, (gpointer) (xww)); //the xwidget
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET_VIEW, (gpointer) (xv)); //the xwidget
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET, (gpointer) (xww)); //the xwidget  
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET_VIEW, (gpointer) (xv)); //the xwidget

  
  gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xww->width, xww->height);
  gtk_fixed_put (EMACS_FIXED (s->f->gwfixed), GTK_WIDGET (xv->widgetwindow), x, y);
  xv->x = x;  xv->y = y;
  gtk_widget_show_all (GTK_WIDGET (xv->widgetwindow));

  
  //this seems to enable xcomposition. later we need to paint ourselves somehow,
  //since the widget is no longer responsible for painting itself
  //if(xw->type!=3)  //im having trouble with compositing and sockets. hmmm.
      //gdk_window_set_composited (xw->widget->window, TRUE);
  //gdk_window_set_composited (GTK_LAYOUT (xw->widgetwindow)->bin_window, TRUE);  
  // gtk_widget_set_double_buffered (xw->widget,FALSE);
  // gtk_widget_set_double_buffered (xw->widgetwindow,FALSE);  
  //gdk_window_set_composited (xw->widgetwindow, TRUE);  
  //g_signal_connect_after(xw->widget, "expose-event", G_CALLBACK(xwidget_composite_draw), "widget exposed");
  //g_signal_connect_after(xw->widgetwindow, "expose-event", G_CALLBACK(xwidget_composite_draw_widgetwindow), "widgetwindow exposed");  
  //  g_signal_connect_after(xw->widget, "damage-event", G_CALLBACK(xwidget_composite_draw), "damaged");  
  
  //widgettype specific initialization only possible after realization
 if (EQ(xww->type, Qsocket)) {
      printf ("xwid:%d socket id:%x %d\n",
              xww->id,
              gtk_socket_get_id (GTK_SOCKET (xv->widget)),
              gtk_socket_get_id (GTK_SOCKET (xv->widget)));
      send_xembed_ready_event (xww->id,
                               gtk_socket_get_id (GTK_SOCKET (xv->widget)));
      //gtk_widget_realize(xw->widget);
 }
  return xv;
}


void
x_draw_xwidget_glyph_string (struct glyph_string *s)
{
  /*
    this method is called by the redisplay engine and is supposed to put the xwidget on screen.

    must handle both live xwidgets, and phantom xwidgets.

    BUG it seems this method for some reason is called with bad s->x and s->y sometimes.
    When this happens the xwidget doesnt move on screen as it should.
    This mightbe because of x_scroll_run. Emacs decides to scroll the screen by blitting sometimes.
    then emacs doesnt try to actualy call the paint routines, which means this here code will never
    run so the xwidget wont know it has been moved.

    Solved temporarily by never optimizing in try_window_reusing_current_matrix().

    BUG the phantoming code doesnt work very well when the live xwidget is off screen.
    you will get weirdo display artefacts. Composition ought to solve this, since that means the live window is
    always available in an off-screen buffer. My current attempt at composition doesnt work properly however.


   */
  int box_line_hwidth = eabs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height = s->height;

  //int drawing_in_selected_window = (XWINDOW (FRAME_SELECTED_WINDOW (s->f))) == (s->w);
  //TODO drawing_in_selected_window can be true for several windows if we have several frames.
  //we also need to check that the xwidget is to be drawn inside a window on a frame where it originaly lives.
  //otherwise draw a phantom, or maybe reparent the xwidget.

  struct xwidget *xww = &xwidgets[s->xwidget_id];
  struct xwidget_view *xv = xwidget_view_lookup(xww, (s->w));



  
  int clipx; int clipy;

  /*printf("x_draw_xwidget_glyph_string: id:%d %d %d  (%d,%d,%d,%d) selected win:%d\n",
     s->xwidget_id, box_line_hwidth, box_line_vwidth,
         s->x, s->y, s->height, s->width,
         drawing_in_selected_window);*/

  int x = s->x;
  int y = s->y + (s->height / 2) - (xww->height / 2);
  int doingsocket = 0;
  int moved=0;

  if (xv == NULL){
    xv = xwidget_init_view (xww, s, x, y); //once for each view 
  }

  //calculate clip widht and height, which is used both for the xwidget
  //and its phantom counterpart
  clipx = min (xww->width, WINDOW_RIGHT_EDGE_X (s->w) - x - WINDOW_RIGHT_SCROLL_BAR_AREA_WIDTH(s->w) - WINDOW_RIGHT_FRINGE_WIDTH(s->w));
  clipy = min (xww->height,
               WINDOW_BOTTOM_EDGE_Y (s->w) - WINDOW_MODE_LINE_HEIGHT (s->w) - y);

  //TODO:
  // 1) always draw live xwidget in slected window
  // (2) if there were no live instances of the xwidget in selected window, also draw it live)
  // 3) if there was a live xwidget previously, now phantom it.
  if (1)//moving to MVC pattern, there will be no "phantom" support for a while //drawing_in_selected_window)
    {
      moved = (xv->x != x) || (xv->y != y);
      if(moved)
        printf ("live xwidget moved: id:%d (%d,%d)->(%d,%d)\n", xww->id, xv->x, xv->y, x, y);
      //xw refers to the *live* instance of the xwidget, so only
      //update coords when drawing in the selected window
      xv->x = x;
      xv->y = y;
      if (moved)	//has it moved?
        {
          if (!xwidget_hidden(xv))	//hidden equals not being seen in the live window
            {
              gtk_fixed_move (GTK_FIXED (s->f->gwfixed),
                              GTK_WIDGET (xv->widgetwindow), x, y);
            }
        }
      //clip the widget window if some parts happen to be outside drawable area
      //an emacs window is not a gtk window, a gtk window covers the entire frame
      //cliping might have changed even if we havent actualy moved, we try figure out when we need to reclip for real
      if((xv->clipx != clipx) || (xv->clipy != clipy)){
        gtk_widget_set_size_request (GTK_WIDGET (xv->widgetwindow),
                                     clipx, clipy);
        printf("reclip %d %d -> %d %d\n",xv->clipx, xv->clipy,  clipx, clipy );

        //allocation debugging. the correct values cant be expected to show upp immediately, but eventually they should get to be ok
        // this is because we dont know when the container gets around to doing layout
        GtkAllocation galloc;
        gtk_widget_get_allocation(GTK_WIDGET (xv->widgetwindow), &galloc);
        printf("allocation %d %d , %d %d\n", galloc.x,galloc.y,galloc.width,galloc.height);
        
        xv->clipx = clipx; xv->clipy = clipy;
      }
      //a live xwidget paints itself. when using composition, that
      //happens through the expose handler for the xwidget
      //if emacs wants to repaint the area where the widget lives, queue a redraw
      if (!xwidget_hidden(xv))
        gtk_widget_queue_draw (xv->widget);
    }
  else
    {
      //ok, we are painting the xwidgets in non-selected window, so draw a phantom
      //printf("draw phantom xwidget at:%d %d\n",x,y);
      //xwidget_composite_draw_phantom (xw, x, y, clipx, clipy); //TODO MVC there will be very few cases of phantoming
    }
}

#ifdef HAVE_WEBKIT
DEFUN ("xwidget-webkit-goto-uri", Fxwidget_webkit_goto_uri,  Sxwidget_webkit_goto_uri, 2, 2, 0,
       doc:	/* webkit goto uri.*/
       )
  (Lisp_Object xwidget_id, Lisp_Object uri)
{
/* now we have the same issue as always except worse. webkit resists an MVC approach!
   for now, the 1st webkit view will be manipulated only
 */
  
//webkit_web_view_load_uri(xv->widget, "http://www.fsf.org");
}
#endif        



DEFUN ("xwidget-embed-steal-window", Fxwidget_embed_steal_window, Sxwidget_embed_steal_window, 2, 2, 0,
       doc:	/* Tell existing embed xwidget to steal other window id. This is based on a deprecated method in GTK and doesnt work too well.*/
       )
  (Lisp_Object xwidget_id, Lisp_Object window_id)
{
  struct xwidget *xw;
  int xid, iwindow_id;

  CHECK_NUMBER (xwidget_id);
  CHECK_NUMBER (window_id);
  xid = XFASTINT (xwidget_id);
  iwindow_id = XFASTINT (window_id);
  xw = &xwidgets[xid];
  printf ("  gtk_socket_add_id: %d %d\n", xid, iwindow_id);
  //  gtk_socket_steal(GTK_SOCKET(xw->widget),iwindow_id);
  //try adding proper gtk plugs instead, i never once had "steal" work
  /////////  gtk_socket_add_id (GTK_SOCKET (xw->widget), iwindow_id); /////TODO MVC
  //add_id annoyingly odesnt work either. the only working option
  //seems to be clients that plug into the sockets, and so far only emacs and mplayer
  //oenvrml
  return Qnil;
}



DEFUN ("xwidget-resize-internal", Fxwidget_resize_internal, Sxwidget_resize_internal, 3, 3, 0, doc:
       /* resize xwidgets internal use only, because the lisp specs need to be updated also*/)
  (Lisp_Object xwidget_id, Lisp_Object new_width, Lisp_Object new_height)
{
  struct xwidget *xw;
  struct xwidget_view *xv;
  int xid, w, h;

  CHECK_NUMBER (xwidget_id);
  CHECK_NUMBER (new_width);
  CHECK_NUMBER (new_height);
  xid = XFASTINT (xwidget_id);
  w = XFASTINT (new_width);
  h = XFASTINT (new_height);
  xw = &xwidgets[xid];

  printf("resize xwidget %d (%d,%d)->(%d,%d)",xid,xw->width,xw->height,w,h);
  xw->width=w;
  xw->height=h;
  for (int i = 0; i < MAX_XWIDGETS; i++) //TODO MVC refactor lazy linear search
    {
      xv = &xwidget_views[i];
      if(xv->model == xw){
        gtk_layout_set_size (GTK_LAYOUT (xv->widgetwindow), xw->width, xw->height);
        gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xw->width, xw->height);
      }
    }

  return Qnil;
}



DEFUN("xwidget-info", Fxwidget_info , Sxwidget_info, 1,1,0, doc: /* get xwidget props */)
     (Lisp_Object xwidget_id)
{
  struct xwidget *xw = &xwidgets[XFASTINT (xwidget_id)];
  Lisp_Object info;

  info = Fmake_vector (make_number (7), Qnil);
  XVECTOR (info)->contents[0] = make_number(xw->id);
  XVECTOR (info)->contents[1] = make_number(xw->type);
  XVECTOR (info)->contents[2] = Qnil; //make_number(xw->x);
  XVECTOR (info)->contents[3] = Qnil;//make_number(xw->y);
  XVECTOR (info)->contents[4] = make_number(xw->width);
  XVECTOR (info)->contents[5] = make_number(xw->height);
  XVECTOR (info)->contents[6] = Qnil;//make_number(xw->hidden);

  return info;
}

//xterm.c listens to xwidget_owns_kbd  and tries to not eat events when its set
int xwidget_owns_kbd = 0;
DEFUN ("xwidget-set-keyboard-grab", Fxwidget_set_keyboard_grab, Sxwidget_set_keyboard_grab, 2, 2, 0, doc:	/* set unset kbd grab for xwidget. */
       )
  (Lisp_Object xwidget_id, Lisp_Object kbd_grab)
{
  struct xwidget *xw;
  int xid, kbd_flag;

  CHECK_NUMBER (xwidget_id);
  CHECK_NUMBER (kbd_grab);
  xid = XFASTINT (xwidget_id);
  kbd_flag = XFASTINT (kbd_grab);
  xw = &xwidgets[xid];
  if(xw->type != 3) return Qnil; //only relevant for xembed  //TODO MVC
  
  printf ("kbd grab: %d %d\n", xid, kbd_flag);
  if (kbd_flag)
    {
      //int rv=gtk_widget_activate(xw->widget); //ok, but how deactivate?
      //printf("activation:%d\n",rv);
      //      gtk_window_present(GTK_WINDOW(xw->widget));
      //gtk_widget_grab_focus(xw->widget);
      //      gtk_socket_windowing_update_active (xw->widget,1);
      //      GDK_WINDOW_XWINDOW (GTK_WIDGET (socket)->window)
      //FRAME_X_OUTPUT (f)->widget
      //      gdk_keyboard_grab(xw->widget,TRUE,GDK_CURRENT_TIME);

      /* GtkWidget *parent = gtk_widget_get_parent (xw->widget); */
      /* GtkWidget *lastparent; */
      /* for (lastparent = parent; parent = gtk_widget_get_parent (parent); */
      /*      parent == NULL); */

      /* gtk_container_set_focus_child (GTK_CONTAINER (lastparent), xw->widget); */

      ////gtk_container_set_focus_child (GTK_CONTAINER (xw->widgetwindow), xw->widget); //TODO MVC

      xwidget_owns_kbd = TRUE;
    }
  else
    {
      xwidget_owns_kbd = FALSE;
    }
  /*
    gdk_keyboard_grab(xw->widget,TRUE,GDK_CURRENT_TIME);
    else
    gdk_keyboard_ungrab(GDK_CURRENT_TIME);
  */
  return Qnil;
}


//lowlevel fn mostly cloned from xembed_send_message()
void
xwidget_key_send_message (struct frame *f,
                          Window destination_window,
                          int keycode, int keypress, int modifiers)
{

  XKeyEvent event;
  //segfaults:
  /* xwidget_key_send_message (f=0x0, destination_window=0, keycode=65, keypress=1,  */
  /*     modifiers=0) at xwidget.c:332 */
  /* 332          event.display = FRAME_X_DISPLAY (f); */

  event.display = FRAME_X_DISPLAY (f);
  event.window = destination_window;
  event.root = FRAME_X_WINDOW (f);
  event.subwindow = None;
  event.time = CurrentTime;
  event.x = 1;
  event.y = 1;
  event.x_root = 1;
  event.y_root = 1;
  event.same_screen = TRUE;

  event.type = keypress ? KeyPress : KeyRelease;
  event.keycode = keycode;
  event.state = modifiers;

  XSendEvent (event.display, event.window, TRUE, KeyPressMask,
              (XEvent *) & event);
}

//using "accessible" interfaces seems expensive
//pkg-config --cflags cspi-1.0
//#include <at-spi-1.0/cspi/spi.h>

DEFUN ("xwidget-send-keyboard-event", Fxwidget_send_keyboard_event, Sxwidget_send_keyboard_event, 2, 2, 0, doc:/* synthesize a kbd event for a xwidget. */
       )
  (Lisp_Object  xwidget_id, Lisp_Object keydescriptor)
{
  int keyval;
  char *keystring = "";
  FRAME_PTR f;
  struct xwidget *xw;
  GdkWindow *window;
  int xwid;
  XID xid;

  CHECK_NUMBER (xwidget_id);
  xwid = XFASTINT (xwidget_id);
  xw = &xwidgets[xwid];

  /* TODO MVC
  f = (FRAME_PTR) g_object_get_data (G_OBJECT (xw->widget), XG_FRAME_DATA);

  //GdkWindow* window=gtk_widget_get_window(xw->widget); //event winds up in emacs

  //TODO assert xw is a gtk_socket or THIS WILL FAIL GLORIOUSLY
  window = gtk_socket_get_plug_window (GTK_SOCKET (xw->widget));
  //the event gets eaten somewhere.
  //i suspect you just cant send an event to a child window and not have emacs eat it.
  //but if this were true the event should pop to emacs right?


  xid = gdk_x11_drawable_get_xid (window);

  printf ("xwidget-send-keyboard-event %d %d\n", window, xid);

  xwidget_key_send_message (f, xid, 38, 1, 0);	//38 is 'a' HACK for now
  xwidget_key_send_message (f, xid, 38, 0, 0);
  */
  return Qnil;
}

void
syms_of_xwidget (void)
{
  int i;

  defsubr (&Sxwidget_set_keyboard_grab);
  defsubr (&Sxwidget_send_keyboard_event);
  defsubr (&Sxwidget_embed_steal_window);
  defsubr (&Sxwidget_info);
  defsubr (&Sxwidget_resize_internal);
  defsubr (&Sxwidget_embed_steal_window);

  DEFSYM (Qxwidget ,"xwidget");

  DEFSYM (Qxwidget_id ,":xwidget-id");
  DEFSYM (Qtitle ,":title");

  DEFSYM (Qbutton, "button");  
  DEFSYM (Qtoggle, "toggle");  
  DEFSYM (Qslider, "slider");  
  DEFSYM (Qsocket, "socket");
  DEFSYM (Qcairo, "cairo");
  DEFSYM (Qwebkit ,"webkit");
  DEFSYM (Qwebkit_osr ,"webkit-osr");    
  DEFSYM (QCplist, ":plist");  

  Fprovide (intern ("xwidget-internal"), Qnil);

  for (i = 0; i < MAX_XWIDGETS; i++)
    xwidgets[i].initialized = 0;
}


/* Value is non-zero if OBJECT is a valid Lisp xwidget specification.  A
   valid xwidget specification is a list whose car is the symbol
   `xwidget', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported xwidget type.  The rest of the property list depends on the
   xwidget type.  */

int
valid_xwidget_p (Lisp_Object object)
{
  int valid_p = 0;

  if (XWIDGETP (object))
    {
      /* Lisp_Object tem; */

      /* for (tem = XCDR (object); CONSP (tem); tem = XCDR (tem)) */
      /*   if (EQ (XCAR (tem), QCtype)) */
      /*     { */
      /*       tem = XCDR (tem); */
      /*       if (CONSP (tem) && SYMBOLP (XCAR (tem))) */
      /*         { */
      /*        struct xwidget_type *type; */
      /*        type = lookup_xwidget_type (XCAR (tem)); */
      /*        if (type) */
      /*          valid_p = type->valid_p (object); */
      /*         } */

      /*       break; */
      /*     } */
      //never mind type support for now
      valid_p = 1;
    }

  return valid_p;
}




/* find a value associated with key in spec */
Lisp_Object
xwidget_spec_value (
                    Lisp_Object spec, Lisp_Object  key,
                    int *found)
{
  Lisp_Object tail;

  xassert (valid_xwidget_p (spec));

  for (tail = XCDR (spec);
       CONSP (tail) && CONSP (XCDR (tail)); tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), key))
        {
          if (found)
            *found = 1;
          return XCAR (XCDR (tail));
        }
    }

  if (found)
    *found = 0;
  return Qnil;
}

void
assert_valid_xwidget_id (int id, char *str)
{
  if (id < 0 || id > MAX_XWIDGETS)
    {
      printf ("broken xwidgetid:%d %s\n", id, str);
      abort ();
    }
}

struct xwidget *
xwidget_from_id (int id)
{
  assert_valid_xwidget_id (id, "xwidget_from_id");
  return &xwidgets[id];
}

struct xwidget_view* xwidget_view_lookup(struct xwidget* xw,     struct window *w){
  struct xwidget_view* xv = NULL;
  for (int i = 0; i < MAX_XWIDGETS; i++)
    if ((xwidget_views[i].model == xw) && (xwidget_views[i].w == w))
      xv =  &xwidget_views[i];
  
  return xv;
}

//attempting a workaround for a webkit offscreen bug
void                gtk_window_get_position             (GtkWindow *window,
                                                         gint *root_x,
                                                         gint *root_y){
  printf("my getsize\n");
  *root_x = 0;
  *root_y = 0;
}

  

int
lookup_xwidget (Lisp_Object  spec)
{
  /*when a xwidget lisp spec is found initialize the C struct that is used in the C code.
    xwidget_init
   */
  int found = 0, found1 = 0, found2 = 0;
  Lisp_Object value;
  int id;
  struct xwidget *xw;

  value = xwidget_spec_value (spec, Qxwidget_id, &found1);
  id = INTEGERP (value) ? XFASTINT (value) : 0;	//id 0 by default, but id must be unique so this is dumb

  xw = &xwidgets[id];
  xw->id=id;
  value = xwidget_spec_value (spec, QCtype, &found);
  xw->type = SYMBOLP (value) ? value : Qbutton;	//default to button
  value = xwidget_spec_value (spec, Qtitle, &found2);
  xw->title = STRINGP (value) ? (char *) SDATA (value) : "?";	//funky cast FIXME

  value = xwidget_spec_value (spec, QCheight, NULL);
  xw->height = INTEGERP (value) ? XFASTINT (value) : 50;	//ok
  value = xwidget_spec_value (spec, QCwidth, NULL);
  xw->width = INTEGERP (value) ? XFASTINT (value) : 50;	//ok

  value = xwidget_spec_value (spec, QCplist, Qnil);
  xw->plist = value;
  printf ("xwidget_id:%d type:%d found:%d %d %d title:%s (%d,%d)\n", id,
          xw->type, found, found1, found2, xw->title, xw->height, xw->width);


  assert_valid_xwidget_id (id, "lookup_xwidget");

#ifdef HAVE_WEBKIT_OSR
  //diy mvc. widget is rendered offscreen, later blitted onscreen
  if (EQ(xw->type, Qwebkit_osr)){
    xw->widgetwindow_osr = GTK_CONTAINER (gtk_offscreen_window_new ());
    gtk_window_resize(    GTK_WINDOW(xw->widgetwindow_osr), xw->width, xw->height);
    xw->widget_osr = webkit_web_view_new();

    //random debug hack
    gtk_widget_set_double_buffered (xw->widget_osr,FALSE);
    gtk_widget_set_double_buffered (xw->widgetwindow_osr,FALSE);  

    
    //xw->widget_osr ///XXX
    gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width, xw->height);      
    gtk_container_add (xw->widgetwindow_osr, xw->widget_osr);
    gtk_widget_show_all (GTK_WIDGET (xw->widgetwindow_osr));

    //store some xwidget data in the gtk widgets
    g_object_set_data (G_OBJECT (xw->widget_osr), XG_XWIDGET, (gpointer) (xw)); //the xwidget
    g_object_set_data (G_OBJECT (xw->widgetwindow_osr), XG_XWIDGET, (gpointer) (xw)); //the xwidget  
    
    g_signal_connect (G_OBJECT (    xw->widgetwindow_osr), "damage_event",                    
    G_CALLBACK (webkit_osr_damage_event_callback), NULL);                  


      
    webkit_web_view_load_uri(xw->widget_osr, "http://www.fsf.org");

  }
#endif          
  return id;
}





//////////////////////////////////
int region_modified = 0;

/*set up detection of touched xwidget*/
void
xwidget_start_redisplay (void)
{
  int i;
  for (i = 0; i < MAX_XWIDGETS; i++)
    xwidget_views[i].redisplayed = 0;

}

/* the xwidget was touched during redisplay, so it isnt a candidate for hiding*/
void
xwidget_touch (struct xwidget_view *xw)
{
  //printf("touch xwidget %d\n", xw->id);
  xw->redisplayed = 1;
}

int
xwidget_touched (struct xwidget_view *xw)
{
  return  xw->redisplayed;
}

/* redisplay has ended, now we should hide untouched xwidgets

   atm this works as follows: only check if xwidgets are displayed in the
   "selected window". if not, hide them or phantom them.

   this means valid cases like xwidgets being displayed only once in
   non-selected windows, does not work well. they should also be visible
   in that case not phantomed.

*/
void
xwidget_end_redisplay (struct glyph_matrix *matrix)
{
  
  int i;
  struct xwidget *xw;
  int area;

  region_modified = 0;
  xwidget_start_redisplay ();
  //iterate desired glyph matrix of "live" window here, hide gtk widgets
  //not in the desired matrix.

  //the current scheme will fail on the case of several buffers showing xwidgets

  //  dump_glyph_matrix(matrix, 2);
  for (i = 0; i < matrix->nrows; ++i)
    {
      //    dump_glyph_row (MATRIX_ROW (matrix, i), i, glyphs);
      struct glyph_row *row;
      row = MATRIX_ROW (matrix, i);
      if (row->enabled_p != 0)
        {
          for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
            {
              struct glyph *glyph = row->glyphs[area];
              struct glyph *glyph_end = glyph + row->used[area];
              for (; glyph < glyph_end; ++glyph)
                {
                  if (glyph->type == XWIDGET_GLYPH)
                    {
                      //printf("(%d)",glyph->u.xwidget_id);
                      //here the id sometimes sucks, so maybe the desired glyph matrix isnt ready here?
                      //also, it appears the desired matrix is not the entire window, but only the changed part. wtf?
                      int id = glyph->u.xwidget_id;
                      if (id < 0 || id > MAX_XWIDGETS)
                        {
                          printf
                            ("glyph matrix contains crap, abort xwidget handling and wait for better times\n ");
                          //dump_glyph_matrix(matrix, 2);
                          return;
                        }
                      else
                        {
                          // printf("row %d not enabled\n", i);
                        }
                        /*
                          the only call to xwidget_end_redisplay is in dispnew and looks like:
                          if ((XWINDOW(FRAME_SELECTED_WINDOW (SELECTED_FRAME()))) ==  (w))
                          xwidget_end_redisplay(w->current_matrix);
                        */
                      xwidget_touch (xwidget_view_lookup(&xwidgets[glyph->u.xwidget_id],
                                                         (XWINDOW(FRAME_SELECTED_WINDOW (SELECTED_FRAME())))));
                    }
                }
            }
        }
    }

  for (i = 0; i < MAX_XWIDGETS; i++)
    {
      struct xwidget_view* xv = &xwidget_views[i];

      //"touched" is only meaningful for the "live" window, so disregard other views
      if (xv->initialized && ( xv->w ==    (XWINDOW(FRAME_SELECTED_WINDOW (SELECTED_FRAME())))))
        {
          if (xwidget_touched(xv))
            xwidget_show_view (xv);
          else
            xwidget_hide_view (xv);
        }
    }
}

/* some type of modification was made to the buffers(unused)*/
void
xwidget_modify_region (void)
{
  region_modified = 1;
}


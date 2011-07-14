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

#include <wchar.h>

#ifdef HAVE_WEBKIT_OSR
#include <webkit/webkitwebview.h>
#endif

#include "xwidget.h"

//TODO should of course not be a hardcoded array but I can't be bothered atm
//just a fixed array of xwidgets for now
//would need to be hashtables or something

#define MAX_XWIDGETS 100
struct xwidget_view xwidget_views[MAX_XWIDGETS]; 

//TODO embryo of lisp allocators for xwidgets
//TODO xwidget* should be Lisp_xwidget*
struct xwidget*
allocate_xwidget (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget, height, PVEC_OTHER);
}

//TODO xwidget_view* should be Lisp_xwidget_view*
struct xwidget_view*
allocate_xwidget_view (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget_view, redisplayed, PVEC_OTHER);
}


Lisp_Object Qxwidget;
Lisp_Object Qcxwidget;
Lisp_Object Qtitle;
Lisp_Object Qxwidget_set_keyboard_grab;
Lisp_Object Qxwidget_embed_steal_window;
Lisp_Object Qxwidget_info;
Lisp_Object Qxwidget_resize_internal;
Lisp_Object Qxwidget_send_keyboard_event;

Lisp_Object Qbutton, Qtoggle, Qslider, Qsocket, Qcairo, 
  Qwebkit_osr, QCplist;


extern Lisp_Object  QCtype;   
extern Lisp_Object QCwidth, QCheight;  

struct xwidget_view* xwidget_view_lookup(struct xwidget* xw,     struct window *w);
Lisp_Object xwidget_spec_value ( Lisp_Object spec, Lisp_Object  key,  int *found);
gboolean webkit_osr_damage_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data) ;

DEFUN ("make-xwidget", Fmake_xwidget, Smake_xwidget, 7, 7, 0,
         doc: /* xw */
         )
  (Lisp_Object beg, Lisp_Object end,
     Lisp_Object type,
     Lisp_Object title,     
     Lisp_Object width, Lisp_Object height,
     Lisp_Object data)
{
  //should work a bit like "make-button"(make-button BEG END &rest PROPERTIES)
  // arg "type" and fwd should be keyword args eventually
  //(make-xwidget 3 3 'button "oei" 31 31 nil)
  //(xwidget-info (car xwidget-alist))
  struct xwidget* xw = allocate_xwidget();
  Lisp_Object val;
  struct gcpro gcpro1;
  GCPRO1(xw);
  XSETSYMBOL(xw->type, type);
  XSETSTRING(xw->title, title);  
  XSETBUFFER(xw->buffer,  Fcurrent_buffer()); // conservatively gcpro xw since we call lisp
  xw->height = XFASTINT(height);
  xw->width = XFASTINT(width);
  XSETPSEUDOVECTOR (val, xw, PVEC_OTHER); //?? dunno why i need this
  Vxwidget_alist = Fcons ( val, Vxwidget_alist);



#ifdef HAVE_WEBKIT_OSR
  /* DIY mvc. widget is rendered offscreen,
     later bitmap copied to the views.
   */
  if (EQ(xw->type, Qwebkit_osr) && !xw->widgetwindow_osr){
    BLOCK_INPUT;
    xw->widgetwindow_osr = GTK_CONTAINER (gtk_offscreen_window_new ());
    gtk_window_resize(    GTK_WINDOW(xw->widgetwindow_osr), xw->width, xw->height);
    xw->widget_osr = webkit_web_view_new();

    gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width, xw->height);      
    gtk_container_add (xw->widgetwindow_osr, xw->widget_osr);
    
    gtk_widget_show_all (GTK_WIDGET (xw->widgetwindow_osr));

    /* store some xwidget data in the gtk widgets for convenient retrieval in the event handlers. */
    g_object_set_data (G_OBJECT (xw->widget_osr), XG_XWIDGET, (gpointer) (xw));
    g_object_set_data (G_OBJECT (xw->widgetwindow_osr), XG_XWIDGET, (gpointer) (xw));
    g_signal_connect (G_OBJECT (    xw->widgetwindow_osr), "damage-event",    G_CALLBACK (webkit_osr_damage_event_callback), NULL);
      
    webkit_web_view_load_uri(WEBKIT_WEB_VIEW(xw->widget_osr), "http://www.fsf.org");
    UNBLOCK_INPUT;

  }
#endif          


  UNGCPRO;
  return val;
}

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
  printf ("button clicked xw:%d '%s'\n", xw, xw->title);

  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;

  XSETFRAME (frame, f);

  event.frame_or_window = Qnil;	//frame; //how to get the frame here?


  event.arg = Qnil;
  event.arg = Fcons (xw, event.arg); //TODO send the actual xwidget object now instead
  event.arg = Fcons (intern ("buttonclick"), event.arg);

  kbd_buffer_store_event (&event);


}


static void
send_xembed_ready_event (struct xwidget* xw, int xembedid)
{
  struct input_event event;
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;	//frame; //how to get the frame here? //TODO i store it in the xwidget now

  event.arg = Qnil;
  event.arg = Fcons (make_number (xembedid), event.arg);
  event.arg = Fcons (intern ("xembed-ready"), event.arg);
  event.arg = Fcons (xw, event.arg); //TODO 


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
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), GTK_WIDGET (xv->widgetwindow),  xv->x  + xv->clip_left, xv->y + xv->clip_top); //TODO refactor 
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
  double v=gtk_range_get_value(range);
  struct xwidget_view* xvp = g_object_get_data (G_OBJECT (range), XG_XWIDGET_VIEW);
  struct xwidget_view* xv;

  printf("slider changed val:%f\n", v);

  
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
        gtk_range_set_value(GTK_RANGE(xv->widget), v);
        g_signal_handler_unblock( xv->widget,xv->handler_id);
      }
    }

}


/* when the off-screen webkit master view changes this signal is called.
   it copies the bitmap from the off-screen webkit instance */
gboolean webkit_osr_damage_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data) 
{
  //TODO this is wrong! should just oueu a redraw of onscreen widget
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  struct xwidget_view* xv;
  //webkit_osr_redraw_child(xw, widget);
  printf ("damage\n");
  for (int i = 0; i < MAX_XWIDGETS; i++)//todo mvc refactor
    {
      xv = &xwidget_views[i];
      if(xv->model == xw){
        gtk_widget_queue_draw (xv->widget); //redraw all views, the master has changed
      }
    }

  return FALSE;
}                                                                               


//for gtk3 webkit_osr
gboolean
xwidget_osr_draw_callback (GtkWidget *widget, cairo_t *cr, gpointer data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  struct xwidget_view* xv = (struct xwidget_view*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET_VIEW);    
  
  printf("xwidget_osr_draw_callback gtk3 xw.id:%d xw.type:%d window:%d vis:%d\n",
         xw,xw->type, gtk_widget_get_window (widget),  gtk_widget_get_visible (xw->widget_osr));

  cairo_rectangle(cr, 0,0, xv->clip_right, xv->clip_bottom);//xw->width, xw->height);
  cairo_clip(cr);

  gtk_widget_draw (xw->widget_osr,  cr);

  
  return FALSE;
}


gboolean
xwidget_osr_button_callback ( GtkWidget *widget,
                              GdkEvent  *event,
                              gpointer   user_data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);    
  GdkEvent* eventcopy =  gdk_event_copy(event);
  
  ((GdkEventButton*)eventcopy)->window = gtk_widget_get_window(xw->widget_osr);
  gtk_main_do_event(eventcopy); //TODO this will leak events. they should be deallocated later
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
      xv->widget = gtk_button_new_with_label (XSTRING(xww->title)->data);
      g_signal_connect (G_OBJECT (xv->widget), "clicked",
                        G_CALLBACK (buttonclick_handler), xww); //the model rather than the view
    } else if (EQ(xww->type, Qtoggle)) {
    xv->widget = gtk_toggle_button_new_with_label (XSTRING(xww->title)->data);
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
  } else if (EQ(xww->type, Qwebkit_osr)) {
#ifdef HAVE_WEBKIT_OSR
    xv->widget = gtk_drawing_area_new();
    gtk_widget_set_app_paintable ( xv->widget, TRUE); //because expose event handling
    gtk_widget_add_events(xv->widget,
                          GDK_BUTTON_PRESS_MASK
                          | GDK_BUTTON_RELEASE_MASK
                          | GDK_POINTER_MOTION_MASK);
    g_signal_connect (G_OBJECT (    xv->widget), "draw",                    
                      G_CALLBACK (xwidget_osr_draw_callback), NULL);
    g_signal_connect (G_OBJECT (    xv->widget), "button-press-event",                    
                      G_CALLBACK (xwidget_osr_button_callback), NULL);
    g_signal_connect (G_OBJECT (    xv->widget), "button-release-event",                    
                      G_CALLBACK (xwidget_osr_button_callback), NULL);
    g_signal_connect (G_OBJECT (    xv->widget), "motion-notify-event",                    
                      G_CALLBACK (xwidget_osr_button_callback), NULL);
#endif


  } else return NULL;
  
  //widget realization
  //make container widget 1st, and put the actual widget inside the container
  //later, drawing should crop container window if necessary to handle case where xwidget
  //is partially obscured by other emacs windows
  xv->emacswindow = GTK_CONTAINER (s->f->gwfixed);
  //xw->widgetwindow = GTK_CONTAINER (gtk_layout_new (NULL, NULL));
  //xw->widgetwindow = GTK_CONTAINER (gtk_offscreen_window_new ());

  xv->widgetwindow = GTK_CONTAINER (gtk_fixed_new ()); //works well for clipping on gtk2 not gtk3
  //xv->widgetwindow = GTK_CONTAINER (gtk_event_box_new ()); //doesnt help clipping gtk3
  //xv->widgetwindow = GTK_CONTAINER (gtk_scrolled_window_new (NULL, NULL)); //clips in gtk3
  //xv->widgetwindow = GTK_CONTAINER (gtk_viewport_new (NULL, NULL));
  

  /* GtkAllocation a; */
  /* a.x=0;  a.y=0;   a.width=xww->width;  a.height=xww->height; */
  /* gtk_widget_set_allocation (GTK_WIDGET (xv->widget), &a); */

  gtk_widget_set_has_window(GTK_WIDGET (  xv->widgetwindow), TRUE);
  //on GTK2 if gtk_fixed doesnt have a window it will surprisingly not honor
  //setsize so that children gets clipped later. the documentation is
  //not consistent regarding if its legal to call this method.

  //on GTK3 the call isnt necessary except for windowless widgets such as the drawarea used for the webkit views


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
  gtk_widget_set_size_request (GTK_WIDGET (xv->widgetwindow), xww->width, xww->height);
  gtk_fixed_put (GTK_FIXED (s->f->gwfixed), GTK_WIDGET (xv->widgetwindow), x, y);
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
            xww,
            gtk_socket_get_id (GTK_SOCKET (xv->widget)),
            gtk_socket_get_id (GTK_SOCKET (xv->widget)));
    send_xembed_ready_event (xww,
                             gtk_socket_get_id (GTK_SOCKET (xv->widget)));
    //gtk_widget_realize(xw->widget);
  }
  return xv;
}


void
x_draw_xwidget_glyph_string (struct glyph_string *s)
{
  /*
    this method is called by the redisplay engine and places the xwidget on screen.
    moving and clipping is done here. also view init.

  */
  int box_line_hwidth = eabs (s->face->box_line_width);
  int box_line_vwidth = max (s->face->box_line_width, 0);
  int height = s->height;
  struct xwidget *xww = s->xwidget;
  struct xwidget_view *xv = xwidget_view_lookup(xww, (s->w));
  int clip_right; int clip_bottom; int clip_top; int clip_left;

  int x = s->x;
  int y = s->y + (s->height / 2) - (xww->height / 2);
  int moved=0;

  if (xv == NULL){
    /* Views must be initialized once(only once).
       We do it here in the display loop because there is no other time to know things like
       window placement etc.
    */
    printf ("xv init for xw %d\n", xww);
    xv = xwidget_init_view (xww, s, x, y); 
  }

  //calculate clipping, which is used for all manner of onscreen xwidget views
  //each widget border can get clipped by other emacs objects so there are four clipping variables
  clip_right = min (xww->width, WINDOW_RIGHT_EDGE_X (s->w) - x - WINDOW_RIGHT_SCROLL_BAR_AREA_WIDTH(s->w) - WINDOW_RIGHT_FRINGE_WIDTH(s->w));
  clip_left = max (0, WINDOW_LEFT_EDGE_X (s->w) - x + WINDOW_LEFT_SCROLL_BAR_AREA_WIDTH(s->w) + WINDOW_LEFT_FRINGE_WIDTH(s->w));
  
  clip_bottom = min (xww->height, WINDOW_BOTTOM_EDGE_Y (s->w) - WINDOW_MODE_LINE_HEIGHT (s->w) - y);
  clip_top = max(0, WINDOW_TOP_EDGE_Y(s->w) -y ); 

  //we are conserned with movement of the onscreen area. the area might sit still when the widget actually moves
  //this happens when an emacs window border moves across a widget winow
  moved = (xv->x  + xv->clip_left != x+clip_left)
    || ((xv->y + xv->clip_top)!= (y+clip_top));
  //if(moved)    printf ("live xwidget moved: id:%d (%d,%d)->(%d,%d) y+clip_top:%d\n", xww->id, xv->x, xv->y, x, y, y + clip_top);
  xv->x = x;
  xv->y = y;
  if (moved)	//has it moved?
    {
      if (!xwidget_hidden(xv))	//hidden equals not being seen during redisplay
        {
          //TODO should be possible to use xwidget_show_view here
          gtk_fixed_move (GTK_FIXED (s->f->gwfixed),
                          GTK_WIDGET (xv->widgetwindow),
                          x + clip_left, y + clip_top);
        }
    }
  //clip the widget window if some parts happen to be outside drawable area
  //an emacs window is not a gtk window, a gtk window covers the entire frame
  //cliping might have changed even if we havent actualy moved, we try figure out when we need to reclip for real
  if((xv->clip_right != clip_right)
     || (xv->clip_bottom != clip_bottom)
     || (xv->clip_top != clip_top)
     || (xv->clip_left != clip_left)){
    gtk_widget_set_size_request (GTK_WIDGET (xv->widgetwindow),  clip_right + clip_left, clip_bottom + clip_top);
    gtk_fixed_move(GTK_FIXED(xv->widgetwindow), xv->widget, -clip_left, -clip_top);
    printf("reclip %d %d -> %d %d  clip_top:%d clip_left:%d\n",xv->clip_right, xv->clip_bottom,  clip_right, clip_bottom, clip_top , clip_left);

        
    xv->clip_right = clip_right; xv->clip_bottom = clip_bottom; xv->clip_top = clip_top;xv->clip_left = clip_left;
  }
  //if emacs wants to repaint the area where the widget lives, queue a redraw
  if (!xwidget_hidden(xv)){
    gtk_widget_queue_draw (GTK_WIDGET(xv->widgetwindow));
    gtk_widget_queue_draw (xv->widget);
  }
}


#ifdef HAVE_WEBKIT_OSR
DEFUN ("xwidget-webkit-goto-uri", Fxwidget_webkit_goto_uri,  Sxwidget_webkit_goto_uri, 2, 2, 0,
       doc:	/* webkit goto uri.*/
       )
  (Lisp_Object xwidget, Lisp_Object uri)
{
  struct xwidget* xw = XXWIDGET(xwidget);
  webkit_web_view_load_uri ( WEBKIT_WEB_VIEW(xw->widget_osr), SDATA(uri));
  return Qnil;
}

DEFUN ("xwidget-webkit-execute-script", Fxwidget_webkit_execute_script,  Sxwidget_webkit_execute_script, 2, 2, 0,
       doc:	/* webkit exec js.*/
       )
  (Lisp_Object xwidget, Lisp_Object script)
{
  struct xwidget* xw = XXWIDGET(xwidget);
  webkit_web_view_execute_script( WEBKIT_WEB_VIEW(xw->widget_osr), SDATA(script));
  return Qnil;
}

DEFUN ("xwidget-webkit-get-title", Fxwidget_webkit_get_title,  Sxwidget_webkit_get_title, 1, 1, 0,
       doc:	/* webkit get title. can be used to work around exec method lacks return val*/
       )
  (Lisp_Object xwidget)
{
  //TODO support multibyte strings
  struct xwidget* xw = XXWIDGET(xwidget);
  const gchar* str=webkit_web_view_get_title( WEBKIT_WEB_VIEW(xw->widget_osr));
  return make_string_from_bytes(str, wcslen((const wchar_t *)str), strlen(str));
}



#endif        





DEFUN ("xwidget-resize-internal", Fxwidget_resize_internal, Sxwidget_resize_internal, 3, 3, 0, doc:
       /* resize xwidgets internal use only, because the lisp specs need to be updated also*/)
  (Lisp_Object xwidget, Lisp_Object new_width, Lisp_Object new_height)
{
  struct xwidget* xw = XXWIDGET(xwidget);
  struct xwidget_view *xv;
  int  w, h;

  CHECK_NUMBER (new_width);
  CHECK_NUMBER (new_height);
  w = XFASTINT (new_width);
  h = XFASTINT (new_height);
  

  printf("resize xwidget %d (%d,%d)->(%d,%d)",xw, xw->width,xw->height,w,h);
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
  (Lisp_Object xwidget)
{
  Lisp_Object info;
  struct xwidget* xw = XXWIDGET(xwidget);

  info = Fmake_vector (make_number (4), Qnil);
  XSETSYMBOL (XVECTOR (info)->contents[0], xw->type);
  XSETSTRING (XVECTOR (info)->contents[1], xw->title);
  XSETINT    (XVECTOR (info)->contents[2], xw->width);
  XSETINT    (XVECTOR (info)->contents[3], xw->height);


  return info;
}

 
DEFUN("xwidget-view-info", Fxwidget_view_info , Sxwidget_view_info, 2,2,0, doc: /* get xwidget view props */)
  (Lisp_Object xwidget, Lisp_Object window)
{
  struct xwidget* xw = XXWIDGET(xwidget);
  struct xwidget_view* xv = xwidget_view_lookup(xw, XWINDOW(window));
  
  Lisp_Object info;

  info = Fmake_vector (make_number (6), Qnil);
  XVECTOR (info)->contents[0] = make_number(xv->x);
  XVECTOR (info)->contents[1] = make_number(xv->y);
  XVECTOR (info)->contents[2] = make_number(xv->clip_right);
  XVECTOR (info)->contents[3] = make_number(xv->clip_bottom);
  XVECTOR (info)->contents[4] = make_number(xv->clip_top);
  XVECTOR (info)->contents[5] = make_number(xv->clip_left);

  return info;
}

void
syms_of_xwidget (void)
{
  int i;

  defsubr (&Smake_xwidget);

  defsubr (&Sxwidget_info);
  defsubr (&Sxwidget_view_info);
  defsubr (&Sxwidget_resize_internal);


  defsubr (&Sxwidget_webkit_goto_uri);
  defsubr (&Sxwidget_webkit_execute_script);
  defsubr (&Sxwidget_webkit_get_title);
  
  DEFSYM (Qxwidget ,"xwidget");

  DEFSYM (Qcxwidget ,":xwidget");
  DEFSYM (Qtitle ,":title");

  DEFSYM (Qbutton, "button");  
  DEFSYM (Qtoggle, "toggle");  
  DEFSYM (Qslider, "slider");  
  DEFSYM (Qsocket, "socket");
  DEFSYM (Qcairo, "cairo");
  DEFSYM (Qwebkit_osr ,"webkit-osr");    
  DEFSYM (QCplist, ":plist");  

   DEFVAR_LISP ("xwidget-alist", Vxwidget_alist, doc: /*xwidgets list*/);
   Vxwidget_alist = Qnil;
   DEFVAR_LISP ("xwidget-view-alist", Vxwidget_view_alist, doc: /*xwidget views list*/);
   Vxwidget_alist = Qnil;
 
  Fprovide (intern ("xwidget-internal"), Qnil);

  //  for (i = 0; i < MAX_XWIDGETS; i++)
  //xwidgets[i].initialized = 0;
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
xwidget_spec_value ( Lisp_Object spec, Lisp_Object  key,
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


void      xwidget_view_delete_all_in_window(  struct window *w )
{
  struct xwidget_view* xv = NULL;
  for (int i = 0; i < MAX_XWIDGETS; i++){
      xv =  &xwidget_views[i];
      if(xv->w == w){
        gtk_widget_destroy(GTK_WIDGET(xv->widgetwindow));
      }
  }
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

  

struct xwidget*
lookup_xwidget (Lisp_Object  spec)
{
  /* When a xwidget lisp spec is found initialize the C struct that is used in the C code.
     This is done by redisplay so values change if the spec changes.
     So, take special care of one-shot events

     TODO remove xwidget init from display spec. simply store an xwidget reference only and set
     size etc when creating the xwidget, which should happen before insertion into buffer 
  */
  int found = 0, found1 = 0, found2 = 0;
  Lisp_Object value;
  struct xwidget *xw;

  value = xwidget_spec_value (spec, Qcxwidget, &found1);
  xw = XXWIDGET(value);

  /* value = xwidget_spec_value (spec, QCtype, &found); */
  /* xw->type = SYMBOLP (value) ? value : Qbutton;	//default to button */
  /* value = xwidget_spec_value (spec, Qtitle, &found2); */
  /* xw->title = STRINGP (value) ? (char *) SDATA (value) : "?";	//funky cast FIXME TODO */

  /* value = xwidget_spec_value (spec, QCheight, NULL); */
  /* xw->height = INTEGERP (value) ? XFASTINT (value) : 50;  */
  /* value = xwidget_spec_value (spec, QCwidth, NULL); */
  /* xw->width = INTEGERP (value) ? XFASTINT (value) : 50; */

  /* value = xwidget_spec_value (spec, QCplist, NULL); */
  /* xw->plist = value; */
  printf ("xwidget_id:%d type:%d found:%d %d %d title:%s (%d,%d)\n", xw,
          xw->type, found, found1, found2, xw->title, xw->height, xw->width);

  //assert_valid_xwidget_id (id, "lookup_xwidget");
  return xw;
}

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
  xw->redisplayed = 1;
}

int
xwidget_touched (struct xwidget_view *xw)
{
  return  xw->redisplayed;
}

/* redisplay has ended, now we should hide untouched xwidgets
*/
void
xwidget_end_redisplay (struct glyph_matrix *matrix)
{
  
  int i;
  struct xwidget *xw;
  int area;


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
                      /*
                        the only call to xwidget_end_redisplay is in dispnew and looks like:
                        if ((XWINDOW(FRAME_SELECTED_WINDOW (SELECTED_FRAME()))) ==  (w))
                        xwidget_end_redisplay(w->current_matrix);
                      */
                      xwidget_touch (xwidget_view_lookup(glyph->u.xwidget,
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



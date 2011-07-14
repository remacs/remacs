static int once = 0;

int
xwidget_has_composition(void){ //unused
int event_base, error_base;
Display* dpy = GDK_DISPLAY ();
int i;
  if(xwidget_query_composition_called)
    return hasNamePixmap;
  xwidget_query_composition_called = 1;

  //do this once in an emacs session

  if(gdk_display_supports_composite(gdk_display_get_default ())){
    hasNamePixmap = 1;
  }else{
    return 0;
  }
  return 1;
}





void
xwidget_end_composition(struct xwidget* w){ //unused
  //XCompositeUnredirectWindow(); stop redirecting, should be called when the socket is destroyed
}


static gboolean
xwidget_composite_draw_phantom(struct xwidget* xw,
                               int x, int y,
                               int clipx, int clipy)
{  
  FRAME_PTR f = (FRAME_PTR) g_object_get_data (G_OBJECT (xw->widget), XG_FRAME_DATA);
  ////////////////////////////////////////////////////////////////
  //Example 7. Composited windows
  GdkRegion *region;
  GtkWidget *child;
  cairo_t *cr;
  printf("xwidget_composite_draw_2 at:%d %d\n", x,y);
  /* get our child (in this case, the event box) */
  child = xw->widget; //gtk_bin_get_child (GTK_BIN (widget));
  /* create a cairo context to draw to the emacs window */
  //  cr = gdk_cairo_create (gtk_widget_get_window (f->gwfixed));//GTK_WIDGET(xw->emacswindow));//xw->widgetwindow));//widget->window);
  cr = gdk_cairo_create (gtk_widget_get_window (f->gwfixed));//GTK_WIDGET(xw->emacswindow));//));//widget->window);  
  /* the source data is the (composited) xwidget */
  //cairo_move_to(cr, xw->x, xw->y);
  
    cairo_rectangle(cr, x,y, clipx, clipy);
    cairo_clip(cr);
    
    cairo_set_source_rgb(cr,1.0,0,0);
    cairo_rectangle(cr,x,y,xw->width,xw->height);
    cairo_fill(cr);

  gdk_cairo_set_source_pixmap (cr, child->window,
                               x,//child->allocation.x,
                               y//child->allocation.y
                               );
  /* draw no more than our expose event intersects our child */
  /*  region = gdk_region_rectangle (&child->allocation);
  gdk_region_intersect (region, event->region);
  gdk_cairo_region (cr, region);
  cairo_clip (cr);                                                        */
  /* composite, with a 50% opacity */
  cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
  //cairo_paint_with_alpha (cr, phantom ? 0.5 : 0);
  cairo_paint_with_alpha (cr, 0.9);
  //cairo_paint(cr);//transparency);
  /* we're done */
  cairo_destroy (cr);
  return FALSE;
}



/*
static gboolean
xwidget_composite_draw(GtkWidget *widget,
    GdkEventExpose *event,
    gpointer data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  printf("xwidget_composite_draw %s\n", data);
  xwidget_composite_draw_2(widget,
                         event,
                         data,
                           xw->x, xw->y, 0);
  return FALSE;
}  
*/


static gboolean
xwidget_composite_draw_widgetwindow(GtkWidget *widget,
    GdkEventExpose *event,
    gpointer data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);  
  cairo_t *cr;
  GdkPixmap *pixmap;
  pixmap=xw->widget->window;
  printf("xwidget_composite_draw_widgetwindow xw.id:%d xw.type:%d window:%d\n", xw->id,xw->type, gtk_widget_get_window (widget));
  //if(xw->type!=3)//TODO this is just trial and terror to see if i can draw the live socket anywhere at all
  cr = gdk_cairo_create (gtk_widget_get_window (widget));//GTK_LAYOUT (xw->widgetwindow)->bin_window);//
  //else    cr = gdk_cairo_create (gtk_widget_get_window (xw->emacswindow));
  cairo_rectangle(cr, 0,0, xw->width, xw->height);
  cairo_clip(cr);

  cairo_set_source_rgb(cr,0,1.0,0);
  cairo_rectangle(cr, 0,0, xw->width, xw->height);
  cairo_fill(cr);
  gdk_cairo_set_source_pixmap (cr, pixmap,
                               0,0);
  
  cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
  cairo_paint_with_alpha (cr, 0.9);
  //cairo_paint(cr);
  cairo_destroy (cr);
  
  return FALSE;
}


//type support nevermind for now

/* /\* List of supported image types.  Use define_image_type to add new */
/*    types.  Use lookup_image_type to find a type for a given symbol.  *\/ */

/* static struct wxidget_type *wxidget_types; */

/* /\* Look up xwidget type SYMBOL, and return a pointer to its xwidget_type */
/*    structure.  Value is null if SYMBOL is not a known image type.  *\/ */

/* static INLINE struct xwidget_type *lookup_xwidget_type (Lisp_Object symbol) */
/* { */
/*   struct xwidget_type *type; */

/*   for (type = xwidget_types; type; type = type->next) */
/*     if (EQ (symbol, *type->type)) */
/*       break; */

/*   return type; */
/* } */


////////////////////////////////////////////////////////////////

/* delete the xwidget and its native widget peer(unused) */
void xwidget_delete(struct xwidget* xw);


void
xwidget_delete (struct xwidget *xw)
{
  printf ("xwidget %d deleted\n", xw->id);
  xw->initialized = 0;
  gtk_widget_destroy (xw->widget);

}


/* redraw all xwidgets(unused) */
void
xwidget_invalidate (void)
{
  int i;
  struct xwidget *xw;
  printf ("invalidate ");
  for (i = 0; i < MAX_XWIDGETS; i++)
    {
      xw = &xwidgets[i];
      if (xw->initialized)
        {
          printf ("%d,", i);
          gtk_widget_queue_draw_area (xw->widget, 0, 0, xw->width,
                                      xw->height);
        }
    }
  printf ("\n");
}


/* initializes the xwidget model */
void
xwidget_init_model (struct xwidget *xw,
                    struct glyph_string *s,
                    int x, int y)
{
  xw->id = s->xwidget_id;
  xw->initialized = 1;
}



DEFUN ("xwidget-replug", Fxwidget_replug, Sxwidget_replug, 2, 2, 0,
       doc:	/* unplug from socket1 plug into socket2.*/
       )
  (Lisp_Object old_parent, Lisp_Object new_parent)
{
  
  struct xwidget *xw1;
  struct xwidget *xw2;
  

  GtkWidget* widget;
  CHECK_NUMBER (old_parent);
  CHECK_NUMBER (new_parent);
  
  xw1 = &xwidgets[XFASTINT (old_parent)];
  xw2 = &xwidgets[XFASTINT (new_parent)];


  ///this wasnt thought through. we need the views rather than the model.
  //so we need to map xw+w to xv
  
  widget = xw1->widget->gtk_socket_get_plug_window ();

  //the plug...
  gtk_widget_ref(widget);//...gets an xtra ref to prevent garb, then it ...
  gtk_container_remove(GTK_CONTAINER(xw1->widget), widget);//...is uplugged from old socket and...
  gtk_container_add(GTK_CONTAINER(xw2->widget), widget);//...replugged in new socket...
  gtk_widget_unref(widget);//...and lastly remove the ref
  
  return Qnil;
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
#else
  cairo_t *cr;
  cairo_surface_t * *src_pixmap;
  src_pixmap =    gtk_offscreen_window_get_surface (xw->widgetwindow_osr);

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



DEFUN ("xwidget-embed-steal-window", Fxwidget_embed_steal_window, Sxwidget_embed_steal_window, 2, 2, 0,
       doc:	/* Tell existing embed xwidget to steal other window id. This is based on a deprecated method in GTK and doesnt work too well.*/
       )
  (Lisp_Object xwidget_id, Lisp_Object window_id)
{
  int iwindow_id;
  struct xwidget* xw = xid2xw(xwidget_id);
  
  CHECK_NUMBER (window_id);
  iwindow_id = XFASTINT (window_id);

  //  gtk_socket_steal(GTK_SOCKET(xw->widget),iwindow_id);
  //try adding proper gtk plugs instead, i never once had "steal" work
  /////////  gtk_socket_add_id (GTK_SOCKET (xw->widget), iwindow_id); /////TODO MVC
  //add_id annoyingly odesnt work either. the only working option
  //seems to be clients that plug into the sockets, and so far only emacs and mplayer
  //oenvrml
  return Qnil;
}



DEFUN("xwidget-info", Fxwidget_info , Sxwidget_info, 1,1,0, doc: /* get xwidget props */)
  (Lisp_Object xwidget_id)
{
  struct xwidget *xw = xid2xw(xwidget_id);
  Lisp_Object info;

  info = Fmake_vector (make_number (4), Qnil);
  XVECTOR (info)->contents[0] = make_number(xw->id);
  XVECTOR (info)->contents[1] = xw->type;
  XVECTOR (info)->contents[2] = make_number(xw->width);
  XVECTOR (info)->contents[3] = make_number(xw->height);


  return info;
}



//xterm.c listens to xwidget_owns_kbd  and tries to not eat events when its set
int xwidget_owns_kbd = 0;
DEFUN ("xwidget-set-keyboard-grab", Fxwidget_set_keyboard_grab, Sxwidget_set_keyboard_grab, 2, 2, 0, doc:	/* set unset kbd grab for xwidget. */
       )
  (Lisp_Object xwidget_id, Lisp_Object kbd_grab)
{
  struct xwidget *xw = xid2xw(xwidget_id);
  int xid, kbd_flag;

  CHECK_NUMBER (kbd_grab);
  kbd_flag = XFASTINT (kbd_grab);


  
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
  struct xwidget *xw = xid2xw(xwidget_id);
  GdkWindow *window;
  XID xid;

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

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

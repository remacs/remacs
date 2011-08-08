#ifndef XWIDGET_H_INCLUDED
#define XWIDGET_H_INCLUDED

void x_draw_xwidget_glyph_string (struct glyph_string *s);
void syms_of_xwidget ();

extern Lisp_Object Qxwidget;


int valid_xwidget_p (Lisp_Object object) ;

#include <gtk/gtk.h>


/*
each xwidget instance/model is described by this struct.

lisp pseudovector.


 */
struct xwidget{
  struct vectorlike_header header;  
  Lisp_Object plist;//auxilliary data
  Lisp_Object type;//the widget type
  Lisp_Object buffer; //buffer where xwidget lives
  Lisp_Object title;//a title that is used for button labels for instance
  
  //here ends the lisp part.
  //"height" is the marker field
  int height;
  int width;

  //for offscreen widgets, unused if not osr
  GtkWidget* widget_osr;
  GtkContainer* widgetwindow_osr;

  //TODO these are WIP


  
};


//struct for each xwidget view
struct xwidget_view{
  struct vectorlike_header header;
  struct xwidget* model; //TODO should be lisp

  
  //here ends the lisp part.
  //"redisplayed" is the marker field
  int redisplayed; //if touched by redisplay  


  struct window *w; //TODO should be lisp
  
  int hidden;//if the "live" instance isnt drawn

  int initialized;  

  GtkWidget* widget;
  GtkContainer* widgetwindow;
  GtkContainer* emacswindow;
  int x; int y;
  int clip_right; int clip_bottom; int clip_top; int clip_left;


  long handler_id;
};


/* Test for xwidget (xwidget . spec)  (car must be the symbol xwidget)*/
#define XWIDGETP(x) (CONSP (x) && EQ (XCAR (x), Qxwidget))
/* Test for xwidget pseudovector*/
#define XXWIDGETP(x) PSEUDOVECTORP (x, PVEC_XWIDGET)
#define XXWIDGET(a) (eassert (XWIDGETP(a)),(struct xwidget *) XPNTR(a))


struct xwidget_type
{
  /* A symbol uniquely identifying the xwidget type, */
  Lisp_Object *type;

  /* Check that SPEC is a valid image specification for the given
     image type.  Value is non-zero if SPEC is valid.  */
  int (* valid_p) (Lisp_Object spec);

  /* Next in list of all supported image types.  */
  struct xwidget_type *next;
};
                             
static struct xwidget_type *lookup_xwidget_type (Lisp_Object symbol);

struct xwidget* xwidget_from_id(int id);

//extern int xwidget_owns_kbd;

void xwidget_start_redisplay();
void xwidget_end_redisplay (struct window *w, struct glyph_matrix *matrix);

void xwidget_touch (struct xwidget_view *xw);

//void assert_valid_xwidget_id(int id,char *str);

struct xwidget* lookup_xwidget (Lisp_Object  spec); 
#define XG_XWIDGET "emacs_xwidget"
#define XG_XWIDGET_VIEW "emacs_xwidget_view"
void      xwidget_view_delete_all_in_window(  struct window *w );
#endif

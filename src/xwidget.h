void x_draw_xwidget_glyph_string (struct glyph_string *s);
void syms_of_xwidget ();

extern Lisp_Object Qxwidget;
/* Test for xwidget (xwidget . spec)  (car must be the symbol xwidget)*/
#define XWIDGETP(x) (CONSP (x) && EQ (XCAR (x), Qxwidget))

int valid_xwidget_p (Lisp_Object object) ;

#include <gtk/gtk.h>


/*
each xwidget instance/model is described by this struct.
 */
struct xwidget{
  Lisp_Object plist;//auxilliary data
  Lisp_Object type;//the widget type

  int id; // id is stored inside the struct which is conveniont in some cases
  char* title;//a title that is used for button labels for instance


  int height;
  int width;
  int initialized;
};


//struct for each xwidget view
struct xwidget_view{
  struct xwidget* model;

  int hidden;//if the "live" instance isnt drawn
  int redisplayed; //if touched by redisplay
  int initialized;  

  GtkWidget* widget;
  GtkContainer* widgetwindow;
  GtkContainer* emacswindow;
  int x; int y;
  int clipx; int clipy;
  struct window *w;

  long handler_id;
};





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

extern int xwidget_owns_kbd;

void xwidget_start_redisplay();
void xwidget_end_redisplay(struct glyph_matrix* matrix);
void xwidget_modify_region();

void xwidget_touch (struct xwidget_view *xw);

void assert_valid_xwidget_id(int id,char *str);

int lookup_xwidget (Lisp_Object  spec); 

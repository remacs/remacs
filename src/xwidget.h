void x_draw_xwidget_glyph_string (struct glyph_string *s);
void syms_of_xwidget ();

extern Lisp_Object Qxwidget;
/* Test for xwidget (xwidget . spec)  (car must be the symbol xwidget)*/
#define XWIDGETP(x) (CONSP (x) && EQ (XCAR (x), Qxwidget))

int valid_xwidget_p (Lisp_Object object) ;

#include <gtk/gtk.h>

/*
each xwidget instance is described by this struct.
 */
struct xwidget{
  int id;
  int type;
  int hidden;
  GtkWidget* widget;
  GtkContainer* widgetwindow;

  char* title;
  int initialized;
  int height;
  int width;
  int x; int y;
  Lisp_Object message_hook;
  int redisplayed;
  GtkContainer* emacswindow;
  int clipx; int clipy;
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
                             

static INLINE struct xwidget_type *lookup_xwidget_type (Lisp_Object symbol);



struct xwidget* xwidget_from_id(int id);

extern int xwidget_owns_kbd;

void   xwidget_start_redisplay();
void   xwidget_end_redisplay(struct glyph_matrix* matrix);
void xwidget_modify_region();

void xwidget_touch(struct xwidget* xw);
void xwidget_delete(struct xwidget* xw);
void assert_valid_xwidget_id(int id,char *str);

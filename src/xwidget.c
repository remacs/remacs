#include <config.h>
#ifdef HAVE_XWIDGETS

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



#ifdef HAVE_GOOCANVAS
#include <goocanvas.h>
#endif

#ifdef HAVE_CLUTTER
#include <librsvg/rsvg.h>
#include <clutter/clutter.h>
#include <clutter-gtk/clutter-gtk.h>
#endif

#include <wchar.h>

#ifdef HAVE_WEBKIT_OSR
#include <webkit/webkitwebview.h>
#include <webkit/webkitwebplugindatabase.h>
#include <webkit/webkitwebplugin.h>
#include <webkit/webkitglobals.h>
#include <webkit/webkitwebnavigationaction.h>
#include <webkit/webkitdownload.h>
#endif

//for GIR
#include <girepository.h>

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
  return ALLOCATE_PSEUDOVECTOR (struct xwidget, height, PVEC_XWIDGET);
}

//TODO xwidget_view* should be Lisp_xwidget_view*
struct xwidget_view*
allocate_xwidget_view (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget_view, redisplayed, PVEC_XWIDGET_VIEW);
}
#define XSETXWIDGET(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET))
#define XSETXWIDGET_VIEW(a, b) (XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET_VIEW))

Lisp_Object Qxwidget;
Lisp_Object Qcxwidget;
Lisp_Object Qtitle;
Lisp_Object Qxwidget_set_keyboard_grab;
Lisp_Object Qxwidget_embed_steal_window;
Lisp_Object Qxwidget_info;
Lisp_Object Qxwidget_resize;
Lisp_Object Qxwidget_send_keyboard_event;
Lisp_Object Qcxwgir_class;
Lisp_Object Qbutton, Qtoggle, Qslider, Qsocket, Qsocket_osr, Qcairo, Qxwgir,
  Qwebkit_osr, QCplist;


extern Lisp_Object  QCtype;
extern Lisp_Object QCwidth, QCheight;

struct xwidget_view* xwidget_view_lookup(struct xwidget* xw,     struct window *w);
Lisp_Object xwidget_spec_value ( Lisp_Object spec, Lisp_Object  key,  int *found);
gboolean xwidget_osr_damage_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data) ;
gboolean webkit_osr_key_event_callback (GtkWidget *widget, GdkEventKey *event, gpointer data) ;
void     webkit_osr_document_load_finished_callback (WebKitWebView  *webkitwebview,
                                                     WebKitWebFrame *arg1,
                                                     gpointer        user_data);
gboolean     webkit_osr_download_callback (WebKitWebView  *webkitwebview,
                                       WebKitDownload *arg1,
                                       gpointer        data);

gboolean  webkit_osr_mime_type_policy_typedecision_requested_callback(WebKitWebView           *webView,
                                                                      WebKitWebFrame          *frame,
                                                                      WebKitNetworkRequest    *request,
                                                                      gchar                   *mimetype,
                                                                      WebKitWebPolicyDecision *policy_decision,
                                                                      gpointer                 user_data);

gboolean webkit_osr_new_window_policy_decision_requested_callback(WebKitWebView             *webView,
                                                                  WebKitWebFrame            *frame,
                                                                  WebKitNetworkRequest      *request,
                                                                  WebKitWebNavigationAction *navigation_action,
                                                                  WebKitWebPolicyDecision   *policy_decision,
                                                                  gpointer                   user_data);


gboolean webkit_osr_navigation_policy_decision_requested_callback(WebKitWebView             *webView,
                                                        WebKitWebFrame            *frame,
                                                        WebKitNetworkRequest      *request,
                                                        WebKitWebNavigationAction *navigation_action,
                                                        WebKitWebPolicyDecision   *policy_decision,
                                                                  gpointer                   user_data);

gboolean
xwgir_event_callback (GtkWidget *widget,
                      GdkEvent  *event,
                      gpointer   user_data);

GtkWidget* xwgir_create(char* class, char* namespace);
static void
send_xembed_ready_event (struct xwidget* xw, int xembedid);  
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
  //TODO buffer should be an optional argument not just assumed to be the current buffer
  XSETBUFFER(xw->buffer,  Fcurrent_buffer()); // conservatively gcpro xw since we call lisp
  xw->height = XFASTINT(height);
  xw->width = XFASTINT(width);
  XSETPSEUDOVECTOR (val, xw, PVEC_XWIDGET); //?? dunno why i need this
  Vxwidget_alist = Fcons ( val, Vxwidget_alist);
  xw->widgetwindow_osr = NULL;
  xw->widget_osr = NULL;
  xw->plist = Qnil;


#ifdef HAVE_WEBKIT_OSR
  /* DIY mvc. widget is rendered offscreen,
     later bitmap copied to the views.
   */
  if (EQ(xw->type, Qwebkit_osr)||
      EQ(xw->type, Qsocket_osr)||
      (Fget(xw->type, Qcxwgir_class) != Qnil)){
    printf("init osr widget\n");
    block_input();
    xw->widgetwindow_osr = GTK_CONTAINER (gtk_offscreen_window_new ());
    gtk_window_resize(    GTK_WINDOW(xw->widgetwindow_osr), xw->width, xw->height);

    if (EQ(xw->type, Qwebkit_osr))
      xw->widget_osr = webkit_web_view_new();
    if(EQ(xw->type, Qsocket_osr))
      xw->widget_osr = gtk_socket_new();    
    if(Fget(xw->type, Qcxwgir_class) != Qnil)
      xw->widget_osr = xwgir_create(    SDATA(Fcar(Fcdr(Fget(xw->type, Qcxwgir_class)))),
                                        SDATA(Fcar(Fget(xw->type, Qcxwgir_class))));

    gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width, xw->height);
    gtk_container_add (xw->widgetwindow_osr, xw->widget_osr);

    gtk_widget_show_all (GTK_WIDGET (xw->widgetwindow_osr));

    /* store some xwidget data in the gtk widgets for convenient retrieval in the event handlers. */
    g_object_set_data (G_OBJECT (xw->widget_osr), XG_XWIDGET, (gpointer) (xw));
    g_object_set_data (G_OBJECT (xw->widgetwindow_osr), XG_XWIDGET, (gpointer) (xw));
    /* signals */
    g_signal_connect (G_OBJECT ( xw->widgetwindow_osr), "damage-event",    G_CALLBACK (xwidget_osr_damage_event_callback), NULL);


    if (EQ(xw->type, Qwebkit_osr)){
      g_signal_connect (G_OBJECT ( xw->widget_osr),
                        "document-load-finished",
                        G_CALLBACK (webkit_osr_document_load_finished_callback),
                        xw);    
      
      g_signal_connect (G_OBJECT ( xw->widget_osr),
                        "download-requested",
                        G_CALLBACK (webkit_osr_download_callback),
                        xw);    

      g_signal_connect (G_OBJECT ( xw->widget_osr),
                        "mime-type-policy-decision-requested",
                        G_CALLBACK (webkit_osr_mime_type_policy_typedecision_requested_callback),
                        xw);    

      g_signal_connect (G_OBJECT ( xw->widget_osr),
                        "new-window-policy-decision-requested",
                        G_CALLBACK (webkit_osr_new_window_policy_decision_requested_callback),
                        xw);    

      g_signal_connect (G_OBJECT ( xw->widget_osr),
                        "navigation-policy-decision-requested",
                        G_CALLBACK (webkit_osr_navigation_policy_decision_requested_callback),
                        xw);
      //webkit_web_view_load_uri(WEBKIT_WEB_VIEW(xw->widget_osr), "http://www.fsf.org");

    }

    if (EQ(xw->type, Qsocket_osr)) {
    printf ("xwid:%d socket id:%x %d\n",
            xw,
            gtk_socket_get_id (GTK_SOCKET (xw->widget_osr)),
            gtk_socket_get_id (GTK_SOCKET (xw->widget_osr)));
    send_xembed_ready_event (xw,
                             gtk_socket_get_id (GTK_SOCKET (xw->widget_osr)));
    //gtk_widget_realize(xw->widget);
  }


    unblock_input();

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
  event.arg = Fcons ((Lisp_Object)xw, event.arg); //TODO send the actual xwidget object now instead
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
  event.arg = Fcons ((Lisp_Object)xw, event.arg); //TODO
  event.arg = Fcons (intern ("xembed-ready"), event.arg);


  kbd_buffer_store_event (&event);

}

void
xwidget_show_view (struct xwidget_view *xv)
{
  xv->hidden = 0;
  gtk_widget_show(GTK_WIDGET(xv->widgetwindow));
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), GTK_WIDGET (xv->widgetwindow),  xv->x  + xv->clip_left, xv->y + xv->clip_top); //TODO refactor
}


/* hide an xvidget view */
void
xwidget_hide_view (struct xwidget_view *xv)
{
  xv->hidden = 1;
  //gtk_widget_hide(GTK_WIDGET(xw->widgetwindow));
  gtk_fixed_move (GTK_FIXED (xv->emacswindow), GTK_WIDGET (xv->widgetwindow),
                  10000, 10000);
}


void
xwidget_plug_added(GtkSocket *socket,
                   gpointer   user_data)
{
  //hmm this doesnt seem to get called for foreign windows
  printf("xwidget_plug_added\n");
}

gboolean
xwidget_plug_removed(GtkSocket *socket,
                     gpointer   user_data)
{
  printf("xwidget_plug_removed\n");
  return TRUE; /* dont run the default handler because that kills the socket and we want to reuse it*/
}


void
xwidget_slider_changed (GtkRange *range,
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
      if(xv->initialized && xvp->model == xv->model){
        g_signal_handler_block( xv->widget,xv->handler_id);
      }
    }
  //set values of sibling views and unblock
  for (int i = 0; i < MAX_XWIDGETS; i++)
    {
      xv = &xwidget_views[i];
      if(xv->initialized && xvp->model == xv->model){
        gtk_range_set_value(GTK_RANGE(xv->widget), v);
        g_signal_handler_unblock( xv->widget,xv->handler_id);
      }
    }

}


/* when the off-screen webkit master view changes this signal is called.
   it copies the bitmap from the off-screen webkit instance */
gboolean
xwidget_osr_damage_event_callback (GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  //TODO this is wrong! should just queu a redraw of onscreen widget
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  struct xwidget_view* xv;
  //webkit_osr_redraw_child(xw, widget);
  printf ("damage\n");
  for (int i = 0; i < MAX_XWIDGETS; i++)//todo mvc refactor
    {
      xv = &xwidget_views[i];
      if(xv->initialized && xv->model == xw){
        gtk_widget_queue_draw (xv->widget); //redraw all views, the master has changed
      }
    }

  return FALSE;
}



gboolean
webkit_osr_key_event_callback (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
  printf("terminating a webkit osr keypress\n");
  //TRUE terminate the event here. no paren handlers will be called. but webkit then doesng get the event and it still crashes
  //FALSE paren handlers will be called. webkit then gets the event and it still crashes
  return TRUE; 
}


void
store_xwidget_event_string(struct xwidget* xw, char* eventname, const char* eventstr)
{
  //refactor attempt
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET(xwl,xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;	//frame; //how to get the frame here? //TODO i store it in the xwidget now

  event.arg = Qnil;
  event.arg = Fcons (build_string(eventstr), event.arg);  //string so dont intern
  event.arg = Fcons (xwl, event.arg); //TODO
  event.arg = Fcons (intern (eventname), event.arg);//interning should be ok
  kbd_buffer_store_event (&event);

}

//TODO deprecated, use load-status
void
webkit_osr_document_load_finished_callback (WebKitWebView  *webkitwebview,
                                                     WebKitWebFrame *arg1,
                                                     gpointer        data)
{
  //TODO this event sending code should be refactored
  //  struct xwidget *xw = (struct xwidget *) data;
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webkitwebview), XG_XWIDGET);  
  printf("webkit finished loading\n");

  store_xwidget_event_string(xw, 
                             "document-load-finished", "");
}

gboolean
webkit_osr_download_callback (WebKitWebView  *webkitwebview,
                                       WebKitDownload *arg1,
                                       gpointer        data)
{
  //TODO this event sending code should be refactored
  struct input_event event;
  //  struct xwidget *xw = (struct xwidget *) data;
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webkitwebview), XG_XWIDGET);  
  printf("download requested %s\n", webkit_download_get_uri (arg1));


  printf("webkit finished loading\n");

  store_xwidget_event_string(xw, "download-requested", webkit_download_get_uri (arg1));

  return FALSE;
}

gboolean
webkit_osr_mime_type_policy_typedecision_requested_callback(WebKitWebView           *webView,
                                                            WebKitWebFrame          *frame,
                                                            WebKitNetworkRequest    *request,
                                                            gchar                   *mimetype,
                                                            WebKitWebPolicyDecision *policy_decision,
                                                            gpointer                 user_data)
{
  printf("mime policy requested\n");
  // this function makes webkit send a download signal for all unknown mime types
  // TODO defer the decision to lisp, so that its possible to make Emacs handle text mime for instance
  if(!webkit_web_view_can_show_mime_type(webView, mimetype)){
    webkit_web_policy_decision_download (policy_decision);
    return TRUE;
  }else{
    return FALSE;
  }
}


gboolean
webkit_osr_new_window_policy_decision_requested_callback(WebKitWebView             *webView,
                                                         WebKitWebFrame            *frame,
                                                         WebKitNetworkRequest      *request,
                                                         WebKitWebNavigationAction *navigation_action,
                                                         WebKitWebPolicyDecision   *policy_decision,
                                                         gpointer                   user_data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webView), XG_XWIDGET);  
  printf("webkit_osr_new_window_policy_decision_requested_callback %s\n",
         webkit_web_navigation_action_get_original_uri (navigation_action));
  
  store_xwidget_event_string(xw,  "new-window-policy-decision-requested", webkit_web_navigation_action_get_original_uri (navigation_action)
                            );
  return FALSE;
}

gboolean
webkit_osr_navigation_policy_decision_requested_callback(WebKitWebView             *webView,
                                                         WebKitWebFrame            *frame,
                                                         WebKitNetworkRequest      *request,
                                                         WebKitWebNavigationAction *navigation_action,
                                                         WebKitWebPolicyDecision   *policy_decision,
                                                         gpointer                   user_data)
{
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (webView), XG_XWIDGET);  
  printf("webkit_osr_navigation_policy_decision_requested_callback %s\n",
         webkit_web_navigation_action_get_original_uri (navigation_action));
  store_xwidget_event_string(xw,  "navigation-policy-decision-requested", webkit_web_navigation_action_get_original_uri (navigation_action)
                            );
  return FALSE;
}

//for gtk3 offscreen rendered widgets
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

GtkWidget* xwgir_create_debug;



gboolean
xwidget_osr_event_forward (GtkWidget *widget,
                           GdkEvent  *event,
                           gpointer   user_data)
{
  /* copy events that arrive at the outer widget to the offscreen widget */
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  GdkEvent* eventcopy =  gdk_event_copy(event);
  //GdkEvent* eventcopy =  gdk_event_new(GDK_BUTTON_PRESS);
  
  
  //((GdkEventAny*)eventcopy)->window = gtk_widget_get_window(xw->widget_osr);
  //eventcopy->any.window = gtk_widget_get_window(GTK_WIDGET (xw->widgetwindow_osr));
  //((GdkEventAny*)eventcopy)->window = gtk_widget_get_window(xwgir_create_debug);
  eventcopy->any.window = gtk_widget_get_window(xw->widget_osr);//gtk_widget_get_window(xwgir_create_debug);
  //eventcopy->any.window = gtk_button_get_event_window(GTK_BUTTON(xw->widget_osr));//gtk_widget_get_window(xwgir_create_debug);
  //eventcopy->button.x=200; eventcopy->button.y=200;
  //event->button.button = GDK_BUTTON_PRIMARY; //debug
  
  //eventcopy->any.window = xw->widgetwindow_osr;//gtk_widget_get_window(xwgir_create_debug);
  /* eventcopy->any.send_event = TRUE; */
  /* eventcopy->button.time = GDK_CURRENT_TIME; */
  /* eventcopy->button.device =   event->button.device; */

  
  printf("xwidget_osr_event_forward redirect event to window:%d\n",   ((GdkEventAny*)eventcopy)->window);
  printf("A type:%d x:%f y:%f \n",   event->type, event->button.x, event->button.y);  
  printf("B type:%d x:%f y:%f \n",   eventcopy->type, eventcopy->button.x, eventcopy->button.y);  
    //gtk_button_get_event_window(xwgir_create_debug);
  gtk_main_do_event(eventcopy); //TODO this will leak events. they should be deallocated later, perhaps in xwgir_event_callback
  //printf("gtk_widget_event:%d\n",gtk_widget_event(xw->widget_osr, eventcopy));
  //gdk_event_put(eventcopy);
  //gdk_event_queue_append(eventcopy);
  //gdk_event_free(eventcopy);
  return TRUE; //dont propagate this event furter
  //return FALSE; //dont propagate this event furter
}

gboolean
xwgir_event_callback (GtkWidget *widget,
                             GdkEvent  *event,
                             gpointer   user_data)
{
  //debugging
  //perhaps delete copied events here
  struct xwidget* xw = (struct xwidget*) g_object_get_data (G_OBJECT (widget), XG_XWIDGET);
  printf("xwgir_event_callback\n");
  return FALSE;
}



GIRepository *girepository ;
DEFUN( "xwgir-require-namespace",Fxwgir_require_namespace, Sxwgir_require_namespace, 2,2,0,
       doc: /*require a namespace. must be done for all namespaces we want to use, before using other xwgir functions.*/)
  (Lisp_Object lnamespace, Lisp_Object lnamespace_version)  
{
  char* namespace = SDATA(lnamespace);
  char* namespace_version = SDATA(lnamespace_version);
  GError *error = NULL;

  girepository = g_irepository_get_default();
  g_irepository_require(girepository, namespace, namespace_version, 0, &error);
  if (error) {
    g_error("ERROR: %s\n", error->message);
    return Qnil;
  }
  return Qt;
}

GtkWidget* xwgir_create(char* class, char* namespace){
  //TODO this is more or less the same as xwgir-call-method, so should be refactored
  //create a gtk widget, given its name
  //find the constructor
  //call it
  //also figure out how to pass args

  GError *error = NULL;
  GIArgument return_value;

  GIObjectInfo* obj_info = g_irepository_find_by_name(girepository, namespace, class);
  GIFunctionInfo* f_info = g_object_info_find_method (obj_info, "new");
  g_function_info_invoke(f_info,
                         NULL, 0,
                         NULL, 0,
                         &return_value,
                         NULL);
  xwgir_create_debug = return_value.v_pointer;
  return return_value.v_pointer;
  
}

int
xwgir_convert_lisp_to_gir_arg(GIArgument* giarg,
                              GIArgInfo* arginfo,
                              Lisp_Object lisparg )
{

  GITypeTag   tag;
  gboolean    is_pointer;
  gboolean    is_enum;
  tag =  g_type_info_get_tag (g_arg_info_get_type (arginfo));
  
  switch (tag)
    {
    case GI_TYPE_TAG_BOOLEAN:
      giarg->v_boolean = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_INT8:
      giarg->v_int8 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT8:
      giarg->v_uint8 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_INT16:
      giarg->v_int16 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT16:
      giarg->v_uint16 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_INT32:
      giarg->v_int32 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT32:
      giarg->v_uint32 = XFASTINT(lisparg);
      break;

    case GI_TYPE_TAG_INT64:
      giarg->v_int64 = XFASTINT(lisparg);
      break;
    case GI_TYPE_TAG_UINT64:
      giarg->v_uint64 = XFASTINT(lisparg);
      break;


    case GI_TYPE_TAG_FLOAT:
      giarg->v_float = XFLOAT_DATA(lisparg);
      break;

    case GI_TYPE_TAG_DOUBLE:
      giarg->v_double = XFLOAT_DATA(lisparg);
      break;

    case GI_TYPE_TAG_UTF8:
    case GI_TYPE_TAG_FILENAME:
      //giarg->v_string = SDATA(lisparg);
      giarg->v_pointer = SDATA(lisparg);
      break;
      
    case GI_TYPE_TAG_ARRAY:
    case GI_TYPE_TAG_GLIST:
    case GI_TYPE_TAG_GSLIST:
    case GI_TYPE_TAG_GHASH:
    case GI_TYPE_TAG_ERROR:
    case GI_TYPE_TAG_INTERFACE:
    case GI_TYPE_TAG_VOID:
    case GI_TYPE_TAG_UNICHAR:
    case GI_TYPE_TAG_GTYPE:
      //?? i dont know how to handle these yet TODO
      printf("failed in my lisp to gir arg conversion duties. sob!\n");
      return -1;
      break;
    }
  return 0;
}

#if 0
void
refactor_attempt(){
  //this methhod should be called from xwgir-xwidget-call-method and from xwgir xwidget construction  
  char* class = SDATA(Fcar(Fcdr(Fget(xw->type, Qcxwgir_class))));

  GIObjectInfo* obj_info = g_irepository_find_by_name(girepository, namespace, class);
  GIFunctionInfo* f_info = g_object_info_find_method (obj_info, SDATA(method));

  //loop over args, convert from lisp to primitive type, given arg introspection data
  //TODO g_callable_info_get_n_args(f_info) should match
  int argscount = XFASTINT(Flength(arguments));
  if(argscount !=  g_callable_info_get_n_args(f_info)){
    printf("xwgir call method arg count doesn match! \n");
    return Qnil;
  }
  int i;
  for (i = 1; i < argscount + 1; ++i)
    {
      xwgir_convert_lisp_to_gir_arg(&in_args[i], g_callable_info_get_arg(f_info, i - 1), Fnth(i - 1, arguments));
    }

  in_args[0].v_pointer = widget;
  if(g_function_info_invoke(f_info,
                            in_args, argscount + 1,
                            NULL, 0,
                            &return_value,
                            &error)) { 
    //g_error("ERROR: %s\n", error->message);
    printf("invokation error\n");
     return Qnil; 
   }   
  return Qt;
}
#endif

DEFUN ("xwgir-xwidget-call-method", Fxwgir_xwidget_call_method,  Sxwgir_xwidget_call_method,       3, 3, 0,
       doc:	/* call xwidget object method.*/)
  (Lisp_Object xwidget, Lisp_Object method, Lisp_Object arguments)
{
  GError *error = NULL;
  GIArgument return_value;
  GIArgument in_args[20];


  struct xwidget* xw; 
  if(!XXWIDGETP(xwidget)) {printf("ERROR not an xwidget\n"); return Qnil;}; 
  if(Qnil == xwidget) {printf("ERROR xwidget nil\n");   return Qnil;};  
  xw = XXWIDGET(xwidget);                                               
  if(NULL == xw) printf("ERROR xw is 0\n");                               
  char* namespace = SDATA(Fcar(Fget(xw->type, Qcxwgir_class)));
  //we need the concrete widget, which happens in 2 ways depending on OSR or not TODO
  GtkWidget* widget = NULL;
  if(NULL == xw->widget_osr) {
    widget = xwidget_view_lookup (xw, XWINDOW(FRAME_SELECTED_WINDOW (SELECTED_FRAME ()))) -> widget; 
  } else {
    widget = xw->widget_osr;
  }
       
  //char* class = SDATA(SYMBOL_NAME(xw->type)); //this works but is unflexible
  //figure out the class from the widget instead
  /* printf("type class: %s %s\n", G_OBJECT_TYPE_NAME(widget), G_OBJECT_CLASS_NAME(G_OBJECT_GET_CLASS(widget))); */
  /* char* class = G_OBJECT_TYPE_NAME(widget); //gives "GtkButton"(I want "Button") */
  /* class += strlen(namespace);  //TODO check for corresponding api method. but this seems to work. */

  char* class = SDATA(Fcar(Fcdr(Fget(xw->type, Qcxwgir_class))));

  GIObjectInfo* obj_info = g_irepository_find_by_name(girepository, namespace, class);
  GIFunctionInfo* f_info = g_object_info_find_method (obj_info, SDATA(method));

  //loop over args, convert from lisp to primitive type, given arg introspection data
  //TODO g_callable_info_get_n_args(f_info) should match
  int argscount = XFASTINT(Flength(arguments));
  if(argscount !=  g_callable_info_get_n_args(f_info)){
    printf("xwgir call method arg count doesn match! \n");
    return Qnil;
  }
  int i;
  for (i = 1; i < argscount + 1; ++i)
    {
      xwgir_convert_lisp_to_gir_arg(&in_args[i], g_callable_info_get_arg(f_info, i - 1), Fnth(i - 1, arguments));
    }

  in_args[0].v_pointer = widget;
  if(g_function_info_invoke(f_info,
                            in_args, argscount + 1,
                            NULL, 0,
                            &return_value,
                            &error)) { 
    //g_error("ERROR: %s\n", error->message);
    printf("invokation error\n");
     return Qnil; 
   }   
  return Qt;
}

 void
to_child (GtkWidget *bin,
          double         widget_x,
          double         widget_y,
          double        *x_out,
          double        *y_out)
{
  *x_out = widget_x;
  *y_out = widget_y;
}


void
offscreen_window_from_parent (GdkWindow     *window,
                              double         parent_x,
                              double         parent_y,
                              double        *offscreen_x,
                              double        *offscreen_y,
                              GtkWidget *bin)
{
  /* printf("offscreen_window_from_parent %d  %f,%f  %f,%f\n", */
  /*        window, */
  /*        parent_x, */
  /*        parent_y, */
  /*        offscreen_x, */
  /*        offscreen_y         ); */
  to_child (bin, parent_x, parent_y, offscreen_x, offscreen_y);
}

 GdkWindow *
pick_offscreen_child (GdkWindow     *offscreen_window,
                      double         widget_x,
                      double         widget_y,
                      GdkWindow *bin)
{
  //in this simple case we assume the window contains a single widget. easy.
  //but then we get the problem that the widget cant be embedded in several windows
    printf("pick_offscreen_child %d %f %f  %d\n",
offscreen_window,
    widget_x,
    widget_y,
           bin           );
  return bin;
}


void
xwidget_set_embedder_view(struct xwidget* xww,
                          struct xwidget_view* xv){
  printf("gdk_offscreen_window_set_embedder %d %d\n",
         GDK_IS_WINDOW(gtk_widget_get_window (xww->widget_osr)),
         GDK_IS_WINDOW(gtk_widget_get_window (GTK_WIDGET (xv->widget))));
  //set_embedder needs to be called after xv->widget realization
  gdk_offscreen_window_set_embedder (      gtk_widget_get_window (xww->widget_osr),
                                           gtk_widget_get_window (GTK_WIDGET (xv->widget))
                                           

                                           );
  //this signal doesnt seem completely necessary
  /* g_signal_connect (gtk_widget_get_window (xww->widget_osr), "from-embedder", */
  /*                   G_CALLBACK (offscreen_window_from_parent), gtk_widget_get_window (GTK_WIDGET (xv->widget))); */
  //but this one is
  g_signal_connect (gtk_widget_get_window (xv->widget), "pick-embedded-child",
                    G_CALLBACK (pick_offscreen_child), gtk_widget_get_window (xww->widget_osr));
}

gboolean
xwidget_osr_event_set_embedder (GtkWidget *widget,
                           GdkEvent  *event,
                           gpointer   xv)
{
  xwidget_set_embedder_view(((struct xwidget_view*) xv)->model,
                            (struct xwidget_view*) xv);
}

 
int xwidget_view_index=0;

/* initializes and does initial placement of an xwidget view on screen */
struct xwidget_view*
xwidget_init_view (struct xwidget *xww,
                   struct glyph_string *s,
                   int x, int y)
{
  //TODO temp code replace with lisp list
  struct xwidget_view *xv;
  GdkColor color;

  do{
    if(xwidget_view_index < MAX_XWIDGETS)
      xwidget_view_index++;
    else
      xwidget_view_index=0;

    xv = &xwidget_views[xwidget_view_index];
  }while(  xv->initialized == 1); //TODO yeah this can infloop if there are MAX_WIDGETS on-screen

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
    //xv->widget = gtk_entry_new ();//temp hack to experiment with key propagation TODO entry widget is useful for testing
  } else if (EQ(xww->type, Qsocket)) {
    xv->widget = gtk_socket_new ();
    g_signal_connect_after(xv->widget, "plug-added", G_CALLBACK(xwidget_plug_added), "plug added");
    g_signal_connect_after(xv->widget, "plug-removed", G_CALLBACK(xwidget_plug_removed), "plug removed");
    //TODO these doesnt help
    gtk_widget_add_events(xv->widget, GDK_KEY_PRESS);
    gtk_widget_add_events(xv->widget, GDK_KEY_RELEASE);
  } else if (EQ(xww->type, Qslider)) {
    xv->widget =
      //gtk_hscale_new (GTK_ADJUSTMENT(gtk_adjustment_new (0.0, 0.0, 100.0, 1.0, 10.0, 10.0)));
      gtk_hscale_new_with_range ( 0.0, 100.0, 10.0);
    gtk_scale_set_draw_value (GTK_SCALE (xv->widget), FALSE);	//i think its emacs role to show text and stuff, so disable the widgets own text
    xv->handler_id = g_signal_connect_after(xv->widget, "value-changed", G_CALLBACK(xwidget_slider_changed), "slider changed");
  } else if (EQ(xww->type, Qcairo)) {
    //Cairo view
    //uhm cairo is differentish in gtk 3.
    //gdk_cairo_create (gtk_widget_get_window (FRAME_GTK_WIDGET (s->f)));
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
  } else if (EQ(xww->type, Qwebkit_osr)||
             EQ(xww->type, Qsocket_osr)||
             (Fget(xww->type, Qcxwgir_class) != Qnil))//xwgir widgets are OSR
    {
#ifdef HAVE_WEBKIT_OSR //TODO the ifdef isnt really relevant anymore, we always have osr
      printf("osr init:%s\n",SDATA(SYMBOL_NAME(xww->type)));
    xv->widget = gtk_drawing_area_new();
    gtk_widget_set_app_paintable ( xv->widget, TRUE); //because expose event handling
    gtk_widget_add_events(xv->widget,
                          GDK_BUTTON_PRESS_MASK
                          | GDK_BUTTON_RELEASE_MASK
                          | GDK_POINTER_MOTION_MASK);


    if (EQ(xww->type, Qwebkit_osr)){
      /* ///xwgir debug */
      /* //forward events. this isnt compatible with the set_embedded strategy */
      g_signal_connect (G_OBJECT (    xv->widget), "button-press-event",
                        G_CALLBACK (xwidget_osr_event_forward), NULL);
      g_signal_connect (G_OBJECT (    xv->widget), "button-release-event",
                        G_CALLBACK (xwidget_osr_event_forward), NULL);
      g_signal_connect (G_OBJECT (    xv->widget), "motion-notify-event",
                        G_CALLBACK (xwidget_osr_event_forward), NULL);
    }else{
      //xwgir debug , orthogonal to forwarding
      g_signal_connect (G_OBJECT (    xv->widget), "motion-notify-event",
                        G_CALLBACK (xwidget_osr_event_set_embedder), xv);
    }
    
    //draw
    g_signal_connect (G_OBJECT (    xv->widget), "draw",
                      G_CALLBACK (xwidget_osr_draw_callback), NULL);




    /* g_signal_connect (G_OBJECT (    xv->widget), "key-press-event", */
    /*                   G_CALLBACK (xwidget_osr_event_forward), NULL); */
    /* g_signal_connect (G_OBJECT (    xv->widget), "key-release-event", */
    /*                   G_CALLBACK (xwidget_osr_event_forward), NULL); */
    
#endif


  } 
    //else return NULL;

  //widget realization
  //make container widget 1st, and put the actual widget inside the container
  //later, drawing should crop container window if necessary to handle case where xwidget
  //is partially obscured by other emacs windows
  //other containers than gtk_fixed where explored, but gtk_fixed had the most predictable behaviour so far.
  xv->emacswindow = GTK_CONTAINER (FRAME_GTK_WIDGET (s->f));
  xv->widgetwindow = GTK_CONTAINER (gtk_fixed_new ()); 
  gtk_widget_set_has_window(GTK_WIDGET (  xv->widgetwindow), TRUE);
  gtk_container_add (xv->widgetwindow, xv->widget);

  //store some xwidget data in the gtk widgets
  g_object_set_data (G_OBJECT (xv->widget), XG_FRAME_DATA, (gpointer) (s->f)); //the emacs frame
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET, (gpointer) (xww)); //the xwidget
  g_object_set_data (G_OBJECT (xv->widget), XG_XWIDGET_VIEW, (gpointer) (xv)); //the xwidget
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET, (gpointer) (xww)); //the xwidget window
  g_object_set_data (G_OBJECT (xv->widgetwindow), XG_XWIDGET_VIEW, (gpointer) (xv)); //the xwidget window


  gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xww->width, xww->height);
  gtk_widget_set_size_request (GTK_WIDGET (xv->widgetwindow), xww->width, xww->height);
  gtk_fixed_put (GTK_FIXED (FRAME_GTK_WIDGET (s->f)), GTK_WIDGET (xv->widgetwindow), x, y);
  xv->x = x;  xv->y = y;
  gtk_widget_show_all (GTK_WIDGET (xv->widgetwindow));


  
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

  //////////////////////////////////////////////////////////////
  //xwgir debug
  if (//EQ(xww->type, Qwebkit_osr)|| //TODO should be able to choose compile time which method to use with webkit
      EQ(xww->type, Qsocket_osr)||
      (Fget(xww->type, Qcxwgir_class) != Qnil))//xwgir widgets are OSR
    {
      //xwidget_set_embedder_view(xww,xv);
      printf("gdk_offscreen_window_set_embedder %d %d\n",
             GDK_IS_WINDOW(gtk_widget_get_window (xww->widget_osr)),
             GDK_IS_WINDOW(gtk_widget_get_window (GTK_WIDGET (xv->widget))));
      //set_embedder needs to be called after xv->widget realization
      gdk_offscreen_window_set_embedder (      gtk_widget_get_window (xww->widget_osr),
                                               gtk_widget_get_window (GTK_WIDGET (xv->widget))

                                               );
      /* g_signal_connect (gtk_widget_get_window (xww->widget_osr), "from-embedder", */
      /*                   G_CALLBACK (offscreen_window_from_parent), gtk_widget_get_window (GTK_WIDGET (xv->widget))); */
      g_signal_connect (gtk_widget_get_window (xv->widget), "pick-embedded-child",
                        G_CALLBACK (pick_offscreen_child), gtk_widget_get_window (xww->widget_osr));
    }
  ////////////////////////////////////////
  
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

  if (xv == NULL || xv->initialized == 0){
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
  //this happens when an emacs window border moves across a widget window
  //so, if any corner of the outer widget clippng window moves, that counts as movement here, even
  //if it looks like no movement happens because the widget sits still inside the clipping area.
  //the widget can also move inside the clipping area, which happens later
  moved = (xv->x  + xv->clip_left != x+clip_left)
    || ((xv->y + xv->clip_top)!= (y+clip_top));
  if(moved)    printf ("lxwidget moved: id:%d (%d,%d)->(%d,%d) y+clip_top:%d\n", xww, xv->x, xv->y, x, y, y + clip_top);
  else
    printf ("lxwidget DIDNT move: id:%d (%d,%d)->(%d,%d) y+clip_top:%d\n", xww, xv->x, xv->y, x, y, y + clip_top);
  xv->x = x;
  xv->y = y;
  if (moved)	//has it moved?
    {
      if (1)//!xwidget_hidden(xv))	//hidden equals not being seen during redisplay
        {
          //TODO should be possible to use xwidget_show_view here
          gtk_fixed_move (GTK_FIXED (FRAME_GTK_WIDGET (s->f)),
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
  //TODO it seems its possible to get out of sync with emacs redraws so emacs bg sometimes shows up instead of xwidget
  //its just a visual glitch though
  if (!xwidget_hidden(xv)){
    gtk_widget_queue_draw (GTK_WIDGET(xv->widgetwindow));
    gtk_widget_queue_draw (xv->widget);
  }
}


#ifdef HAVE_WEBKIT_OSR

//FUGLY macro that checks WEBKIT_IS_WEB_VIEW(xw->widget_osr) first 
#define WEBKIT_FN_INIT()                        \
  struct xwidget* xw; \
if(!XXWIDGETP(xwidget)) {printf("ERROR not an xwidget\n"); return Qnil;}; \
if(Qnil == xwidget) {printf("ERROR xwidget nil\n");   return Qnil;};    \
  xw = XXWIDGET(xwidget);                                                    \
  if(NULL == xw) printf("ERROR xw is 0\n");                               \
  if((NULL == xw->widget_osr) || !WEBKIT_IS_WEB_VIEW(xw->widget_osr)){  \
    printf("ERROR xw->widget_osr does not hold a webkit instance\n");\
    return Qnil;\
  };


DEFUN ("xwidget-webkit-goto-uri", Fxwidget_webkit_goto_uri,  Sxwidget_webkit_goto_uri,
       2, 2, 0,
       doc:	/* webkit goto uri.*/)
  (Lisp_Object xwidget, Lisp_Object uri)
{
  WEBKIT_FN_INIT();
  webkit_web_view_load_uri ( WEBKIT_WEB_VIEW(xw->widget_osr), SDATA(uri));
  return Qnil;
}


DEFUN ("xwidget-webkit-execute-script", Fxwidget_webkit_execute_script,  Sxwidget_webkit_execute_script,
       2, 2, 0,
       doc:	/* webkit exec js.*/)
  (Lisp_Object xwidget, Lisp_Object script)
{
  WEBKIT_FN_INIT();
  webkit_web_view_execute_script( WEBKIT_WEB_VIEW(xw->widget_osr), SDATA(script));
  return Qnil;
}

DEFUN ("xwidget-webkit-get-title", Fxwidget_webkit_get_title,  Sxwidget_webkit_get_title,
       1, 1, 0,
       doc:	/* webkit get title. can be used to work around exec method lacks return val*/)
  (Lisp_Object xwidget)
{
  //TODO support multibyte strings
  WEBKIT_FN_INIT();
  const gchar* str=webkit_web_view_get_title( WEBKIT_WEB_VIEW(xw->widget_osr));
  //return make_string_from_bytes(str, wcslen((const wchar_t *)str), strlen(str));
  if(str == 0){
    //TODO maybe return Qnil instead. I suppose webkit returns nullpointer when doc is not properly loaded or something
    printf("xwidget-webkit-get-title null webkit title\n");
    return build_string("");
  }
  return build_string(str);
}

//TODO missnamed
DEFUN("xwidget-disable-plugin-for-mime", Fxwidget_disable_plugin_for_mime , Sxwidget_disable_plugin_for_mime,
      1,1,0, doc: /* */)
  (Lisp_Object mime)
{
  WebKitWebPlugin *wp = webkit_web_plugin_database_get_plugin_for_mimetype
    (webkit_get_web_plugin_database(),  SDATA(mime));
  if(wp == NULL) return Qnil;
  if(webkit_web_plugin_get_enabled (wp)){
    webkit_web_plugin_set_enabled  (wp, FALSE);
    return Qt;
  }
  return Qnil;
}


//attempting a workaround for a webkit offscreen bug
//TODO verify its still needed
void
gtk_window_get_position (GtkWindow *window,
                         gint *root_x,
                         gint *root_y)
{
  printf("my getsize\n");
  *root_x = 0;
  *root_y = 0;
}

void
xwidget_webkit_dom_dump(WebKitDOMNode* parent)
{
  WebKitDOMNodeList* list;
  int i;
  int length;
  WebKitDOMNode* attribute;
  WebKitDOMNamedNodeMap* attrs;
  WebKitDOMNode* child;
  printf("node:%d type:%d name:%s content:%s\n",
         parent,
         webkit_dom_node_get_node_type(parent),//1 element 3 text 8 comment 2 attribute
         webkit_dom_node_get_local_name(parent),
         webkit_dom_node_get_text_content(parent));

  if(webkit_dom_node_has_attributes(parent)){
    attrs = webkit_dom_node_get_attributes(parent);

    length = webkit_dom_named_node_map_get_length(attrs);
    for (int i = 0; i < length; i++) {
      attribute = webkit_dom_named_node_map_item(attrs,i);
      printf(" attr node:%d type:%d name:%s content:%s\n",
             attribute,
             webkit_dom_node_get_node_type(attribute),//1 element 3 text 8 comment
             webkit_dom_node_get_local_name(attribute),
             webkit_dom_node_get_text_content(attribute));
    }
  }
  list = webkit_dom_node_get_child_nodes(parent);
  length = webkit_dom_node_list_get_length(list);
  
  for (int i = 0; i < length; i++) {
    child = webkit_dom_node_list_item(list, i);
    //if(webkit_dom_node_has_child_nodes(child))
    xwidget_webkit_dom_dump(child);
  }
}


DEFUN ("xwidget-webkit-dom-dump", Fxwidget_webkit_dom_dump,  Sxwidget_webkit_dom_dump,
       1, 1, 0,
       doc:	/* webkit dom dump*/)
  (Lisp_Object xwidget)
{
  WEBKIT_FN_INIT();
  xwidget_webkit_dom_dump(WEBKIT_DOM_NODE(webkit_web_view_get_dom_document( WEBKIT_WEB_VIEW(xw->widget_osr))));
  return Qnil;
}



#endif





DEFUN ("xwidget-resize", Fxwidget_resize, Sxwidget_resize, 3, 3, 0, doc:
       /* resize xwidgets*/)
  (Lisp_Object xwidget, Lisp_Object new_width, Lisp_Object new_height)
{
  struct xwidget* xw = XXWIDGET(xwidget);
  struct xwidget_view *xv;
  int  w, h;

  CHECK_NUMBER (new_width);
  CHECK_NUMBER (new_height);
  w = XFASTINT (new_width);
  h = XFASTINT (new_height);


  printf("resize xwidget %d (%d,%d)->(%d,%d)\n",xw, xw->width,xw->height,w,h);
  xw->width=w;
  xw->height=h;
  //if theres a osr resize it 1st
  if(xw->widget_osr){
    printf("resize xwidget_osr\n");
    //gtk_container_set_resize_mode ( GTK_WINDOW(xw->widgetwindow_osr), GTK_RESIZE_QUEUE);
    //gtk_container_set_resize_mode ( GTK_WINDOW(xw->widget_osr), GTK_RESIZE_QUEUE);


    //gtk_layout_set_size (GTK_LAYOUT (xw->widgetwindow_osr), xw->width, xw->height);
    gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width, xw->height); //minimum size
    //gtk_window_resize(    GTK_WINDOW(xw->widget_osr), xw->width, xw->height);
    gtk_window_resize(    GTK_WINDOW(xw->widgetwindow_osr), xw->width, xw->height);
    //gtk_container_resize_children ( GTK_WINDOW(xw->widgetwindow_osr));
    gtk_container_resize_children ( GTK_CONTAINER(xw->widgetwindow_osr));
    
  }

  for (int i = 0; i < MAX_XWIDGETS; i++) //TODO MVC refactor lazy linear search
    {
      xv = &xwidget_views[i];
      if(xv->initialized && xv->model == xw){
        gtk_layout_set_size (GTK_LAYOUT (xv->widgetwindow), xw->width, xw->height);
        gtk_widget_set_size_request (GTK_WIDGET (xv->widget), xw->width, xw->height);
      }
    }

  return Qnil;
}

DEFUN ("xwidget-size-request", Fxwidget_size_request, Sxwidget_size_request, 1, 1, 0, doc:
       /* desired size (TODO crashes if arg not osr widget)*/)
  (Lisp_Object xwidget)
{
  GtkRequisition requisition;
  Lisp_Object rv;
  gtk_widget_size_request(XXWIDGET(xwidget)->widget_osr, &requisition);
  rv = Qnil;
  rv = Fcons (make_number(requisition.height), rv);
  rv = Fcons (make_number(requisition.width), rv);
  return rv;

}

DEFUN ("xwidgetp", Fxwidgetp, Sxwidgetp, 1, 1, 0,
       doc: /* Return t if OBJECT is a xwidget.  */)
  (Lisp_Object object)
{
  return XWIDGETP (object) ? Qt : Qnil;
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

DEFUN ("xwidget-send-keyboard-event", Fxwidget_send_keyboard_event, Sxwidget_send_keyboard_event, 2, 2, 0, doc:/* synthesize a kbd event for a xwidget. */
       )
  (Lisp_Object  xwidget, Lisp_Object keydescriptor)
{
  //TODO this code crashes for offscreen widgets and ive tried many different strategies
  //int keyval = 0x058; //X
  int keyval = XFASTINT(keydescriptor); //X
  char *keystring = "";
  GdkKeymapKey* keys;
  gint n_keys;
  GdkDeviceManager* manager;
  struct xwidget *xw;
  GtkWidget* widget;
  GdkEventKey* ev;
  Lisp_Object window;
  //popup_activated_flag = 1; //TODO just a hack
  gdk_keymap_get_entries_for_keyval(gdk_keymap_get_default(), keyval, &keys, &n_keys);
  
  xw = XXWIDGET(xwidget);

  ev = (GdkEventKey*)gdk_event_new(GDK_KEY_PRESS);


  //todo what about windowless widgets?

  window = FRAME_SELECTED_WINDOW (SELECTED_FRAME ());


  //TODO maybe we also need to special case sockets by picking up the plug rather than the socket
  if(xw->widget_osr)
    widget = xw->widget_osr;
  else
    widget = xwidget_view_lookup(xw, XWINDOW(window))->widget;
  
  ev->window = gtk_widget_get_window(widget);
  gtk_widget_grab_focus(widget); 
  ev->send_event = FALSE;

  ev->hardware_keycode = keys[0].keycode;
  ev->group = keys[0].group;
  
  ev->keyval = keyval;
  ev->time = GDK_CURRENT_TIME;

  //ev->device = gdk_device_get_core_pointer();
  manager = gdk_display_get_device_manager(gdk_window_get_display(ev->window));
  gdk_event_set_device ((GdkEvent*)ev,   gdk_device_manager_get_client_pointer(manager));
  gdk_event_put((GdkEvent*)ev);
  //g_signal_emit_by_name(ev->window,"key-press-event", ev);
  
  ev->type = GDK_KEY_RELEASE;
  gdk_event_put((GdkEvent*)ev);
  //g_signal_emit_by_name(ev->window,"key-release-event", ev);
  //gtk_main_do_event(ev);

  //TODO
  //if I delete the event the receiving component eventually crashes.
  //it ough TDTRT since event_put is supposed to copy the event
  //so probably this leaks events now
  //gdk_event_free((GdkEvent*)ev);

  return Qnil;
}



DEFUN("xwidget-delete-zombies", Fxwidget_delete_zombies , Sxwidget_delete_zombies, 0,0,0, doc: /* */)
  (void)
{
  /*
    - remove all views with window gone

    TODO
    - remove all xwidgets with buffer gone
    - remove all views with xw gone

   */
  struct xwidget_view* xv = NULL;
  Lisp_Object w;
  for (int i = 0; i < MAX_XWIDGETS; i++){
      xv =  &xwidget_views[i];
      XSETWINDOW(w,  xv->w);
      if(xv->initialized && (! (WINDOW_LIVE_P(w)))){

        gtk_widget_destroy(GTK_WIDGET(xv->widgetwindow));
        xv->initialized = 0;
      }
  }
}


DEFUN ("xwidget-plist", Fxwidget_plist, Sxwidget_plist,
       1, 1, 0,
       doc: /* Return the plist of XWIDGET.  */)
  (register Lisp_Object xwidget)
{
  //CHECK_XWIDGET (xwidget); //todo
  return XXWIDGET (xwidget)->plist;
}

DEFUN ("xwidget-buffer", Fxwidget_buffer, Sxwidget_buffer,
       1, 1, 0,
       doc: /* Return the buffer of XWIDGET.  */)
  (register Lisp_Object xwidget)
{
  //CHECK_XWIDGET (xwidget); //todo
  return XXWIDGET (xwidget)->buffer;
}

DEFUN ("set-xwidget-plist", Fset_xwidget_plist, Sset_xwidget_plist,
       2, 2, 0,
       doc: /* Replace the plist of XWIDGET with PLIST.  Returns PLIST.  */)
  (register Lisp_Object xwidget, Lisp_Object plist)
{
  //CHECK_XWIDGET (xwidget); //todo
  CHECK_LIST (plist);

  XXWIDGET (xwidget)->plist = plist;
  return plist;
}



void
syms_of_xwidget (void)
{
  int i;

  defsubr (&Smake_xwidget);
  defsubr (&Sxwidgetp);
  defsubr (&Sxwidget_info);
  defsubr (&Sxwidget_view_info);
  defsubr (&Sxwidget_resize);

#ifdef HAVE_WEBKIT_OSR
  defsubr (&Sxwidget_webkit_goto_uri);
  defsubr (&Sxwidget_webkit_execute_script);
  defsubr (&Sxwidget_webkit_get_title);
  DEFSYM (Qwebkit_osr ,"webkit-osr");
#endif

  defsubr (&Sxwgir_xwidget_call_method  );
  defsubr (&Sxwgir_require_namespace);
  defsubr (&Sxwidget_size_request  );
  defsubr (&Sxwidget_delete_zombies);
  defsubr (&Sxwidget_disable_plugin_for_mime);

  defsubr (&Sxwidget_send_keyboard_event);
  defsubr (&Sxwidget_webkit_dom_dump);
  defsubr (&Sxwidget_plist);
  defsubr (&Sxwidget_buffer);
  defsubr (&Sset_xwidget_plist);
  
  DEFSYM (Qxwidget ,"xwidget");

  DEFSYM (Qcxwidget ,":xwidget");
  DEFSYM (Qcxwgir_class ,":xwgir-class");
  DEFSYM (Qtitle ,":title");

  DEFSYM (Qbutton, "Button"); //changed to match the gtk class because xwgir(experimental and not really needed)
  DEFSYM (Qtoggle, "ToggleButton");
  DEFSYM (Qslider, "slider");
  DEFSYM (Qsocket, "socket");
  DEFSYM (Qsocket_osr, "socket-osr");
  DEFSYM (Qcairo, "cairo");

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

  eassert (valid_xwidget_p (spec));

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
xwidget_view_delete_all_in_window (struct window *w)
{
  struct xwidget_view* xv = NULL;
  for (int i = 0; i < MAX_XWIDGETS; i++){
      xv =  &xwidget_views[i];
      if(xv->initialized && xv->w == w){
        gtk_widget_destroy(GTK_WIDGET(xv->widgetwindow));
        xv->initialized = 0;
      }
  }
}



struct xwidget_view*
xwidget_view_lookup (struct xwidget* xw, struct window *w)
{
  struct xwidget_view* xv = NULL;
  for (int i = 0; i < MAX_XWIDGETS; i++){
    xv = &xwidget_views[i];
    if (xv->initialized && (xv->model == xw) && (xv->w == w))
      return xv;
  }
  return NULL; /* we didnt find a matching view */
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
  /* coordinates are not known here */
  printf ("lookup_xwidget xwidget_id:%d type:%d found:%d %d %d title:'%s' (%d,%d)\n", xw,
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
xwidget_touch (struct xwidget_view *xv)
{
  xv->redisplayed = 1;
}

int
xwidget_touched (struct xwidget_view *xv)
{
  return  xv->redisplayed;
}

/* redisplay has ended, now we should hide untouched xwidgets
*/
void
xwidget_end_redisplay (struct window *w, struct glyph_matrix *matrix)
{

  int i;
  struct xwidget *xw;
  int area;


  xwidget_start_redisplay ();
  //iterate desired glyph matrix of window here, hide gtk widgets
  //not in the desired matrix.

  //this only takes care of xwidgets in active windows.
  //if a window goes away from screen xwidget views wust be deleted
  
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
                        the only call to xwidget_end_redisplay is in dispnew
                        xwidget_end_redisplay(w->current_matrix);
                      */
                      xwidget_touch (xwidget_view_lookup(glyph->u.xwidget,
                                                         w));
                    }
                }
            }
        }
    }

  for (i = 0; i < MAX_XWIDGETS; i++)
    {
      struct xwidget_view* xv = &xwidget_views[i];

      //"touched" is only meaningful for the current window, so disregard other views
      if (xv->initialized && ( xv->w ==    w))
        {
          if (xwidget_touched(xv))
            xwidget_show_view (xv);
          else
            xwidget_hide_view (xv);
        }
    }
}

      
#endif

#include "config.h"
#include "limits.h"

#undef HAVE_X_WINDOWS
#undef HAVE_X11
#undef HAVE_WINDOW_SYSTEM

// we want bindgen to include bindings for the same set of functions
// on all platforms. We must thus take care that functions which are
// inline on any platform are always inline here, because we can't
// reliably call into inline functions. See m4/inline-extern.m4 for
// details on why functions might not always be marked inline.
#define INLINE inline
#define EXTERN_INLINE extern inline

#include "lisp.h"

#include "atimer.h"
#include "blockinput.h"
#include "buffer.h"
#include "category.h"
#include "ccl.h"
#include "character.h"
#include "charset.h"
#include "cm.h"
#include "coding.h"
#include "commands.h"
#include "composite.h"
#include "config.h"
#include "conf_post.h"
#include "dispextern.h"
#include "disptab.h"
#include "dynlib.h"
#include "emacs-icon.h"
#include "emacs-module.h"
#include "epaths.h"
#include "font.h"
#include "fontset.h"
#include "frame.h"
#include "getpagesize.h"
//#include "globals.h"
#include "gnutls.h"
#include "indent.h"
#include "intervals.h"
//#include "keyboard.h"
#include "keymap.h"
#include "macros.h"
#include "macuvs.h"
#include "menu.h"
#include "nsterm.h"
#include "process.h"
#include "puresize.h"
#include "regex.h"
#include "region-cache.h"
#include "remacs-lib.h"
#include "sheap.h"
#include "syntax.h"
#include "sysselect.h"
#include "syssignal.h"
#include "sysstdio.h"
#include "systhread.h"
#include "systime.h"
#include "systty.h"
#include "syswait.h"
#include "termchar.h"
#include "termhooks.h"
#include "termopts.h"
#include "thread.h"
#include "tparam.h"
#include "unexec.h"
#ifdef HAVE_X_WINDOWS
# include "widget.h"
# include "widgetprv.h"
# include "xsettings.h"
#endif
#include "window.h"
#include "xgselect.h"

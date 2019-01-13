#ifndef EMACS_CALLPROC_H
#define EMACS_CALLPROC_H

#include "lisp.h"

INLINE_HEADER_BEGIN

extern int create_temp_file (ptrdiff_t nargs, Lisp_Object *args,
			     Lisp_Object *filename_string_ptr);

INLINE_HEADER_END

#endif

# Rust + Emacs

An experiment in building porting some Emacs C to Rust.

GPLv3, just like all Emacs code.

## Building

```
$ cd rust_src
$ cargo build --release
$ cd ..
$ ./autogen.sh
$ ./configure
```

Modify `src/Makefile` to read:

``` makefile
LIBS_SYSTEM=-L../rust_src/target/release -lremacs -ldl
```

Then compile Emacs:

```
$ make
```

## Understanding Emacs macros:

Define a little file, e.g.

``` c
#include "lisp.h"

DEFUN ("return-t", Freturn_t, Sreturn_t, 0, 0, 0,
       doc: /* Return t unconditionally.  */)
    ()
{
    return Qt;
}
```

Then expand it with GCC:

```
$ cd /path/to/remacs
$ gcc -Ilib -E src/dummy.c > dummy_exp.c
```

## TODOC

* Overriding git hooks (just delete them?)

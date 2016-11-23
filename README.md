# Rust + Emacs

An experiment in compiling Rust with C.

GPLv3, just like all Emacs code.

## Building

You need to use nightly rust.

```
$ rustup override set nightly
$ cargo build --release
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

* Building (as described in
  https://bawk.space/2016/10/06/c-to-rust.html )
* Overriding git hooks (just delete them?)

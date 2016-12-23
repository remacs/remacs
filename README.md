# Rust + Emacs [![Build Status](https://travis-ci.org/Wilfred/remacs.svg?branch=master)](https://travis-ci.org/Wilfred/remacs)

An experiment in building porting some Emacs C to Rust.

This codebase is based on the emacs 25.1 tag in git, plus commits to
add some Rust!

GPLv3, just like all Emacs code.

## Why Emacs?

TODO

## Why Rust?

TODO

## Building Remacs

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

You can then run your shiny new Remacs:

```
# Using -q to ignore your .emacs.d, so Remacs starts up quickly
$ src/emacs -q
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

## Contributing

Pull requests welcome, no copyright assignment required. This project is under the
[Rust code of conduct](https://www.rust-lang.org/en-US/conduct.html).

## Help Needed

TODO -- interesting bugs, first topics, ideas

## TODOC

* Overriding git hooks (just delete them?)

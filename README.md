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
$ cargo build
$ cd ..
$ ./autogen.sh
$ ./configure
```

Modify `src/Makefile` to read:

``` makefile
LIBS_SYSTEM=-L../rust_src/target/debug -lremacs -ldl
```

Then compile Emacs:

```
$ make
```

You can then run your shiny new Remacs:

```
# Using -q to ignore your .emacs.d, so Remacs starts up quickly.
# RUST_BACKTRACE is optional, but useful if your instance crashes.
$ RUST_BACKTRACE=1 src/emacs -q
```

### Release builds

As above, but invoke Cargo with:

``` bash
$ cargo build --release
```

and modify `src/Makefile` to:

``` makefile
LIBS_SYSTEM=-L../rust_src/target/release -lremacs -ldl
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

There's lots to do!

Easy tasks:

* Find a small function in lisp.h and write an equivalent in lisp.rs.
* Improve our unit tests. Currently we're passing `Qnil` to test
  functions, which isn't very useful.
* Add docstrings to public functions in lisp.rs.
* Tidy up messy Rust that's been translated directly from C. Run
  `rustfmt`, add or rename internal variables, run `clippy`, and so
  on.
* Fix the makefile to recompile with cargo and rebuild temacs when the
  Rust source changes.

Medium tasks:

* Choose an elisp function you like, and port it to rust. Look at
  `rust-mod` for an example.
* Expand our Travis configuration to do a complete Emacs build,
  including the C code.
* Expand our Travis configuration to run 'make check', so we know
  remacs passes Emacs' internal test suite.

Big tasks:

* Find equivalent Rust libraries for parts of Emacs, and replace all
  the relevant C code. Rust has great libraries for regular
  expressions, GUI, terminal UI, managing processes, amongst others.

## TODOC

* Overriding git hooks (just delete them?)

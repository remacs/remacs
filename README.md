# Rust :heart: Emacs
[![Build Status](https://travis-ci.org/Wilfred/remacs.svg?branch=master)](https://travis-ci.org/Wilfred/remacs)

A community-driven port of Emacs to Rust.

GPLv3 license.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Rust :heart: Emacs](#rust-heart-emacs)
    - [Why Emacs?](#why-emacs)
    - [Why Rust?](#why-rust)
    - [Why A Fork?](#why-a-fork)
    - [Design Goals](#design-goals)
    - [Non-Design Goals](#non-design-goals)
    - [Building Remacs](#building-remacs)
        - [Release builds](#release-builds)
        - [Rustdoc builds](#rustdoc-builds)
    - [Porting C Functions To Rust: Walkthrough](#porting-c-functions-to-rust-walkthrough)
    - [Contributing](#contributing)
    - [Help Needed](#help-needed)
    - [Rust Porting Tips](#rust-porting-tips)
        - [C Functions](#c-functions)
        - [C Macros](#c-macros)
        - [Assertions](#assertions)

<!-- markdown-toc end -->

## Why Emacs?

Emacs will change how you think about programming.

Emacs is **totally introspectable**. You can always find out 'what
code runs when I press this button?'.

Emacs is an **incremental programming environment**. There's no
edit-compile-run cycle. There isn't even an edit-run cycle. You can
execute snippets of code and gradually turn them into a finished
project. There's no distinction between your editor and your
interpreter.

Emacs is a **mutable environment**. You can set variables, tweak
functions with advice, or redefine entire functions. Nothing is
off-limits.

Emacs **provides functionality without applications**. Rather than
separate applications, functionality is all integrated into your Emacs
instance. Amazingly, this works. Ever wanted to use the same snippet
tool for writing C++ classes as well as emails?

Emacs is full of **incredible software concepts that haven't hit the
mainstream yet**. For example:

* Many platforms have a single item clipboard. Emacs has an **infinite
  clipboard**.
* If you undo a change, and then continue editing, you can't redo the
  original change. Emacs allows **undoing to any historical state**, even
  allowing tree-based exploration of history.
* Emacs supports a **reverse variable search**: you can find variables
  with a given value.
* You can perform **structural editing** of code, allowing you to make
  changes without breaking syntax. This works for lisps (paredit) and
  non-lisps (smartparens).
* Many applications use a modal GUI: for example, you can't do other
  edits during a find-and-replace operation. Emacs provides
  **recursive editing** that allow you to suspend what you're
  currently doing, perform other edits, then continue the original
  task.

Emacs has a **documentation culture**. Emacs includes a usage manual,
a lisp programming manual, pervasive docstrings and even an
interactive tutorial.

Emacs has **a broad ecosystem**. If you want to edit code in a
niche language, there's probably an Emacs package for it.

Emacs doesn't have a monopoly on good ideas, and there are other great
tools out there. Nonetheless, we believe the [Emacs learning curve pays
off](https://i.stack.imgur.com/7Cu9Z.jpg).

## Why Rust?

Rust is a great alternative to C.

Rust has **a fantastic learning curve**. The documentation is superb,
and the community is very helpful if you get stuck.

Rust has **excellent tooling**. The compiler makes great suggestions,
the unit test framework is good, and `rustfmt` helps ensure formatting
is beautiful and consistent.

The Rust **packaging story is excellent**. It's easy to reuse
the great libraries available, and just as easy to factor out code for
the benefit of others. We can replace entire C files in Emacs with
well-maintained Rust libraries.

Code written in Rust **easily interoperates with C**. This means we
can **port to Rust incrementally**, and having a working Emacs at each
step of the process.

Rust provides **many compile-time checks**, making it much easier to write
fast, correct code (even when using multithreading). This also makes
it much easier for newcomers to contribute.

Give it a try. We think you'll like it.

## Why A Fork?

Emacs is a widely used tool with a long history, broad platform
support and strong backward compatibility requirements. The core team
is understandably cautious in making far-reaching changes.

Forking is a longstanding tradition in the Emacs community for trying
different approaches. Notable Emacs forks include [XEmacs](http://www.xemacs.org/),
[Guile Emacs](https://www.emacswiki.org/emacs/GuileEmacs),
and [emacs-jit](https://github.com/burtonsamograd/emacs-jit).

There have also been separate elisp implementations, such as
[Deuce](https://github.com/hraberg/deuce),
[JEmacs](http://jemacs.sourceforge.net/) and
[El Compilador](https://github.com/tromey/el-compilador).

By forking, we can **explore new development approaches**. We can
use a pull request workflow with integrated CI.

We can **drop legacy platforms and compilers**. Remacs will never run
on MS-DOS, and that's OK.

There's a difference between **the idea of Emacs** and the **current
implementation of Emacs**. Forking allows us to explore being even
more Emacs-y.

## Design Goals

**Compatibility**: Remacs should not break existing elisp code, and
ideally provide the same FFI too.

**Similar naming conventions**: Code in Remacs should use the same
naming conventions for elisp namespaces, to make translation
straightforward.

This means that an elisp function `do-stuff` will have a corresponding
Rust function `Fdo_stuff`, and a declaration struct `Sdo_stuff`. A
lisp variable `do-stuff` will have a Rust variable `Vdo_stuff` and a
symbol `'do-stuff` will have a Rust variaable `Qdo_stuff`.

Otherwise, we follow Rust naming conventions, with docstrings noting
equivalent functions or macros in C. When incrementally porting, we
may define Rust functions with the same name as their C predecessors.

**Leverage Rust itself**: Remacs should make best use of Rust to
ensure code is robust and performant.

**Leverage the Rust ecosystem**: Remacs should use existing Rust
crates wherever possible, and create new, separate crates where our
code could benefit others.

**Great docs**: Emacs has excellent documentation, Remacs should be no
different.

## Non-Design Goals

**`etags`**: The
[universal ctags project](https://github.com/universal-ctags/ctags)
supports a wider range of languages and we recommend it instead.

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

### Rustdoc builds

You can use rustdoc to generate API docs:

``` bash
# http://stackoverflow.com/a/39374515/509706
$ cargo rustdoc -- \
    --no-defaults \
    --passes strip-hidden \
    --passes collapse-docs \
    --passes unindent-comments \
    --passes strip-priv-imports
```

You can then open these docs with:

``` bash
$ cargo doc --open
```

## Porting C Functions To Rust: Walkthrough

Let's look at porting `numberp` to Rust.

First, make sure you have configured and built Remacs on your system.

Emacs C uses a lot of macros, so it's useful to look at the expanded
version of the code.

Define a little file `src/dummy.c` with the C source of `numberp`, along
with the `lisp.h` header file:

``` c
#include "lisp.h"

DEFUN ("numberp", Fnumberp, Snumberp, 1, 1, 0,
       doc: /* Return t if OBJECT is a number (floating point or integer).  */
       attributes: const)
  (Lisp_Object object)
{
  if (NUMBERP (object))
    return Qt;
  else
    return Qnil;
}
```

Then expand it with GCC:

```
$ cd /path/to/remacs
$ gcc -Ilib -E src/dummy.c > dummy_exp.c
```

This gives us a file that ends with:

``` c
static struct Lisp_Subr 
# 3 "src/dummy.c" 3 4
_Alignas 
# 3 "src/dummy.c"
(8) Snumberp = { { PVEC_SUBR << PSEUDOVECTOR_AREA_BITS }, { .a1 = Fnumberp }, 1, 1, "numberp", 0, 0}; Lisp_Object Fnumberp


  (Lisp_Object object)
{
  if (NUMBERP (object))
    return Qt;
  else
    return builtin_lisp_symbol (0);
}
```

We can see we need to define a `Snumberp` and a `Fnumberp`. `Qt` and
`Qnil` are already defined in `lisp.rs`, so we can simply write:

``` rust
// This is the function that gets called when 
// we call numberp in elisp.
fn Fnumberp(object: LispObject) -> LispObject {
    if lisp::SYMBOLP(object) {
        unsafe {
            Qt
        }
    } else {
        Qnil
    }
}

// This defines a built-in function in elisp.
lazy_static! {
    static ref Snumberp: LispSubr = LispSubr {
        header: VectorLikeHeader {
            size: ((PvecType::PVEC_SUBR as libc::c_int) <<
                   PSEUDOVECTOR_AREA_BITS) as libc::ptrdiff_t,
        },
        function: (Fnumberp as *const libc::c_void),
        // Our elisp function takes exactly one argument.
        min_args: 1,
        max_args: 1,
        // The name of our function in elisp.
        symbol_name: ("numberp\0".as_ptr()) as *const c_char,
        // Our function is not interactive.
        intspec: ptr::null(),
        // Docstring. The last line ensures that *Help* shows the
        // correct calling convention
        doc: ("Return t if OBJECT is a number (floating point or integer).

(fn OBJECT)\0".as_ptr()) as *const c_char,
    };
}
```

Finally, we need to delete the old C definition and call `defsubr`
inside `rust_init_syms`:

``` rust
pub extern "C" fn rust_init_syms() {
    unsafe {
        // ...
        defsubr(&*yourmodule::Snumberp);
    }
}
```

You're done! Compile Remacs, try your function with `M-x ielm`, and
open a pull request. Fame and glory await!

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
* Fix 'make clean' to delete .elc files, otherwise you don't notice
  some bugs until Travis runs.
* Fix the makefile to output a binary called `remacs`
* Update the Remacs welcome screen to say Remacs
* Add Rust-level unit tests to elisp functions defined in lib.rs.

Medium tasks:

* Choose an elisp function you like, and port it to rust. Look at
  `rust-mod` for an example.
* Teach `describe-function` to find functions defined in Rust.
* Expand our Travis configuration to run 'make check', so we know
  remacs passes Emacs' internal test suite.
* Expand our Travis configuration to ensure that Rust code has been
  formatted with rustfmt
* Set up bors/homu.
* Set up a badge tracking pub struct/function coverage using
  cargo-doc-coverage.
* Search the Rust source code for `TODO` comments and fix them.
* Teach Emacs how to jump to definition for Rust functions.

Big tasks:

* Find equivalent Rust libraries for parts of Emacs, and replace all
  the relevant C code. Rust has great libraries for regular
  expressions, GUI, terminal UI, managing processes, amongst others.
* Change the elisp float representation to use
  [nan boxing](https://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations)
  rather than allocating floats on the heap.

## Rust Porting Tips

### C Functions

When writing a Rust version of a C function, give it the same name and
same arguments. If this isn't appropriate, docstrings should say the
equivalent C function to help future porters.

For example, `make_natnum` mentions that it can be used
in place of `XSETFASTINT`.

### C Macros

For C macros, we try to define a fairly equivalent Rust
function. The docstring should mention the original macro name.

Since the Rust function is not a drop-in replacement, we prefer Rust
naming conventions for the new function.

For the checked arithmetic macros (`INT_ADD_WRAPV`,
`INT_MULTIPLY_WRAPV` and so on), you can simply use `.checked_add`,
`.checked_mul` from the Rust stdlib.

### Assertions

`eassert` in Emacs C should be `debug_assert!` in Rust.

`emacs_abort()` in Emacs C should be `panic!("reason for panicking")`
in Rust.

# Rust :heart: Emacs

[![Join the chat at https://gitter.im/remacs-discuss/Lobby](https://badges.gitter.im/remacs-discuss/Lobby.svg)](https://gitter.im/remacs-discuss/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/Wilfred/remacs.svg?branch=master)](https://travis-ci.org/Wilfred/remacs)

A community-driven port of [Emacs](https://www.gnu.org/software/emacs/) to [Rust](https://www.rust-lang.org).

GPLv3 license.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Rust :heart: Emacs](#rust-heart-emacs)
    - [Why Emacs?](#why-emacs)
    - [Why Rust?](#why-rust)
    - [Why A Fork?](#why-a-fork)
    - [Getting Started](#getting-started)
        - [Requirements](#requirements)
        - [Building Remacs](#building-remacs)
        - [Running Remacs](#running-remacs)
        - [Rustdoc builds](#rustdoc-builds)
    - [Porting Elisp Primitive Functions: Walkthrough](#porting-elisp-primitive-functions-walkthrough)
        - [Porting Widely Used C Functions](#porting-widely-used-c-functions)
    - [Design Goals](#design-goals)
    - [Non-Design Goals](#non-design-goals)
    - [Contributing](#contributing)
    - [Help Needed](#help-needed)
    - [Rust Porting Tips](#rust-porting-tips)
        - [C Functions](#c-functions)
        - [C Macros](#c-macros)
        - [Assertions](#assertions)
        - [Safety](#safety)

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

## Getting Started

### Requirements

1. You will need
   [Rust installed](https://www.rust-lang.org/en-US/install.html). Remacs
   uses unstable Rust features, so you will need to use nightly.

        rustup install nightly
        cd /path/to/remacs
        rustup override set nightly

2. You will need a C compiler and toolchain. On Linux, you can do
   something like `apt-get install build-essential automake`. On
   macOS, you'll need Xcode.

3. You will need some C libraries. On Linux, you can install
   everything you need with:

        apt-get install texinfo libjpeg-dev libtiff-dev \
          libgif-dev libxpm-dev libgtk-3-dev libgnutls-dev \
          libncurses5-dev libxml2-dev

    On macOS, you'll need libxml2 (via `xcode-select --install`) and
    gnutls (via `brew install gnutls`).

#### Dockerized development environment

If you don't want to bother with the above setup you can use the provided docker environment.  Make sure you have [docker](https://www.docker.com/) 1.12+ and [docker-compose](https://github.com/docker/compose) 1.8+ available.

To spin up the environment run

``` shell
docker-compose up -d
```

First time you run this command docker will build the image.  After that any subsequent startups will happen in less than a second.

The working directory with remacs will be mounted under the same path in the container so editing the files on your host machine will automatically be reflected inside the container.   To build remacs use the steps from [Building Remacs](#building-remacs) prefixed with `docker-compose exec remacs`, this will ensure the commands are executed inside the container.

### Building Remacs

```
$ ./autogen.sh
$ ./configure --enable-rust-debug
$ make
```

For a release build, don't pass `--enable-rust-debug`.

The Makefile obeys cargo's RUSTFLAGS variable and additional options
can be passed to cargo with CARGO_FLAGS.

For example:

``` bash
$ make CARGO_FLAGS="-vv" RUSTFLAGS="-Zunstable-options --pretty"
```

### Running Remacs

You can now run your shiny new Remacs build!

```bash
# Using -q to ignore your .emacs.d, so Remacs starts up quickly.
# RUST_BACKTRACE is optional, but useful if your instance crashes.
$ RUST_BACKTRACE=1 src/remacs -q
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

## Porting Elisp Primitive Functions: Walkthrough

Let's look at porting `numberp` to Rust.

First, make sure you have configured and built Remacs on your
system. You'll probably want to generate TAGS too, so you can jump to
definitions of C functions.

This is the definition of `numberp`:

``` c
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

The `DEFUN` macro, in addition to defining a function `Fnumberp`, also
creates a static struct `Snumberp` that describes the function for Emacs'
Lisp interpreter.

In Rust, we define a `numberp` function that does the actual work then use
an attribute (implemented as a procedural macro) named `lisp_fn` that
handles these definitions for us:

``` rust
// This is the function that gets called when
// we call numberp in elisp.
//
// `lisp_fn` defines a wrapper function that calls numberp with
// LispObject values. It also declares a struct that we can pass to
// defsubr so the elisp interpreter knows about this function.

/// Return t if OBJECT is a number.
#[lisp_fn]
fn numberp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_number())
}
```

Due to an issue with procedural macros (#263) `lisp_fn` will make all warnings
and errors appear to be on its line instead of on the real line of Rust code.
The easy work around is to comment out `lisp_fn` until the compile succeeds
then enable it to do a final build and begin testing.

The elisp name of the function is derived from the Rust name, with
underscores replaced by hyphens.  If that is not possible (like for
the function `+`), you can give an elisp name as an argument to
`lisp_fn`, like `#[lisp_fn(name = "+")]`.

Optional arguments are also possible: to make the minimum number of
arguments from elisp different from the number of Rust arguments,
pass a `min = "n"` argument.

The docstring of the function should be the same as the docstring
in the C code.  (Don't wonder about it being a comment there, Emacs
has some magic that extracts it into a separate file.)

Finally, delete the old C definition.

You're done! Compile Remacs, try your function with `M-x ielm`, and
open a pull request. Fame and glory await!

### Porting Widely Used C Functions

If your Rust function replaces a C function that is used elsewhere in
the C codebase, it needs to be exported. If the function is not a Lisp
function (i.e. doesn't use the `#[lisp_fn]` macro), you need to
manually mark it as `#[no_mangle]` and `extern "C"` to be exported
with the correct ABI.

### Source code style guide

In order to pass Travis checks on pull requests, the source has to
be formatted according to the default style of `rustfmt-nightly`, version 0.2.9.
To do that, install `rustfmt`:

```
$ cargo install rustfmt-nightly
```

Please note that this is not the old `-nightly` version of the
`rustfmt` crate, 0.9.0. It is a different crate with a lower version
number which is in fact newer. See
https://users.rust-lang.org/t/rustfmt-releases/11357 and the linked
blog post for details. In short, `rustfmt` is being written with a new
API which only supports nightly at the moment and the split crate
situation will be resolved eventually.

Then you can run this in the checkout root to reformat all Rust code:

```
$ make rustfmt
```

### Running tests

Run elisp and Rust tests in toplevel directory. If run in a subdirectory, 
only run the tests in that directory.

* `make check`
  Run all tests as defined in the directory. Expensive tests are
  suppressed. The result of the tests for <filename>.el is stored in
  <filename>.log.

* `make check-maybe`
  Like "make check", but run only the tests for files that have been 
  modified since the last build.

## Design Goals

**Compatibility**: Remacs should not break existing elisp code, and
ideally provide the same FFI too.

**Similar naming conventions**: Code in Remacs should use the same
naming conventions for elisp namespaces, to make translation
straightforward.

This means that an elisp function `do-stuff` will have a corresponding
Rust function `Fdo_stuff`, and a declaration struct `Sdo_stuff`. A
lisp variable `do-stuff` will have a Rust variable `Vdo_stuff` and a
symbol `'do-stuff` will have a Rust variable `Qdo_stuff`.

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

## Contributing

Pull requests welcome, no copyright assignment required. This project is under the
[Rust code of conduct](https://www.rust-lang.org/en-US/conduct.html).

## Help Needed

There's lots to do! We keep a list of low hanging fruit here so you can easily choose
one. If you do, please open a new issue to keep track of the task and link to it.

Easy tasks:

- [ ] Find a small function in lisp.h and write an equivalent in lisp.rs.
- [ ] Add Rust unit tests. Currently we're relying on Emacs' own
  test suite.
- [ ] Add docstrings to public functions in lisp.rs.
- [ ] Tidy up messy Rust that's been translated directly from C. Run
  `rustfmt`, add or rename internal variables, run `clippy`, and so on.
- [ ] Add Rust-level unit tests to elisp functions defined in lib.rs.

Medium tasks:

- [ ] Choose an elisp function you like, and port it to rust. Look at
  `rust-mod` for an example.
- [x] Teach `describe-function` to find functions defined in Rust.
- [ ] Expand our Travis configuration to run 'make check', so we know
  remacs passes Emacs' internal test suite.
- [x] Expand our Travis configuration to ensure that Rust code has been
  formatted with rustfmt
- [ ] Set up bors/homu.
- [ ] Set up a badge tracking pub struct/function coverage using
  cargo-doc-coverage.
- [ ] Search the Rust source code for `TODO` comments and fix them.

Big tasks:

- [ ] Find equivalent Rust libraries for parts of Emacs, and replace all
  the relevant C code. Rust has great libraries for regular
  expressions, GUI, terminal UI, managing processes, amongst others.
- [ ] Change the elisp float representation to use
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

### Safety

`LispObject` values may represent pointers, so the usual safety
concerns of raw pointers apply.

If you can break memory safety by passing a valid value to a function,
then it should be marked as `unsafe`. For example:

``` rust
// This function is unsafe because it's dereferencing the car
// of a cons cell. If `object` is not a cons cell, we'll dereference
// an invalid pointer.
unsafe fn XCAR(object: LispObject) -> LispObject {
    (*XCONS(object)).car
}

// This function is safe because it preserves the contract
// of XCAR: it only passes valid cons cells. We just use
// unsafe blocks instead.
fn car(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe {
            XCAR(object)
        }
    } else if NILP(object) {
        Qnil
    } else {
        unsafe {
            wrong_type_argument(Qlistp, object)
        }
    }
}
```

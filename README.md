# Rust :heart: Emacs

[![Join the chat at https://gitter.im/remacs-discuss/Lobby](https://badges.gitter.im/remacs-discuss/Lobby.svg)](https://gitter.im/remacs-discuss/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/remacs/remacs.svg?branch=master)](https://travis-ci.org/remacs/remacs)

A community-driven port of [Emacs](https://www.gnu.org/software/emacs/) to [Rust](https://www.rust-lang.org).

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Why Emacs?](#why-emacs)
- [Why Rust?](#why-rust)
- [Why A Fork?](#why-a-fork)
- [Getting Started](#getting-started)
    - [Requirements](#requirements)
    - [Dockerized development environment](#dockerized-development-environment)
    - [Building Remacs](#building-remacs)
    - [Running Remacs](#running-remacs)
- [Design Goals](#design-goals)
- [Progress](#progress)
- [Porting Elisp Primitive Functions](#porting-elisp-primitive-functions)
- [Contributing](#contributing)

<!-- markdown-toc end -->


# Why Emacs?

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

# Why Rust?

Rust is a great alternative to C.

Rust has **a fantastic learning curve**. [The documentation is superb](https://doc.rust-lang.org/),
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

# Why A Fork?

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

# Getting Started

## Requirements

1. You will need
   [Rust installed](https://www.rust-lang.org/en-US/install.html). 
   The file `rust-toolchain` indicates the version that gets installed.
   This happens automatically, so don't override the toolchain manually.
   IMPORTANT: Whenever the toolchain updates, you have to reinstall
   rustfmt manually.

2. You will need a C compiler and toolchain. On Linux, you can do
   something like:

        apt install build-essential automake clang libclang-dev

   On macOS, you'll need Xcode.

3. Linux:

        apt install texinfo libjpeg-dev libtiff-dev \
          libgif-dev libxpm-dev libgtk-3-dev libgnutls28-dev \
          libncurses5-dev libxml2-dev libxt-dev

   macOS:
   
        brew install gnutls texinfo autoconf
        
    To use the installed version of `makeinfo` instead of the built-in 
    (`/usr/bin/makeinfo`) one, you'll need to make sure `/usr/local/opt/texinfo/bin` 
    is before `/usr/bin` in `PATH`.
    Mojave install libxml2 headers with: `open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg`

## Dockerized development environment

If you don't want to bother with the above setup you can use the
provided Docker environment. Make sure you have
[docker](https://www.docker.com/) 1.12+ and
[docker-compose](https://github.com/docker/compose) 1.8+ available.

To spin up the environment run

``` shell
docker-compose up -d
```

The first time you run this command, Docker will build the image. After
that any subsequent startups will happen in less than a second. If
this command fails because of needing absolute paths, make sure to set
the PWD environment variable before calling the command like so:

```shell
PWD=$(pwd) docker-compose up -d
```

The working directory with remacs will be mounted under the same path
in the container so editing the files on your host machine will
automatically be reflected inside the container. To build remacs use
the steps from [Building Remacs](#building-remacs) prefixed with
`docker-compose exec remacs`, this will ensure the commands are
executed inside the container.

## Building Remacs

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
$ make CARGO_FLAGS="-vv" RUSTFLAGS="-Zunstable-options --cfg MARKER_DEBUG"
```

## Running Remacs

You can now run your shiny new Remacs build!

```bash
# Using -q to ignore your .emacs.d, so Remacs starts up quickly.
# RUST_BACKTRACE is optional, but useful if your instance crashes.
$ RUST_BACKTRACE=1 src/remacs -q
```

# Design Goals

**Compatibility**: Remacs should not break existing elisp code, and
ideally provide the same FFI too.

**Leverage Rust itself**: Remacs should make best use of Rust to
ensure code is robust and performant.

**Leverage the Rust ecosystem**: Remacs should use existing Rust
crates wherever possible, and create new, separate crates where our
code could benefit others.

**Great docs**: Emacs has excellent documentation, Remacs should be no
different.

# Progress

At this point we focus on porting lisp functions from C to Rust.
Currently there are 600 functions in Rust and 859 in C (March 2019).

We have a [progress section](https://github.com/Wilfred/remacs/wiki/Progress) in our wiki
and there's also a list of [long-term goals](https://github.com/Wilfred/remacs/projects/1) 
under projects.

# Porting Elisp Primitive Functions

The first thing to look at is the C implementation for the `atan` function. It takes an optional second argument, which makes it interesting. The complicated mathematical bits, on the other hand, are handled by the standard library. This allows us to focus on the porting process without getting distracted by the math.

The Lisp values we are given as arguments are tagged pointers; in this case they are pointers to doubles. The code has to check the tag and follow the pointer to retrieve the real values. Note that this code invokes a C macro (called `DEFUN`) that reduces some of the boilerplate. The macro declares a static variable called `Satan` that holds the metadata the Lisp compiler will need in order to successfully call this function, such as the docstring and the pointer to the `Fatan` function, which is what the C implementation is named:

``` c
DEFUN ("atan", Fatan, Satan, 1, 2, 0,
       doc: /* Return the inverse tangent of the arguments.
If only one argument Y is given, return the inverse tangent of Y.
If two arguments Y and X are given, return the inverse tangent of Y
divided by X, i.e. the angle in radians between the vector (X, Y)
and the x-axis.  */)
  (Lisp_Object y, Lisp_Object x)
{
  double d = extract_float (y);
  if (NILP (x))
    d = atan (d);
  else
    {
      double d2 = extract_float (x);
      d = atan2 (d, d2);
    }
  return make_float (d);
}
```

`extract_float` checks the tag (signalling an "invalid argument" error if it's not the tag for a double), and returns the actual value. `NILP` checks to see if the tag indicates that this is a null value, indicating that the user didn't supply a second argument at all.

Next take a look at the current Rust implementation. It must also take an optional argument, and it also invokes a (Rust) macro to reduce the boilerplate of declaring the static data for the function. However, it also takes care of all of the type conversions and checks that we need to do in order to handle the arguments and return value:

``` rust
/// Return the inverse tangent of the arguments.
/// If only one argument Y is given, return the inverse tangent of Y.
/// If two arguments Y and X are given, return the inverse tangent of Y
/// divided by X, i.e. the angle in radians between the vector (X, Y)
/// and the x-axis
#[lisp_fn(min = "1")]
pub fn atan(y: EmacsDouble, x: Option<EmacsDouble>) -> EmacsDouble {
    match x {
        None => y.atan(),
        Some(x) => y.atan2(x)
    }
}
```

You can see that we don't have to check to see if our arguments are of the correct type, the code generated by the `lisp_fn` macro does this for us. We also asked for the second argument to be an `Option<EmacsDouble>`. This is the Rust type for a value which is either a valid double or isn't specified at all. We use a match statement to handle both cases.

This code is so much better that it's hard to believe just how simple the implementation of the macro is. It just calls `.into()` on the arguments and the return value; the compiler does the rest when it dispatches this method call to the correct implementation.

# Contributing

Pull requests welcome, no copyright assignment required. This project is under the
[Rust code of conduct](https://www.rust-lang.org/policies/code-of-conduct).

There's lots to do! We keep a list of [low hanging fruit](https://github.com/Wilfred/remacs/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22) here so you can easily choose
one. You can find information in the [Porting cookbook](https://github.com/Wilfred/remacs/wiki/Porting-cookbook) or ask for help in our gitter channel.

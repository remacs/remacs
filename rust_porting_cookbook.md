# Intro

The goal of this document is to provide guidance for the developer when
porting code from Emacs C code to Rust.

## Porting Elisp Primitive Functions: Walk-through

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

/// Return t if OBJECT is a number (floating point or integer).
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

## Porting Widely Used C Functions

If a Rust function replaces a C function that is used elsewhere in
the C codebase, it needs to be exported. If the function is not a Lisp
function (i.e. doesn't use the `#[lisp_fn]` macro), you need to
manually mark it as `#[no_mangle]` and `extern "C"` to be exported
with the correct ABI. The build scripts handle the export automatically
however you need to maintain an entry in a C header to keep the compilation
happy.

## Source code style guide

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

## Running tests

Run elisp and Rust tests in toplevel directory. If run in a subdirectory,
only run the tests in that directory.

* `make check`
  Run all tests as defined in the directory. Expensive tests are
  suppressed. The result of the tests for <filename>.el is stored in
  <filename>.log.

* `make check-maybe`
  Like "make check", but run only the tests for files that have been
  modified since the last build.

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

# Recipes
## DEFUN
### Minimum args, maximum args, and intspec

`2,2,0` means "minimum of 2, maximum of 2, not interactive"
`1,2,0` means "minimum of 1, maximum of 2, not interactive"
`1,2,"b"` means "minimum of 1, maximum of 2, interactive, takes the name of a buffer"

(Explanation of interactive codes)[https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html]

`1,2,"i"` would look like this in Rust:

``` rust
#[lisp_fn(min = "1", intspec = "i")]
```

## Numbers
### Fixnum and Natnum

There are two types of Integers in Emacs Lisp -- Fixnums and Natnums. A fixnum
is equivalent to a i64 in Rust and a natnum is u64. In general, use a fixnum
unless there is a reason to use a natnum.

## Loops
### CAR

``` c
for (Lisp_Object tail = thing; CONSP (tail); tail = XCDR (tail))
{
    func(XCAR(tail));
}
```

becomes

``` rust
for i in thing.iter_cars() {
    func(i);
}
```

### CDR

``` c
for (Lisp_Object tail = thing; CONSP (tail); tail = XCDR (tail))
{
    func(XCAR(tail), XCDR(tail));
}
```

becomes

``` rust
for i in thing.iter_tails() {
    func(i.car(), i.cdr());
}
```

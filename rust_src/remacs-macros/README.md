remacs-macros
=============

# Attributes

## `lisp_fn`

This macro creates the necessary FFI functions and symbols
automatically.  It handles normal functions and functions that take an
arbitrary number of arguments (functions with `MANY` as the maximum
number of arguments on the C side)

It is used like this:

```rust
/// Return the same argument
#[lisp_fn(name = "same", c_name = "same", min = "1"]
fn same(obj: LispObject) -> LispObject {
    obj
}
```

Here the `name` argument specifies the **symbol name** that is going
to be use in Emacs Lisp, `c_name` specifies the name for the `Fsame`
and `Ssame` statics used in C, and `min` specifies the **minimum**
number of arguments that can be passed to this function, the
**maximum** number of arguments is calculated automatically from the
function signature.

All three of these arguments are optional, and have sane defaults.
Default for `name` is the Rust function name with `_` replaced by `-`.
Default for `c_name` is the Rust function name.  Default for `min` is
the number of Rust arguments, giving a function without optional
arguments.

In this example the attribute generates the `Fsame` function that is
going to be called in C, and the `Ssame` structure that holds the
function information. You still need to register the function with
`defsubr` to make it visible in Emacs Lisp. To make a function visible
to C you need to export it in the crate root (lib.rs) as follows:

```rust
use somemodule::Fsome;
```

### Functions with a dynamic number of arguments (`MANY`)

This attribute handles too the definition of functions that take an
arbitrary number of arguments, these functions can take an arbitrary
number of arguments, but you *still can* specify a `min` number of
arguments.

They are created as follows:

```rust
/// Returns the first argument.
#[lisp_fn(min = "1")]
fn first(args: &mut [LispObject]) -> LispObject {
    args[0]
}
```

### Example: Porting `numberp`

This is how the `numberp` function looks in C.

```c
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

Looking at it when can gather some information about it, for example its
name is "numberp" and it defines a `Fnumberp` function and a static
structure `Snumberp`, plus the function only takes one argument
`object`. With that in mind we can rewrite it now in Rust:

```rust
/// Return t if OBJECT is a number (floating point or integer).
#[lisp_fn]
fn numberp(object: LispObject) -> LispObject {
    LispObject::from_bool(object.is_number())
}
```

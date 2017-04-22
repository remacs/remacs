remacs-macros
=============

# Attributes

## `lisp_fn`
This macros creates the necessary functions and symbols automatically.
It handles `MANY` functions and normal functions too (support for `UNEVALLED`
needs to be done).

It is used like this:

```rust
/// Return the same argument
#[lisp_fn(name = "same", min = "1")]
fn same(obj: LispObject) -> LispObject {
    obj
}
```

Where the `name` argument specifies the **symbol name** that is going to be use in Emacs Lisp, `min` specifies the **minimum** number of arguments that can be passed to this function.

In this example the attribute generates the `Fsame` function that is going to b called in **C**, and the `Ssame` structure that holds the function information. You still need to register the function with `defsubr` to make it visible in Emacs Lisp. To make a function visible to C you need to export it in the crate root (lib.rs) as follows:

```rust
use somemodule::Fsome;
```

### `MANY` functions

This attributes handles the definitions of `MANY` functions, these functions can take an arbitrary number of arguments, but you *still can* specify a `min` number of arguments.

They are created as follows:

```rust
/// Returns the first argument.
#[lisp_fn(name = "first", min = "1")]
fn first(args: &mut [LispObject]) -> LispObject {
    args[0]
}
```

## `lisp_doc`
This is only a place holder, it does nothing but might have special meaning in the future.

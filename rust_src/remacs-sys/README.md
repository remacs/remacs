# remacs-sys

`remacs-sys` is Rust library containing all [Foreign Function Interface (FFI)](https://doc.rust-lang.org/book/first-edition/ffi.html) declarations of structs and functions that are defined in C. Then, they can be used in the Rust code. C implementations are in `remacs/src`.

## Example: Use `make_float` C function in Rust

First, let's have a look how `make_float` is defined historically in the C code. Then, how one can re-use it in Rust.

### On the C side

`make_float` is regular C function . The declaration can be found in the header file `remacs/src/lisp.h`:

```c
extern Lisp_Object make_float (double);
```

The definition is in source file `remacs/src/alloc.c`:

```c
Lisp_Object
make_float (double float_value)
{
  // ...
}
```

### On the Rust side

In order to use `make_float` in the Rust code, the "foreign function" needs to be declared in Rust by adding the signature in `remacs/rust_src/remacs-sys/lib.rs`:

```rust
extern "C" {
    pub fn make_float(float_value: c_double) -> Lisp_Object;
}
```

There are two remaining unknowns:
1. `c_double`
2. `Lisp_Object`

First, `c_double` is a type representing a C `double`. It is defined in the [`libc`](https://doc.rust-lang.org/book/first-edition/ffi.html#a-note-about-libc) crate and imported in Remacs as follows:

```rust
// ...
extern crate libc;

use libc::{c_double, /* ... */ };
// ...
```

Second, `Lisp_Object` is an alias to `EmacsInt`:

```rust
pub type Lisp_Object = EmacsInt;
```

The later is a type generated at build time (cf. [build.rs](http://doc.crates.io/build-script.html)) to mimic how they are in C:

Now everything is defined, `make_float` can be used in Rust. For example in `remacs/rust_src/src/lisp.rs`:

```rust
    #[inline]
    pub fn from_float(v: EmacsDouble) -> LispObject {
        LispObject::from_raw(unsafe { make_float(v) })
    }
```

Note, foreign functions are considered unsafe so the call is wrapped in a `unsafe` block.

## Example: `Lisp_Hash_Table` struct ported to Rust

Structures can be ported from C to rust. Let's have a look at `Lisp_Hash_Table`.

### On the C side

Like the function `make_float`, it is historically defined in C. It is in `remacs/src/lisp.h`:

```c
struct Lisp_Hash_Table
{
  struct vectorlike_header header;
  // ...
}
```

### On the Rust side

Similar to `make_float`, the Rust implementation of the struct should be in `remacs/rust_src/remacs-sys/lib.rs`:

```rust
#[repr(C)]
pub struct Lisp_Hash_Table {
    pub header: Lisp_Vectorlike_Header,
    // ...
}
```

All types of `Lisp_Hash_Table` have to be defined as well. Then, under the hood, C and Rust use different data layout. [`repr(C)`](https://doc.rust-lang.org/nomicon/other-reprs.html#reprc) is used to ensure the compatibility between the two.

Finally, `Lisp_Hash_Table` struct can be used in Rust and abso be passed to C functions. For example, in `remacs/rust_src/src/hashtable.rs`:

```rust
pub fn allocate() -> LispHashTableRef {
     let vec_ptr =
         allocate_pseudovector!(Lisp_Hash_Table, count, PseudovecType::PVEC_HASH_TABLE);
     LispHashTableRef::new(vec_ptr)
}
```

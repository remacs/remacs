remacs-lib

This crate is used to house rust replacements for the gnulib code included with the emacs codebase. This includes the c files in lib/ and lib-src/. The code in this module should be:

* Generic in functionality (not specific to elisp)
* Self contained in nature (any rust program could use the code contained in this module)

Conventions for code in this crate:
When porting an existing c function, we should try to keep all guarantees that function makes according to it's man page. For example, the rust implementation of mkostemp (called rust_make_temp), has the interface and errno guarantees described in https://linux.die.net/man/3/mkostemp.

Generally, it is advised that all functionality of these functions should be in idomatic rust, dealing with rust standard data structures. For example, if you are porting a function that is something like:

```
// Returns a File Descriptor based on values of x and y
int foo(int x, const char* y);
```

You should write something like the following:
```
pub fn good_name_for_foo(x: i32, y: &str) -> Result<File> {
    // ... 
}

pub extern "C" fn rust_foo(x: libc::c_int, y: *const libc::c_char) {
    // Convert x and y into rust equivalents
    let valueX = ...
    let valueY = ...
    match good_name_for_foo(valueX, valueY) {
        Ok(result) => {
            // Deal with OK result according to C interface
        }
        Err(err) => {
            // Deal with Err result according to C interface, set errno appropriatly, etc.
        }
    }
}

```

Here `good_name_for_foo` can be used in any rust codebase, without having to deal with any C "baggage". `rust_foo` is essentially a glue function, who's only responsibility is to convert our idomatic rust function's results to their C equivalent. It should contain no real logic besides what is needed for that conversion. 
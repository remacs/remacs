Thank you for contributing to Remacs!

## Getting Started

* Consider filing an issue or leaving a note on an existing issue, to
  avoid overlapping with someone else.
* Work in progress PRs are welcome, if you want review or if you get
  stuck. Just put [WIP] in the PR title, and remove [WIP] when you're
  happy. There is also a "help wanted" label.

## Writing a great Pull Request (PR)

* Follow the existing style for new Rust code. Notes about style
  can be found in the section on [Remacs style](#remacs-style).
* Check for Rust compiler warnings, as CI won't go green if there
  are warnings.
* Format your code with
  [rustfmt-nightly](https://github.com/rust-lang-nursery/rustfmt).
  You can learn the exact version we use at any time by looking at the
  .travis.yml file.
* Add docstrings to your Rust functions `/// This function does ...`
* _Really_ great PRs include tests. See
  [Writing Tests](#writing-tests) for more information.

## Writing tests

It is not always possible to write unit tests for Remacs's Rust
code. We do have a mocking library available to help if it needs to
call C code with this. See `rust_src/src/strings.rs` for examples. But
mocking may not cover all situations. Don't worry if it's not possible
to write unit tests for your Rust code yet (this is common).

What you can do is write functional tests in elisp.  The elisp tests
have file names based on the Emacs source they are testing. So if you
are porting code from `src/keyboard.c` the companion tests would be
found in `test/src/keyboard-tests.el`.  Look there first and then the
rest of the test files to see if the function you plan to port is
already covered. If not, it is a good idea to write tests first before
you begin porting C code. This gives you the advantage of starting
with a known quantity. If the tests fail while the code is still
written in C you know you likely have a bug in your test. If the test
works after you have ported the code to Rust you have good reason to
believe the port is complete.

Once the code is ported to Rust, the tests should be associated with
the Rust code by placing them in
`test/rust_src/src/filename-tests.el`. To continue the example above
if you ported a function from keyboard.c to keyboard.rs the tests
would belong in `test/rust_src/src/keyboard-tests.el`. If you wrote
the tests in `test/src/` while porting just copy the code over to the
equivalent file in `test/rust_src/src` before submitting the PR.

## Getting your PRs merged

* Your PR needs to be reviewed by a collaborator. See the !(CONTRIBUTORS)
  file for a current list.
* The build needs to be passing.

## CI

Remacs uses Appveyor to run the CI for Windows and Travis for both
Linux and OSX. Each PR must pass the automated tests. If a PR needs
changes or additions to the CI configuration, include `.appveyor.bat`
or `.travis.yml` in the PR and those modifications will be tested
instead of the currently checked in version.

If for some reason one of the Travis stages needs to be muted this can be
accomplished by adding `TRAVIS_ALLOW_FAILURE=1` to the `env` section of that
stage and then adding the following stanza at the top level of the `jobs`
stanza:

    allow_failures:
      - env: TRAVIS_ALLOW_FAILURE=1

There is nothing magic about the above variable. Any name can be used.

## Remacs style

One of the indicators of good code is a consistent style. When
updating the Rust sources contributors need to follow these
guidelines:

* Be consistent with the existing code. This means use similar
  variable and function names. The Rust source does not follow elisp
  naming conventions. For example, a function is called `is_foo`
  instead of `foop`.
* When porting code from C, use the Rust language features and style.
  Use iterators instead of a counting for loop. Check a pointer with
  `is_null` instead of using `== 0`.
* Imports are grouped with an empty line between groups. This lets
  rustfmt keep them ordered.
  The grouping is:
    1. Rust crates
    1. external crates
    1. remacs crates
    1. modules within the current crate
* For modules where we import many items, we often to do an import for
  each major type. For instance, in most modules the `remacs-sys` module
  is imported from several times. Once for all of the `Qfoo` names, once
  for types, and again for functions. Doing this helps cut down on the
  amount of merge conflicts and makes the conflicts easier to sort out.

## Pitfalls

Below are a list of common mistakes we catch in code review.

* Lisp_Object v. LispObject. The name `Lisp_Object` is from C code and
  it represents an `int`. It will be a 64bit number on 64bit systems
  and 32bit on 32bit systems. `LispObject` is a Rust wrapper which
  provides methods. C functions need the C representation. Rust
  functions usually take `LispObject` but sometimes they use
  `Lisp_Object` instead.
* Prefer `EmacsInt::from(foo)` over `foo as EmacsInt`. The compiler will
  be able to warn you if the conversion is lossy.

Thank you for contributing to Remacs!

## Getting Started

* Consider filing an issue or leaving a note on an existing issue, to
  avoid overlapping with someone else.
* Work in progress PRs are welcome, if you want review or if you get
  stuck. Just put [WIP] in the PR title, and remove [WIP] when you're
  happy. There is also a "help wanted" label.

## Writing a great Pull Request (PR)

* Check for Rust compiler warnings, as Travis won't go green if there
  are warnings.
* Format your code with
  [rustfmt-nightly](https://github.com/rust-lang-nursery/rustfmt).
* Add docstrings to your Rust functions `/// This function does ...`
* _Really_ great PRs include tests. Don't worry if it's not
  possible to write tests for your code yet (this is common).

## Getting your PRs merged

* Your PR needs to be reviewed by a collaborator. The following users
  can do this for you: @Wilfred @c-nixon @CrLF0710 @jeandudey @birkenfeld
* The build needs to be passing.

## Travis CI

Remacs uses Travis to run the CI for both Linux and OSX. Each PR must pass
the automated tests. If a PR needs changes or additions to the Travis
configuration, include `.travis.yml` in the PR and those modifications
will be tested instead of the currently checked in version.

If for some reason one of the Travis stages needs to be muted this can be
accomplished by adding `TRAVIS_ALLOW_FAILURE=1` to the `env` section of that
stage and then adding the following stanza at the top level of the `jobs`
stanza:

    allow_failures:
      - env: TRAVIS_ALLOW_FAILURE=1

There is nothing magic about the above variable. Any name can be used.

# Elisp Compatibility

Remacs should generally behave identically to GNU Emacs. If you find a
difference, please
[file a bug](https://github.com/Wilfred/remacs/issues/new).

## Detecting Remacs

You can detect if your current Emacs instance is Remacs by running the
following code:

``` emacs-lisp
(string= invocation-name "remacs")
```

This will return `t` in a Remacs instance.

# Platform differences

Remacs does not support MS-DOS.

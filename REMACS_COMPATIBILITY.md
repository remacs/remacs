# Elisp Compatibility

Remacs should generally behave identically to GNU Emacs. If you find a
difference, please
[file a bug](https://github.com/Wilfred/remacs/issues/new).

## Differences

   - `base64-encode-string` and `base64-decode-string` does not fail
     on multibyte strings.
   - `base64-encode-region` and `base64-decode-region` does not fail
     on multibyte regions.

## Detecting Remacs

You can detect if your current Emacs instance is Remacs by running the
following code:

``` emacs-lisp
(string= invocation-name "remacs")
```

This will return `t` in a Remacs instance.

;;(require 'screencast)
(require 'xwidget)
(defconst xwidget-screencast-webkit '("Hello, and welcome to a
  short demo of the Emacs xwidget branch, and the Webkit
  integration it provides." n
  "Xwidgets are toolkit widgets that behave like images in an
  Emacs buffer. Except they are actual widgets, so you can
  interact with them." n
  "There are several, but people seem to fancy the webkit the most so lets have a look!"
  (insert "some text")
  (xwidget-insert (point-min)  'webkit-osr "webkit-osr" 500  1000  5)
  n
  "Okay so thats an actual webkit instance in an Emacs buffer! " n
  "Mouse-overs work" n
  "Mouse-clicks work" n
  (split-window-vertically)
  "You can split the buffer and scroll the windows separately, as
  usual in Emacs. This is however not so usual in the browser
  world for some reason." n
  "So, can you use the xwidget branch as your main Emacs instance?"n
  "Not yet, its still not mature. There are many tricky issues
  left. That being said, there are many simple tasks to help out
  with also if you like!" ))


(defun xwidget-screencast(&optional arg)
  "Displays the screencast for xwidgets."
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
          xwidget-screencast-webkit "xvidgets" 1 ()))

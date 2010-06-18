;; xwidget.el - api functions for xwidgets
;;  see xwidget.c for more api functions

(require 'xwidget-internal)


(defun xwidget-insert (pos type title width height  &optional id)
 "Insert an xwidget at POS, given ID, TYPE, TITLE WIDTH and HEIGHT.
Return ID
ID will be made optional, but it isnt implemented yet!

currently interface is lame:
 :type 1=button, 2=toggle btn, 3=xembed socket(id will be printed to stdout)
 obviously these should be symbols

 :xwidget-id 1, MUST be unique and < 100 !
 if slightly wrong, emacs WILL CRASH

These issues are of course fixable but I will continue to
hand-wave issues like these until the hard parts are solved.
"
  (goto-char pos)
  (put-text-property (point) (+ 1 (point)) 'display (list 'xwidget ':xwidget-id id ':type type ':title title ':width width ':height height))
  id)


(defun xwidget-resize-at (pos width height)
  "Resize xwidget at postion POS to WIDTH and HEIGHT."
  (let*
      ((xwidget-prop (cdr (get-text-property pos 'display)))
       (id (plist-get  xwidget-prop ':xwidget-id)))
    (setq xwidget-prop (plist-put xwidget-prop ':width width))
    (setq xwidget-prop (plist-put xwidget-prop  ':height height))
          
    (put-text-property pos (+ 1 pos) 'display (cons 'xwidget xwidget-prop))
    (message "prop %s" xwidget-prop)
    (message "id %d w %d  h %d" id width height)
    (xwidget-resize-internal id width height)
  ))

;; use declare here?
;; (declare-function xwidget-resize-internal "xwidget.c" )
;; check-declare-function?

(provide 'xwidget)

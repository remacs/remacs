;; xwidget.el - api functions for xwidgets
;;  see xwidget.c for more api functions

(require 'xwidget-internal)

;;TODO model after make-text-button instead!
(defun xwidget-insert (pos type title width height)
 "Insert an xwidget at POS, given ID, TYPE, TITLE WIDTH and HEIGHT.
Return ID

see xwidget.c for types suitable for TYPE.
"
  (goto-char pos)
  (let ((id (make-xwidget (point) (point)  type  title  width  height nil)))
    (put-text-property (point) (+ 1 (point)) 'display (list 'xwidget ':xwidget id))
    id))


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


(defun xwidget-socket-handler ()
  "creates plug for socket. TODO"
  (interactive)
  (message "socket handler xwidget %S" last-input-event)
  (let*
      ((xwidget-event-type (nth 2 last-input-event))
       (xwidget-id (nth 1 last-input-event)))
    (cond ( (eq xwidget-event-type 'xembed-ready)
            (let*
                ((xembed-id (nth 3 last-input-event)))
              (message "xembed ready  event: %S xw-id:%s" xembed-id xwidget-id)
              ;;TODO fetch process data from the xwidget. create it, store process info
              ;;will start emacs/uzbl in a xembed socket when its ready
              ;; (cond
              ;;  ((eq 3 xwidget-id)
              ;;   (start-process "xembed" "*xembed*" (format "%ssrc/emacs" default-directory) "-q" "--parent-id" (number-to-string xembed-id) ) )
              ;;  ((eq 5 xwidget-id)
              ;;   (start-process "xembed2" "*xembed2*" "uzbl-core"  "-s" (number-to-string xembed-id)  "http://www.fsf.org" )  )
               
              )
            ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; webkit support
(require 'browse-url)
;;;###autoload
(defun xwidget-webkit-browse-url (url &optional new-session)
  "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.  URL
defaults to the string looking like a url around the cursor position."
  (interactive (progn
		 (require 'browse-url)
		 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (when (stringp url)
    (if new-session
	(xwidget-webkit-new-session url)
      (xwidget-webkit-goto-url url))))


;;todo.
;; - support browse-url with xwidget-webkit
;; - check that the webkit support is compiled in
(define-derived-mode xwidget-webkit-mode special-mode "xwidget-webkit" "xwidget webkit special mode" )
(defun xwidget-webkit-new-session (url)
  (save-excursion
    (let*
        ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
         (xwid 1))
      (set-buffer (get-buffer-create bufname))
      (insert " ")
      (xwidget-insert 1 'webkit-osr  bufname 1000 1000 xwid)
      (xwidget-webkit-mode)
      (xwidget-webkit-goto-uri xwid url ))
    )

  )

(defun xwidget-webkit-goto-url (url))


;; use declare here?
;; (declare-function xwidget-resize-internal "xwidget.c" )
;; check-declare-function?

(provide 'xwidget)

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
(defvar xwidget-webkit-last-session nil)
(defun xwidget-webkit-new-session (url)
  (save-excursion
    (let*
        ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
         )
      (set-buffer (get-buffer-create bufname))
      (insert " ")
      (setq xwidget-webkit-last-session (xwidget-insert 1 'webkit-osr  bufname 1000 1000))
      (xwidget-webkit-mode)
      (xwidget-webkit-goto-uri xwidget-webkit-last-session url ))
    )

  )

(defun xwidget-webkit-goto-url (url)
  (if xwidget-webkit-last-session
      (xwidget-webkit-goto-uri xwidget-webkit-last-session url)
    ( xwidget-webkit-new-session url)))


;; use declare here?
;; (declare-function xwidget-resize-internal "xwidget.c" )
;; check-declare-function?

(provide 'xwidget)

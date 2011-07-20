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
    (put-text-property (point)
                       (+ 1 (point)) 'display (list 'xwidget ':xwidget id))
    
    id))


(defun xwidget-at (pos)
  (car (cdr (cdr  (get-text-property pos 'display)))))



         
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
              )))))




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
(defvar xwidget-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'xwidget-webkit-browse-url)
    (define-key map "a" 'xwidget-webkit-adjust-size-to-content)
    (define-key map "b" 'xwidget-webkit-back )
    (define-key map "\C-m" 'xwidget-webkit-insert-string)
    map)
  
  "Keymap for `xwidget-webkit-mode'.")



(define-derived-mode xwidget-webkit-mode
  special-mode "xwidget-webkit" "xwidget webkit view mode"
    (setq buffer-read-only t))

(defvar xwidget-webkit-last-session-buffer nil)

(defun  xwidget-webkit-last-session ()
  (if (buffer-live-p xwidget-webkit-last-session-buffer)
      (save-excursion
        (set-buffer xwidget-webkit-last-session-buffer)
        (xwidget-at 1))
    nil))

(defun xwidget-adjust-size-to-content (xw)
  ;;xwidgets doesnt support widgets that have thoir own opinions about size well yet
  ;;this reads the size and sets it back
  (let ((size (xwidget-size-request xw)))
    (xwidget-resize xw (car size) (cadr size))))


(defun xwidget-webkit-insert-string (xw str)
  (interactive (list (xwidget-webkit-last-session)
                     (read-string "string:")))
  (xwidget-webkit-execute-script xw (format "document.activeElement.value='%s'" str)))

(defun xwidget-webkit-adjust-size-to-content ()
  (interactive)
  ( xwidget-adjust-size-to-content ( xwidget-webkit-last-session)))


(defun xwidget-webkit-new-session (url)

  (let*
      ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
       )
    (setq xwidget-webkit-last-session-buffer (switch-to-buffer (get-buffer-create bufname)))
    (insert " ")
    (xwidget-insert 1 'webkit-osr  bufname 1000 1000)
    (xwidget-webkit-mode)
    (xwidget-webkit-goto-uri ( xwidget-webkit-last-session) url )))


(defun xwidget-webkit-goto-url (url)
  (if ( xwidget-webkit-last-session)
      (progn
        (xwidget-webkit-goto-uri ( xwidget-webkit-last-session) url)
        (switch-to-buffer xwidget-webkit-last-session-buffer))
    ( xwidget-webkit-new-session url)))

(defun xwidget-webkit-back ()
  (interactive)
  (xwidget-webkit-execute-script ( xwidget-webkit-last-session)  "history.go(-1);"))

(defun xwidget-webkit-reload ()
  (interactive)
  (xwidget-webkit-execute-script ( xwidget-webkit-last-session)  "reload();"))

(defun xwidget-current-url ()
  "get the webkit url"
  ;;notice the fugly "title" hack. it is needed because the webkit api doesnt support returning values.
  ;;TODO make a wrapper for the title hack so its easy to remove should webkit someday support JS return values
  (xwidget-webkit-execute-script (xwidget-webkit-last-session) "document.title=document.URL;")
  (xwidget-webkit-get-title (xwidget-webkit-last-session)))



;; use declare here?
;; (declare-function xwidget-resize-internal "xwidget.c" )
;; check-declare-function?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xwidget-cleanup ()
  ;;its still pretty easy to trigger bugs with xwidgets.
  ;;this function tries to implement a workaround
  (interactive)
  (xwidget-delete-zombies) ;;kill xviews who should have been deleted but stull linger
  (redraw-display);;redraw display otherwise ghost of zombies  will remain to haunt the screen
  )



;;this is a workaround because I cant find the right place to put it in C
(add-hook 'window-configuration-change-hook 'xwidget-cleanup)

(defvar xwidget-webkit-kill-flash-oneshot nil)
(defun xwidget-webkit-kill-flash ()
  ;;you can only call this once or webkit crashes and takes emacs with it. odd.
  (unless xwidget-webkit-kill-flash-oneshot
    (xwidget-disable-plugin-for-mime "application/x-shockwave-flash")
    (setq xwidget-webkit-kill-flash-oneshot t)))

(xwidget-webkit-kill-flash)

(provide 'xwidget)

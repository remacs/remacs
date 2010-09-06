;;test like:
;; cd /path/to/xwidgets-emacs-dir
;; make   all&&  src/emacs -q --eval "(progn (load \"`pwd`/lisp/xwidget-test.el\") (xwidget-demo-basic))"


;; you should see:
;; - a gtk button
;; - a gtk toggle button
;; - a gtk slider button
;; - an xembed window(using gtk_socket) showing another emacs instance
;; - an xembed window(using gtk_socket) showing an uzbl web browser if its installed

;;the widgets will move when you type in the buffer. good!

;;there will be redrawing issues when widgets change rows, etc. bad!

;;its currently difficult to give kbd focus to the xembedded emacs,
;;but try evaling the following:

;; (xwidget-set-keyboard-grab 3 1)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; demo functions
(require 'xwidget)

(defun xwidget-demo-basic ()
  (interactive)
  (insert "xwidgetdemo<<< a button. another button\n")
  (xwidget-insert (point-min)  1 "button" 40  50  1)
  (xwidget-insert          15  2 "toggle" 60  30  2)
  (xwidget-insert          30  3 "emacs"  400 200 3)
  (xwidget-insert          20  4 "slider" 100 50  4)
  (xwidget-insert          40  3 "uzbl-core"   400 400 5)
  (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic)
)


(defun xwidget-demo-single ()
  (interactive)
  (insert "xwidgetdemo<<< a button. another button\n")
  (xwidget-insert (point-min) 1 "1" 200 300 1)
  (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic)
)

;it doesnt seem gtk_socket_steal works very well. its deprecated.
; xwininfo -int
; then (xwidget-embed-steal 3 <winid>)
(defun xwidget-demo-grab ()
  (interactive)
  (insert "0 <<< grabbed appp will appear here\n")
  (xwidget-insert          1 1 3 "1" 1000 1000)
  (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-grab)  
  )

;ive basically found these xembeddable things:
;openvrml
;emacs
;mplayer
;surf
;uzbl

;try the openvrml:
;/usr/libexec/openvrml-xembed 0 ~/Desktop/HelloWorld.wrl


(defun xwidget-handler-demo-basic ()
  (interactive)
  (message "stuff happened to xwidget %S" last-input-event)
  (let*
      ((xwidget-event-type (nth 2 last-input-event))
       (xwidget-id (nth 1 last-input-event)))
    (cond ( (eq xwidget-event-type 'xembed-ready)
            (let*
                ((xembed-id (nth 3 last-input-event)))
              (message "xembed ready  %S" xembed-id)
              ;;will start emacs/uzbl in a xembed socket when its ready
              (cond
               ((eq 3 xwidget-id)
                (start-process "xembed" "*xembed*" (format "%ssrc/emacs" default-directory) "-q" "--parent-id" (number-to-string xembed-id) ) )
               ((eq 5 xwidget-id)
                (start-process "xembed2" "*xembed2*" "uzbl-core"  "-s" (number-to-string xembed-id)  "http://www.fsf.org" )  )
               
              )
            )))))



(defun xwidget-handler-demo-grab ()
  (interactive)
  (message "stuff happened to xwidget %S" last-input-event)
  (let*
      ((xwidget-event-type (nth 2 last-input-event)))
    (cond ( (eq xwidget-event-type 'xembed-ready)
            (let*
                ((xembed-id (nth 3 last-input-event)))
              (message "xembed ready  %S" xembed-id)
              )
            ))))
(defun xwidget-dummy-hook ()
  (message "xwidget dummy hook called"))

;  (xwidget-resize-hack 1 200 200)

;(xwidget-demo-basic)

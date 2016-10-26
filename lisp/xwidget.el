;;; xwidget.el --- api functions for xwidgets  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2011-2016 Free Software Foundation, Inc.
;;
;; Author: Joakim Verona (joakim@verona.se)
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;; --------------------------------------------------------------------

;;; Commentary:
;;
;; See xwidget.c for more api functions.

;; This breaks compilation when we don't have xwidgets.
;; And is pointless when we do, since it's in C and so preloaded.
;;(require 'xwidget-internal)

;;; Code:

(require 'cl-lib)
(require 'bookmark)

(declare-function make-xwidget "xwidget.c"
                  (type title width height arguments &optional buffer))
(declare-function xwidget-set-adjustment "xwidget.c"
                  (xwidget axis relative value))
(declare-function xwidget-buffer "xwidget.c" (xwidget))
(declare-function xwidget-webkit-get-title "xwidget.c" (xwidget))
(declare-function xwidget-size-request "xwidget.c" (xwidget))
(declare-function xwidget-resize "xwidget.c" (xwidget new-width new-height))
(declare-function xwidget-webkit-execute-script "xwidget.c"
                  (xwidget script &optional callback))
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function xwidget-plist "xwidget.c" (xwidget))
(declare-function set-xwidget-plist "xwidget.c" (xwidget plist))
(declare-function xwidget-view-window "xwidget.c" (xwidget-view))
(declare-function xwidget-view-model "xwidget.c" (xwidget-view))
(declare-function delete-xwidget-view "xwidget.c" (xwidget-view))
(declare-function get-buffer-xwidgets "xwidget.c" (buffer))
(declare-function xwidget-query-on-exit-flag "xwidget.c" (xwidget))

(defun xwidget-insert (pos type title width height &optional args)
  "Insert an xwidget at position POS.
Supply the xwidget's TYPE, TITLE, WIDTH, and HEIGHT.
See `make-xwidget' for the possible TYPE values.
The usage of optional argument ARGS depends on the xwidget.
This returns the result of `make-xwidget'."
  (goto-char pos)
  (let ((id (make-xwidget type title width height args)))
    (put-text-property (point) (+ 1 (point))
                       'display (list 'xwidget ':xwidget id))
    id))

(defun xwidget-at (pos)
  "Return xwidget at POS."
  ;; TODO this function is a bit tedious because the C layer isn't well
  ;; protected yet and xwidgetp apparently doesn't work yet.
  (let* ((disp (get-text-property pos 'display))
         (xw (car (cdr (cdr  disp)))))
    ;;(if (xwidgetp  xw) xw nil)
    (if (equal 'xwidget (car disp)) xw)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; webkit support
(require 'browse-url)
(require 'image-mode);;for some image-mode alike functionality

;;;###autoload
(defun xwidget-webkit-browse-url (url &optional new-session)
  "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             ;;(xwidget-webkit-current-url)
                                             )))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (when (stringp url)
    (if new-session
        (xwidget-webkit-new-session url)
      (xwidget-webkit-goto-url url))))

;;todo.
;; - check that the webkit support is compiled in
(defvar xwidget-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'xwidget-webkit-browse-url)
    (define-key map "a" 'xwidget-webkit-adjust-size-dispatch)
    (define-key map "b" 'xwidget-webkit-back)
    (define-key map "r" 'xwidget-webkit-reload)
    (define-key map "t" (lambda () (interactive) (message "o"))) ;FIXME: ?!?
    (define-key map "\C-m" 'xwidget-webkit-insert-string)
    (define-key map "w" 'xwidget-webkit-current-url)

    ;;similar to image mode bindings
    (define-key map (kbd "SPC")                 'xwidget-webkit-scroll-up)
    (define-key map (kbd "DEL")                 'xwidget-webkit-scroll-down)

    (define-key map [remap scroll-up]           'xwidget-webkit-scroll-up)
    (define-key map [remap scroll-up-command]   'xwidget-webkit-scroll-up)

    (define-key map [remap scroll-down]         'xwidget-webkit-scroll-down)
    (define-key map [remap scroll-down-command] 'xwidget-webkit-scroll-down)

    (define-key map [remap forward-char]        'xwidget-webkit-scroll-forward)
    (define-key map [remap backward-char]       'xwidget-webkit-scroll-backward)
    (define-key map [remap right-char]          'xwidget-webkit-scroll-forward)
    (define-key map [remap left-char]           'xwidget-webkit-scroll-backward)
    ;; (define-key map [remap previous-line]          'image-previous-line)
    ;; (define-key map [remap next-line]              'image-next-line)

    ;; (define-key map [remap move-beginning-of-line] 'image-bol)
    ;; (define-key map [remap move-end-of-line]       'image-eol)
    ;; (define-key map [remap beginning-of-buffer]    'image-bob)
    ;; (define-key map [remap end-of-buffer]          'image-eob)
    map)
  "Keymap for `xwidget-webkit-mode'.")

(defun xwidget-webkit-scroll-up ()
  "Scroll webkit up."
  (interactive)
  (xwidget-set-adjustment (xwidget-webkit-last-session) 'vertical t 50))

(defun xwidget-webkit-scroll-down ()
  "Scroll webkit down."
  (interactive)
  (xwidget-set-adjustment (xwidget-webkit-last-session) 'vertical t -50))

(defun xwidget-webkit-scroll-forward ()
  "Scroll webkit forwards."
  (interactive)
  (xwidget-set-adjustment (xwidget-webkit-last-session) 'horizontal t 50))

(defun xwidget-webkit-scroll-backward ()
  "Scroll webkit backwards."
  (interactive)
  (xwidget-set-adjustment (xwidget-webkit-last-session) 'horizontal t -50))


;; The xwidget event needs to go into a higher level handler
;; since the xwidget can generate an event even if it's offscreen.
;; TODO this needs to use callbacks and consider different xwidget event types.
(define-key (current-global-map) [xwidget-event] #'xwidget-event-handler)
(defun xwidget-log (&rest msg)
  "Log MSG to a buffer."
  (let ((buf (get-buffer-create " *xwidget-log*")))
    (with-current-buffer buf
      (insert (apply #'format msg))
      (insert "\n"))))

(defun xwidget-event-handler ()
  "Receive xwidget event."
  (interactive)
  (xwidget-log "stuff happened to xwidget %S" last-input-event)
  (let*
      ((xwidget-event-type (nth 1 last-input-event))
       (xwidget (nth 2 last-input-event))
       ;;(xwidget-callback (xwidget-get xwidget 'callback))
       ;;TODO stopped working for some reason
       )
    ;;(funcall  xwidget-callback xwidget xwidget-event-type)
    (message "xw callback %s" xwidget)
    (funcall  'xwidget-webkit-callback xwidget xwidget-event-type)))

(defun xwidget-webkit-callback (xwidget xwidget-event-type)
  "Callback for xwidgets.
XWIDGET instance, XWIDGET-EVENT-TYPE depends on the originating xwidget."
  (if (not (buffer-live-p (xwidget-buffer xwidget)))
      (xwidget-log
       "error: callback called for xwidget with dead buffer")
    (with-current-buffer (xwidget-buffer xwidget)
      (cond ((eq xwidget-event-type 'load-changed)
             (xwidget-log "webkit finished loading: '%s'"
                          (xwidget-webkit-get-title xwidget))
             ;;TODO - check the native/internal scroll
             ;;(xwidget-adjust-size-to-content xwidget)
             (xwidget-webkit-adjust-size-dispatch) ;;TODO xwidget arg
             (rename-buffer (format "*xwidget webkit: %s *"
                                    (xwidget-webkit-get-title xwidget)))
             (pop-to-buffer (current-buffer)))
            ((eq xwidget-event-type 'decide-policy)
             (let ((strarg  (nth 3 last-input-event)))
               (if (string-match ".*#\\(.*\\)" strarg)
                   (xwidget-webkit-show-id-or-named-element
                    xwidget
                    (match-string 1 strarg)))))
            ((eq xwidget-event-type 'javascript-callback)
             (let ((proc (nth 3 last-input-event))
                   (arg  (nth 4 last-input-event)))
               (funcall proc arg)))
            (t (xwidget-log "unhandled event:%s" xwidget-event-type))))))

(defvar bookmark-make-record-function)
(define-derived-mode xwidget-webkit-mode
    special-mode "xwidget-webkit" "Xwidget webkit view mode."
    (setq buffer-read-only t)
    (setq-local bookmark-make-record-function
                #'xwidget-webkit-bookmark-make-record)
    ;; Keep track of [vh]scroll when switching buffers
    (image-mode-setup-winprops))

(defun xwidget-webkit-bookmark-make-record ()
  "Integrate Emacs bookmarks with the webkit xwidget."
  (nconc (bookmark-make-record-default t t)
         `((page     . ,(xwidget-webkit-current-url))
           (handler  . (lambda (bmk) (browse-url
                                 (bookmark-prop-get bmk 'page)))))))


(defvar xwidget-webkit-last-session-buffer nil)

(defun xwidget-webkit-last-session ()
  "Last active webkit, or nil."
  (if (buffer-live-p xwidget-webkit-last-session-buffer)
      (with-current-buffer xwidget-webkit-last-session-buffer
        (xwidget-at (point-min)))
    nil))

(defun xwidget-webkit-current-session ()
  "Either the webkit in the current buffer, or the last one used.
The latter might be nil."
  (or (xwidget-at (point-min)) (xwidget-webkit-last-session)))

(defun xwidget-adjust-size-to-content (xw)
  "Resize XW to content."
  ;; xwidgets doesn't support widgets that have their own opinions about
  ;; size well, yet this reads the desired size and resizes the Emacs
  ;; allocated area accordingly.
  (let ((size (xwidget-size-request xw)))
    (xwidget-resize xw (car size) (cadr size))))


(defvar xwidget-webkit-activeelement-js"
function findactiveelement(doc){
//alert(doc.activeElement.value);
   if(doc.activeElement.value != undefined){
      return doc.activeElement;
   }else{
        // recurse over the child documents:
        var frames = doc.getElementsByTagName('frame');
        for (var i = 0; i < frames.length; i++)
        {
                var d = frames[i].contentDocument;
                 var rv = findactiveelement(d);
                 if(rv != undefined){
                    return rv;
                 }
        }
    }
    return undefined;
};


"

  "javascript that finds the active element."
  ;; Yes it's ugly, because:
  ;; - there is apparently no way to find the active frame other than recursion
  ;; - the js "for each" construct misbehaved on the "frames" collection
  ;; - a window with no frameset still has frames.length == 1, but
  ;; frames[0].document.activeElement != document.activeElement
  ;;TODO the activeelement type needs to be examined, for iframe, etc.
  )

(defun xwidget-webkit-insert-string (xw str)
  "Insert string STR in the active field in the webkit XW."
  ;; Read out the string in the field first and provide for edit.
  (interactive
   (let* ((xww (xwidget-webkit-current-session))

          (field-value
           (progn
             (xwidget-webkit-execute-script xww xwidget-webkit-activeelement-js)
             (xwidget-webkit-execute-script-rv
              xww
              "findactiveelement(document).value;")))
          (field-type (xwidget-webkit-execute-script-rv
                       xww
                       "findactiveelement(document).type;")))
     (list xww
           (cond ((equal "text" field-type)
                  (read-string "Text: " field-value))
                 ((equal "password" field-type)
                  (read-passwd "Password: " nil field-value))
                 ((equal "textarea" field-type)
                  (xwidget-webkit-begin-edit-textarea xww field-value))))))
  (xwidget-webkit-execute-script
   xw
   (format "findactiveelement(document).value='%s'" str)))

(defvar xwidget-xwbl)
(defun xwidget-webkit-begin-edit-textarea (xw text)
  "Start editing of a webkit text area.
XW is the xwidget identifier, TEXT is retrieved from the webkit."
  (switch-to-buffer
   (generate-new-buffer "textarea"))
  (set (make-local-variable 'xwidget-xwbl) xw)
  (insert text))

(defun xwidget-webkit-end-edit-textarea ()
  "End editing of a webkit text area."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\\n" nil t))
  (xwidget-webkit-execute-script
   xwidget-xwbl
   (format "findactiveelement(document).value='%s'"
           (buffer-substring (point-min) (point-max))))
  ;;TODO convert linefeed to \n
  )

(defun xwidget-webkit-show-named-element (xw element-name)
  "Make webkit xwidget XW show a named element ELEMENT-NAME.
For example, use this to display an anchor."
  (interactive (list (xwidget-webkit-current-session)
                     (read-string "Element name: ")))
  ;;TODO since an xwidget is an Emacs object, it is not trivial to do
  ;; some things that are taken for granted in a normal browser.
  ;; scrolling an anchor/named-element into view is one such thing.
  ;; This function implements a proof-of-concept for this.  Problems
  ;; remaining: - The selected window is scrolled but this is not
  ;; always correct - This needs to be interfaced into browse-url
  ;; somehow.  The tricky part is that we need to do this in two steps:
  ;; A: load the base url, wait for load signal to arrive B: navigate
  ;; to the anchor when the base url is finished rendering

  ;; This part figures out the Y coordinate of the element
  (let ((y (string-to-number
            (xwidget-webkit-execute-script-rv
             xw
             (format
              "document.getElementsByName('%s')[0].getBoundingClientRect().top"
              element-name)
             0))))
    ;; Now we need to tell Emacs to scroll the element into view.
    (xwidget-log "scroll: %d" y)
    (set-window-vscroll (selected-window) y t)))

(defun xwidget-webkit-show-id-element (xw element-id)
  "Make webkit xwidget XW show an id-element ELEMENT-ID.
For example, use this to display an anchor."
  (interactive (list (xwidget-webkit-current-session)
                     (read-string "Element id: ")))
  (let ((y (string-to-number
            (xwidget-webkit-execute-script-rv
             xw
             (format "document.getElementById('%s').getBoundingClientRect().top"
                     element-id)
             0))))
    ;; Now we need to tell Emacs to scroll the element into view.
    (xwidget-log "scroll: %d" y)
    (set-window-vscroll (selected-window) y t)))

(defun xwidget-webkit-show-id-or-named-element (xw element-id)
   "Make webkit xwidget XW show a name or element id ELEMENT-ID.
For example, use this to display an anchor."
  (interactive (list (xwidget-webkit-current-session)
                     (read-string "Name or element id: ")))
  (let* ((y1 (string-to-number
              (xwidget-webkit-execute-script-rv
               xw
               (format "document.getElementsByName('%s')[0].getBoundingClientRect().top" element-id)
               "0")))
         (y2 (string-to-number
              (xwidget-webkit-execute-script-rv
               xw
               (format "document.getElementById('%s').getBoundingClientRect().top" element-id)
               "0")))
         (y3 (max y1 y2)))
    ;; Now we need to tell Emacs to scroll the element into view.
    (xwidget-log "scroll: %d" y3)
    (set-window-vscroll (selected-window) y3 t)))

(defun xwidget-webkit-adjust-size-to-content ()
  "Adjust webkit to content size."
  (interactive)
  (xwidget-adjust-size-to-content (xwidget-webkit-current-session)))

(defun xwidget-webkit-adjust-size-dispatch ()
  "Adjust size according to mode."
  (interactive)
  (xwidget-webkit-adjust-size-to-window)
  ;; The recenter is intended to correct a visual glitch.
  ;; It errors out if the buffer isn't visible, but then we don't get
  ;; the glitch, so silence errors.
  (ignore-errors
    (recenter-top-bottom)))

(defun xwidget-webkit-adjust-size-to-window ()
  "Adjust webkit to window."
  (interactive)
  (xwidget-resize (xwidget-webkit-current-session) (window-pixel-width)
                  (window-pixel-height)))

(defun xwidget-webkit-adjust-size (w h)
  "Manually set webkit size to width W, height H."
  ;; TODO shouldn't be tied to the webkit xwidget
  (interactive "nWidth:\nnHeight:\n")
  (xwidget-resize (xwidget-webkit-current-session) w h))

(defun xwidget-webkit-fit-width ()
  "Adjust width of webkit to window width."
  (interactive)
  (xwidget-webkit-adjust-size (- (nth 2 (window-inside-pixel-edges))
                                 (car (window-inside-pixel-edges)))
                              1000))

(defun xwidget-webkit-new-session (url)
  "Create a new webkit session buffer with URL."
  (let*
      ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
       xw)
    (setq xwidget-webkit-last-session-buffer (switch-to-buffer
                                              (get-buffer-create bufname)))
    (insert " 'a' adjusts the xwidget size.")
    (setq xw (xwidget-insert 1 'webkit  bufname 1000 1000))
    (xwidget-put xw 'callback 'xwidget-webkit-callback)
    (xwidget-webkit-mode)
    (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url)))


(defun xwidget-webkit-goto-url (url)
  "Goto URL."
  (if (xwidget-webkit-current-session)
      (progn
        (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url))
    (xwidget-webkit-new-session url)))

(defun xwidget-webkit-back ()
  "Go back in history."
  (interactive)
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 "history.go(-1);"))

(defun xwidget-webkit-reload ()
  "Reload current url."
  (interactive)
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 "history.go(0);"))

(defun xwidget-webkit-current-url ()
  "Get the webkit url and place it on the kill-ring."
  (interactive)
  (let* ((rv (xwidget-webkit-execute-script-rv (xwidget-webkit-current-session)
                                               "document.URL"))
         (url (kill-new (or rv ""))))
    (message "url: %s" url)
    url))

(defun xwidget-webkit-execute-script-rv (xw script &optional default)
  "Same as `xwidget-webkit-execute-script' but with return value.
XW is the webkit instance.  SCRIPT is the script to execute.
DEFAULT is the default return value."
  ;; Notice the ugly "title" hack.  It is needed because the Webkit
  ;; API at the time of writing didn't support returning values.  This
  ;; is a wrapper for the title hack so it's easy to remove should
  ;; Webkit someday support JS return values or we find some other way
  ;; to access the DOM.

  ;; Reset webkit title.  Not very nice.
  (let* ((emptytag "titlecantbewhitespaceohthehorror")
         title)
    (xwidget-webkit-execute-script xw (format "document.title=\"%s\";"
                                              (or default emptytag)))
    (xwidget-webkit-execute-script xw (format "document.title=%s;" script))
    (setq title (xwidget-webkit-get-title xw))
    (if (equal emptytag title)
        (setq title ""))
    (unless title
      (setq title default))
    title))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xwidget-webkit-get-selection ()
  "Get the webkit selection."
  (xwidget-webkit-execute-script-rv (xwidget-webkit-current-session)
                                    "window.getSelection().toString();"))

(defun xwidget-webkit-copy-selection-as-kill ()
  "Get the webkit selection and put it on the kill-ring."
  (interactive)
  (kill-new (xwidget-webkit-get-selection)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xwidget plist management (similar to the process plist functions)

(defun xwidget-get (xwidget propname)
  "Get an xwidget's property value.
XWIDGET is an xwidget, PROPNAME a property.
Returns the last value stored with `xwidget-put'."
  (plist-get (xwidget-plist xwidget) propname))

(defun xwidget-put (xwidget propname value)
  "Set an xwidget's property value.
XWIDGET is an xwidget, PROPNAME a property to be set to specified VALUE.
You can retrieve the value with `xwidget-get'."
  (set-xwidget-plist xwidget
                     (plist-put (xwidget-plist xwidget) propname value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xwidget-view-list)              ; xwidget.c
(defvar xwidget-list)                   ; xwidget.c

(defun xwidget-delete-zombies ()
  "Helper for `xwidget-cleanup'."
  (dolist (xwidget-view xwidget-view-list)
    (when (or (not (window-live-p (xwidget-view-window xwidget-view)))
              (not (memq (xwidget-view-model xwidget-view)
                         xwidget-list)))
      (delete-xwidget-view xwidget-view))))

(defun xwidget-cleanup ()
  "Delete zombie xwidgets."
  ;; During development it was sometimes easy to wind up with zombie
  ;; xwidget instances.
  ;; This function tries to implement a workaround should it occur again.
  (interactive)
  ;; Kill xviews that should have been deleted but still linger.
  (xwidget-delete-zombies)
  ;; Redraw display otherwise ghost of zombies will remain to haunt the screen
  (redraw-display))

(defun xwidget-kill-buffer-query-function ()
  "Ask before killing a buffer that has xwidgets."
  (let ((xwidgets (get-buffer-xwidgets (current-buffer))))
    (or (not xwidgets)
        (not (memq t (mapcar #'xwidget-query-on-exit-flag xwidgets)))
        (yes-or-no-p
         (format "Buffer %S has xwidgets; kill it? " (buffer-name))))))

(when (featurep 'xwidget-internal)
  (add-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function)
  ;; This would have felt better in C, but this seems to work well in
  ;; practice though.
  (add-hook 'window-configuration-change-hook #'xwidget-delete-zombies))

(provide 'xwidget)
;;; xwidget.el ends here

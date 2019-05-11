(require 'subr-x)
(require 'cl-lib)
(require 'color)

(defface vterm
  '((t :inherit default))
  "Default face to use in Term mode."
  :group 'vterm)

(defface vterm-color-black
  '((t :foreground "black" :background "black"))
  "Face used to render black color code."
  :group 'vterm)

(defface vterm-color-red
  '((t :foreground "red3" :background "red3"))
  "Face used to render red color code."
  :group 'vterm)

(defface vterm-color-green
  '((t :foreground "green3" :background "green3"))
  "Face used to render green color code."
  :group 'vterm)

(defface vterm-color-yellow
  '((t :foreground "yellow3" :background "yellow3"))
  "Face used to render yellow color code."
  :group 'vterm)

(defface vterm-color-blue
  '((t :foreground "blue2" :background "blue2"))
  "Face used to render blue color code."
  :group 'vterm)

(defface vterm-color-magenta
  '((t :foreground "magenta3" :background "magenta3"))
  "Face used to render magenta color code."
  :group 'vterm)

(defface vterm-color-cyan
  '((t :foreground "cyan3" :background "cyan3"))
  "Face used to render cyan color code."
  :group 'vterm)

(defface vterm-color-white
  '((t :foreground "white" :background "white"))
  "Face used to render white color code."
  :group 'vterm)

(defcustom vterm-shell (getenv "SHELL")
  "The shell that gets run in the vterm."
  :type 'string
  :group 'vterm)

(defcustom vterm-display-method 'switch-to-buffer
  "Default display method."
  :type '(choice (const :tag "Display buffer." 'switch-to-buffer)
                 (const :tag "Pop to buffer." 'pop-to-buffer))
  :group 'vterm)

(defcustom vterm-max-scrollback 1000
  "Maximum 'scrollback' value."
  :type 'number
  :group 'vterm)

(defcustom vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v" "C-y")
  "Exceptions for vterm-keymap.

  If you use a keybinding with a prefix-key, add that prefix-key to
  this list. Note that after doing so that prefix-key cannot be sent
  to the terminal anymore."
  :type '(repeat string)
  :group 'vterm)

(defvar vterm--term nil
  "Pointer to Term.")
(make-variable-buffer-local 'vterm--term)

(defvar vterm-buffer-name "*vterm*"
  "Buffer name for vterm buffers.")

(defvar vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]                       #'vterm-self-insert)
    (define-key map [backspace]                 #'vterm-self-insert)
    (define-key map [M-backspace]               #'vterm-self-insert)
    (define-key map [return]                    #'vterm-self-insert)
    (define-key map [left]                      #'vterm-self-insert)
    (define-key map [right]                     #'vterm-self-insert)
    (define-key map [up]                        #'vterm-self-insert)
    (define-key map [down]                      #'vterm-self-insert)
    (define-key map [home]                      #'vterm-self-insert)
    (define-key map [end]                       #'vterm-self-insert)
    (define-key map [escape]                    #'vterm-self-insert)
    (define-key map [remap self-insert-command] #'vterm-self-insert)
    (define-key map [remap yank]                #'vterm-yank)
    (define-key map (kbd "C-c C-y")             #'vterm-self-insert)
    (define-key map (kbd "C-c C-c")             #'vterm-send-ctrl-c)
    map)
  "Keymap for `vterm-mode'.")

;; Function keys and most of C-  and M- bindings
(mapcar (lambda (key)
          (define-key vterm-mode-map (kbd key) #'vterm-self-insert))
        (append (cl-loop for number from 1 to 12
                         collect (format "<f%i>" number))
                (cl-loop for prefix in '("C-" "M-")
                         append (cl-loop for char from ?a to ?z
                                         for key = (format "%s%c" prefix char)
                                         unless (member key vterm-keymap-exceptions)
                                         collect key))))

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Major mode for vterm buffer."
  (buffer-disable-undo)
  (setq vterm--term (vterm-new (window-body-height)
                               (window-body-width)
                               (vterm-make-process)
                               (- vterm-max-scrollback 62)))
  (setq buffer-read-only t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (add-hook 'window-size-change-functions #'vterm-resize-window t t)
  )

(defun vterm-make-process ()
  (let ((process-environment (append '("TERM=xterm"
                                       "INSIDE_EMACS=vterm"
                                       "LINES"
                                       "COLUMNS")
                                     process-environment)))
    (setq vterm--process
          (make-process
           :name "vterm"
           :buffer (current-buffer)
           :command `("/bin/sh" "-c"
                      ,(format "stty -nl sane iutf8 rows %d columns %d >/dev/null && exec %s"
                               (window-body-height)
                               (window-body-width)
                               vterm-shell))
           :coding 'no-conversion
           :connection-type 'pty
           :filter #'vterm-filter
           :sentinel #'vterm-sentinel))))

(defun vterm-filter (process output)
  "I/O Event. Feeds PROCESS's OUTPUT to the virtual terminal.

Then triggers a redraw from the module."
  (let ((inhibit-redisplay t)
        (inhibit-read-only t))
    (with-current-buffer (process-buffer process)
      (vterm-write-input vterm--term output)
      (vterm-update vterm--term))))

(defun vterm-sentinel (proc string)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun vterm-resize-window (frame)
  "Callback triggered by a size change of the FRAME.

Feeds the size change to the virtual terminal."
  (dolist (window (window-list frame))
    (with-current-buffer (window-buffer window)
      (when vterm--term
        (let ((height (window-body-height window))
              (width (window-body-width window))
              (inhibit-read-only t))
          (set-process-window-size vterm--process height width)
          (vterm-set-size vterm--term height width))))))

;; (defun vterm-resize-window (window)
;;   "Callback triggered by a size change of the WINDOW.

;; Feeds the size change to the virtual terminal."
;;   (with-current-buffer (window-buffer window)
;;     (when (and (processp vterm--process)
;;                (process-live-p vterm--process))
;;       (let ((height (window-body-height window))
;;             (width (window-body-width window))
;;             (inhibit-read-only t))
;;         (set-process-window-size vterm--process height width)
;;         (vterm--set-size vterm--term height width)))))


;; (defun vterm-resize-window (frame)
;;   "Callback triggered by a size change of the FRAME.

;; This is only used, when variable `emacs-version' < 27. Calls
;; `vterm--window-size-change' for every window of FRAME."
;;   (dolist (window (window-list frame))
;;     (vterm--window-size-change window)))

(defun vterm-self-insert ()
  "Sends invoking key to libvterm."
  (interactive)
    (let* ((modifiers (event-modifiers last-input-event))
           (shift (memq 'shift modifiers))
           (meta (memq 'meta modifiers))
           (ctrl (memq 'control modifiers)))
      (when-let ((key (key-description (vector (event-basic-type last-input-event)))))
        (vterm-send-key key shift meta ctrl))))

(defun vterm-send-key (key &optional shift meta ctrl)
  "Sends KEY to libvterm with optional modifiers SHIFT, META and CTRL."
    (let ((inhibit-redisplay t)
          (inhibit-read-only t))
      (when (and shift (not meta) (not ctrl))
        (setq key (upcase key)))
      (vterm-update vterm--term key shift meta ctrl)))

(defun vterm-send-ctrl-c ()
  "Sends C-c to the libvterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

(defun vterm--face-color-hex (face attr)
  "Return the color of the FACE's ATTR as a hex string."
  (apply #'color-rgb-to-hex (append (color-name-to-rgb (face-attribute face attr nil 'default)) '(2))))

(defun vterm-yank ()
  "Implementation of `yank' (paste) in vterm."
  (interactive)
  (vterm-send-string (current-kill 0)))

(defun vterm-send-string (string)
  "Send the string STRING to vterm."
  (when vterm--term
    (dolist (char (string-to-list string))
      (vterm-update vterm--term (char-to-string char) nil nil nil))))

;;;###autoload
(defun vterm (&optional arg)
  "Display vterminal. If called with prefix arg open new terminal."
  (interactive "P")
  (let ((buffer (if arg
                    (generate-new-buffer vterm-buffer-name)
                  (get-buffer-create vterm-buffer-name))))
    (when (or arg (not (get-buffer-process buffer)))
      (with-current-buffer buffer
        (vterm-mode)))
    (funcall vterm-display-method buffer)))

(provide 'vterm)

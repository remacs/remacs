;;; tab-bar.el --- frame-local tabs with named persistent window configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: frames tabs
;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `tab-bar-mode' to control display of the tab bar and
;; bindings for the global tab bar.

;; The normal global binding for [tab-bar] (below) uses the value of
;; `tab-bar-map' as the actual keymap to define the tab bar.  Modes
;; may either bind items under the [tab-bar] prefix key of the local
;; map to add to the global bar or may set `tab-bar-map'
;; buffer-locally to override it.

;;; Code:


(defgroup tab-bar nil
  "Frame-local tabs."
  :group 'convenience
  :version "27.1")

(defgroup tab-bar-faces nil
  "Faces used in the tab bar."
  :group 'tab-bar
  :group 'faces
  :version "27.1")

(defface tab-bar
  '((((type x w32 ns) (class color))
     :height 1.1
     :background "grey85"
     :foreground "black")
    (((type x) (class mono))
     :background "grey")
    (t
     :inverse-video t))
  "Tab bar face."
  :version "27.1"
  :group 'tab-bar-faces)

(defface tab-bar-tab
  '((((class color) (min-colors 88))
     :box (:line-width 1 :style released-button))
    (t
     :inverse-video nil))
  "Tab bar face for selected tab."
  :version "27.1"
  :group 'tab-bar-faces)

(defface tab-bar-tab-inactive
  '((default
      :inherit tab-bar-tab)
    (((class color) (min-colors 88))
     :background "grey75")
    (t
     :inverse-video t))
  "Tab bar face for non-selected tab."
  :version "27.1"
  :group 'tab-bar-faces)


(define-minor-mode tab-bar-mode
  "Toggle the tab bar in all graphical frames (Tab Bar mode)."
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable tab-bar-mode
  (let ((val (if tab-bar-mode 1 0)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'tab-bar-lines val))
    ;; If the user has given `default-frame-alist' a `tab-bar-lines'
    ;; parameter, replace it.
    (if (assq 'tab-bar-lines default-frame-alist)
        (setq default-frame-alist
              (cons (cons 'tab-bar-lines val)
                    (assq-delete-all 'tab-bar-lines
                                     default-frame-alist)))))
  (when tab-bar-mode
    (global-set-key [(control shift iso-lefttab)] 'tab-bar-switch-to-prev-tab)
    (global-set-key [(control shift tab)]         'tab-bar-switch-to-prev-tab)
    (global-set-key [(control tab)]               'tab-bar-switch-to-next-tab)))

(defun tab-bar-handle-mouse (event)
  "Text-mode emulation of switching tabs on the tab bar.
This command is used when you click the mouse in the tab bar
on a console which has no window system but does have a mouse."
  (interactive "e")
  (let* ((x-position (car (posn-x-y (event-start event))))
         (keymap (lookup-key (cons 'keymap (nreverse (current-active-maps))) [tab-bar]))
         (column 0))
    (when x-position
      (unless (catch 'done
                (map-keymap
                 (lambda (_key binding)
                   (when (eq (car-safe binding) 'menu-item)
                     (when (> (+ column (length (nth 1 binding))) x-position)
                       ;; TODO: handle close
                       (unless (get-text-property (- x-position column) 'close-tab (nth 1 binding))
                         (call-interactively (nth 2 binding)))
                       (throw 'done t))
                     (setq column (+ column (length (nth 1 binding))))))
                 keymap))
        ;; Clicking anywhere outside existing tabs will add a new tab
        (tab-bar-new-tab)))))

;; Used in the Show/Hide menu, to have the toggle reflect the current frame.
(defun toggle-tab-bar-mode-from-frame (&optional arg)
  "Toggle tab bar on or off, based on the status of the current frame.
See `tab-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tab-bar-mode (if (> (frame-parameter nil 'tab-bar-lines) 0) 0 1))
    (tab-bar-mode arg)))

(defvar tab-bar-map (make-sparse-keymap)
  "Keymap for the tab bar.
Define this locally to override the global tab bar.")

(global-set-key [tab-bar]
                `(menu-item ,(purecopy "tab bar") ignore
                            :filter tab-bar-make-keymap))

(defconst tab-bar-keymap-cache (make-hash-table :weakness t :test 'equal))

(defun tab-bar-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tab-bar-map'.
Its main job is to show tabs in the tab bar."
  (if (= 1 (length tab-bar-map))
      (tab-bar-make-keymap-1)
    (let ((key (cons (frame-terminal) tab-bar-map)))
      (or (gethash key tab-bar-keymap-cache)
          (puthash key tab-bar-map tab-bar-keymap-cache)))))


(defcustom tab-bar-new-tab-choice t
  "Defines what to show in a new tab.
If t, start a new tab with the current buffer, i.e. the buffer
that was current before calling the command that adds a new tab
(this is the same what `make-frame' does by default).
If the value is a string, switch to a buffer if it exists, or switch
to a buffer visiting the file or directory that the string specifies.
If the value is a function, call it with no arguments and switch to
the buffer that it returns.
If nil, duplicate the contents of the tab that was active
before calling the command that adds a new tab."
  :type '(choice (const     :tag "Current buffer" t)
                 (directory :tag "Directory" :value "~/")
                 (file      :tag "File" :value "~/.emacs")
                 (string    :tag "Buffer" "*scratch*")
                 (function  :tag "Function")
                 (const     :tag "Duplicate tab" nil))
  :group 'tab-bar
  :version "27.1")

(defvar tab-bar-new-button
  (propertize " + "
              'display `(image :type xpm
                               :file ,(expand-file-name
                                       "images/tabs/new.xpm"
                                       data-directory)
                               :margin (2 . 0)
                               :ascent center))
  "Button for creating a new tab.")

(defcustom tab-bar-close-button-show t
  "Defines where to show the close tab button.
If t, show the close tab button on all tabs.
If `selected', show it only on the selected tab.
If `non-selected', show it only on non-selected tab.
If nil, don't show it at all."
  :type '(choice (const :tag "On all tabs" t)
                 (const :tag "On selected tab" selected)
                 (const :tag "On non-selected tabs" non-selected)
                 (const :tag "None" nil))
  :set (lambda (sym val)
         (set sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "27.1")

(defvar tab-bar-close-button
  (propertize " x"
              'display `(image :type xpm
                               :file ,(expand-file-name
                                       "images/tabs/close.xpm"
                                       data-directory)
                               :margin (2 . 0)
                               :ascent center)
              'close-tab t
              :help "Click to close tab")
  "Button for closing the clicked tab.")

(defvar tab-bar-separator nil)


(defvar tab-bar-tab-name-function #'tab-bar-tab-name
  "Function to get a tab name.
Function gets no arguments.
By default, use function `tab-bar-tab-name'.")

(defun tab-bar-tab-name ()
  "Generate tab name in the context of the selected frame."
  (mapconcat #'buffer-name
             (delete-dups (mapcar #'window-buffer
                                  (window-list-1 (frame-first-window)
                                                 'nomini)))
             ", "))

(defvar tab-bar-tabs-function #'tab-bar-tabs
  "Function to get a list of tabs to display in the tab bar.
This function should return a list of alists with parameters
that include at least the element (name . TAB-NAME).
For example, '((tab (name . \"Tab 1\")) (current-tab (name . \"Tab 2\")))
By default, use function `tab-bar-tabs'.")

(defun tab-bar-tabs ()
  "Return a list of tabs belonging to the selected frame.
Ensure the frame parameter `tabs' is pre-populated.
Return its existing value or a new value."
  (let ((tabs (frame-parameter nil 'tabs)))
    (if tabs
        ;; Update current tab name
        (let ((name (assq 'name (assq 'current-tab tabs))))
          (when name (setcdr name (funcall tab-bar-tab-name-function))))
      ;; Create default tabs
      (setq tabs `((current-tab (name . ,(funcall tab-bar-tab-name-function)))))
      (set-frame-parameter nil 'tabs tabs))
    tabs))

(defun tab-bar-make-keymap-1 ()
  "Generate an actual keymap from `tab-bar-map', without caching."
  (let ((separator (or tab-bar-separator (if window-system " " "|")))
        (i 0))
    (append
     '(keymap (mouse-1 . tab-bar-handle-mouse))
     (mapcan
      (lambda (tab)
        (setq i (1+ i))
        (append
         `((,(intern (format "sep-%i" i)) menu-item ,separator ignore))
         (cond
          ((eq (car tab) 'current-tab)
           `((current-tab
              menu-item
              ,(propertize (concat (cdr (assq 'name tab))
                                   (or (and tab-bar-close-button-show
                                            (not (eq tab-bar-close-button-show
                                                     'non-selected))
                                            tab-bar-close-button) ""))
                           'face 'tab-bar-tab)
              ignore
              :help "Current tab")))
          (t
           `((,(intern (format "tab-%i" i))
              menu-item
              ,(propertize (concat (cdr (assq 'name tab))
                                   (or (and tab-bar-close-button-show
                                            (not (eq tab-bar-close-button-show
                                                     'selected))
                                            tab-bar-close-button) ""))
                           'face 'tab-bar-tab-inactive)
              ,(or
                (cdr (assq 'binding tab))
                (lambda ()
                  (interactive)
                  (tab-bar-select-tab tab)))
              :help "Click to visit tab"))))
         `((,(if (eq (car tab) 'current-tab) 'C-current-tab (intern (format "C-tab-%i" i)))
            menu-item ""
            ,(or
              (cdr (assq 'close-binding tab))
              (lambda ()
                (interactive)
                (tab-bar-close-tab tab)))))))
      (funcall tab-bar-tabs-function))
     (when tab-bar-new-button
       `((sep-add-tab menu-item ,separator ignore)
         (add-tab menu-item ,tab-bar-new-button tab-bar-new-tab
                  :help "New tab"))))))


(defun tab-bar-read-tab-name (prompt)
  (let* ((tabs (tab-bar-tabs))
         (tab-name
          (completing-read prompt
                           (or (delq nil (mapcar (lambda (tab)
                                                   (cdr (assq 'name tab)))
                                                 tabs))
                               '("")))))
    (catch 'done
      (dolist (tab tabs)
        (when (equal (cdr (assq 'name tab)) tab-name)
          (throw 'done tab))))))

(defun tab-bar-tab-default ()
  (let ((tab `(tab
               (name . ,(funcall tab-bar-tab-name-function))
               (time . ,(time-convert nil 'integer))
               (wc . ,(current-window-configuration))
               (ws . ,(window-state-get
                       (frame-root-window (selected-frame)) 'writable)))))
    tab))

(defun tab-bar-find-prev-tab (&optional tabs)
  (unless tabs
    (setq tabs (tab-bar-tabs)))
  (unless (eq (car (car tabs)) 'current-tab)
    (while (and tabs (not (eq (car (car (cdr tabs))) 'current-tab)))
      (setq tabs (cdr tabs)))
    tabs))


(defun tab-bar-select-tab (tab)
  "Switch to the specified TAB."
  (interactive (list (tab-bar-read-tab-name "Select tab by name: ")))
  (when (and tab (not (eq (car tab) 'current-tab)))
    (let* ((tabs (tab-bar-tabs))
           (new-tab (tab-bar-tab-default))
           (wc (cdr (assq 'wc tab))))
      ;; During the same session, use window-configuration to switch
      ;; tabs, because window-configurations are more reliable
      ;; (they keep references to live buffers) than window-states.
      ;; But after restoring tabs from a previously saved session,
      ;; its value of window-configuration is unreadable,
      ;; so restore its saved window-state.
      (if (window-configuration-p wc)
          (set-window-configuration wc)
        (window-state-put (cdr (assq 'ws tab))
                          (frame-root-window (selected-frame)) 'safe))
      (while tabs
        (cond
         ((eq (car tabs) tab)
          (setcar tabs `(current-tab (name . ,(funcall tab-bar-tab-name-function)))))
         ((eq (car (car tabs)) 'current-tab)
          (setcar tabs new-tab)))
        (setq tabs (cdr tabs)))
      (force-mode-line-update))))

(defun tab-bar-switch-to-prev-tab (&optional _arg)
  "Switch to ARGth previous tab."
  (interactive "p")
  (let ((prev-tab (tab-bar-find-prev-tab)))
    (when prev-tab
      (tab-bar-select-tab (car prev-tab)))))

(defun tab-bar-switch-to-next-tab (&optional _arg)
  "Switch to ARGth next tab."
  (interactive "p")
  (let* ((tabs (tab-bar-tabs))
         (prev-tab (tab-bar-find-prev-tab tabs)))
    (if prev-tab
        (tab-bar-select-tab (car (cdr (cdr prev-tab))))
      (tab-bar-select-tab (car (cdr tabs))))))


(defcustom tab-bar-new-tab-to 'right
  "Defines where to create a new tab.
If `leftmost', create as the first tab.
If `left', create to the left from the current tab.
If `right', create to the right from the current tab.
If `rightmost', create as the last tab."
  :type '(choice (const :tag "First tab" leftmost)
                 (const :tag "To the left" left)
                 (const :tag "To the right" right)
                 (const :tag "Last tab" rightmost))
  :group 'tab-bar
  :version "27.1")

(defun tab-bar-new-tab ()
  "Clone the current tab to the position specified by `tab-bar-new-tab-to'."
  (interactive)
  (unless tab-bar-mode
    (tab-bar-mode 1))
  (let* ((tabs (tab-bar-tabs))
         ;; (i-tab (- (length tabs) (length (memq tab tabs))))
         (new-tab (tab-bar-tab-default)))
    (cond
     ((eq tab-bar-new-tab-to 'leftmost)
      (setq tabs (cons new-tab tabs)))
     ((eq tab-bar-new-tab-to 'rightmost)
      (setq tabs (append tabs (list new-tab))))
     (t
      (let ((prev-tab (tab-bar-find-prev-tab tabs)))
        (cond
         ((eq tab-bar-new-tab-to 'left)
          (if prev-tab
              (setcdr prev-tab (cons new-tab (cdr prev-tab)))
            (setq tabs (cons new-tab tabs))))
         ((eq tab-bar-new-tab-to 'right)
          (if prev-tab
              (setq prev-tab (cdr prev-tab))
            (setq prev-tab tabs))
          (setcdr prev-tab (cons new-tab (cdr prev-tab))))))))
    (set-frame-parameter nil 'tabs tabs)
    (tab-bar-select-tab new-tab)
    (when tab-bar-new-tab-choice
      (delete-other-windows)
      (let ((buffer
             (if (functionp tab-bar-new-tab-choice)
                 (funcall tab-bar-new-tab-choice)
               (if (stringp tab-bar-new-tab-choice)
                   (or (get-buffer tab-bar-new-tab-choice)
                       (find-file-noselect tab-bar-new-tab-choice))))))
        (when (buffer-live-p buffer)
          (switch-to-buffer buffer))))
    (unless tab-bar-mode
      (message "Added new tab with the current window configuration"))))


(defcustom tab-bar-close-tab-select 'right
  "Defines what tab to select after closing the specified tab.
If `left', select the adjacent left tab.
If `right', select the adjacent right tab."
  :type '(choice (const :tag "Select left tab" left)
                 (const :tag "Select right tab" right))
  :group 'tab-bar
  :version "27.1")

(defun tab-bar-close-current-tab (&optional tab select-tab)
  "Close the current TAB.
After closing the current tab switch to the tab
specified by `tab-bar-close-tab-select', or to `select-tab'
if its value is provided."
  (interactive)
  (let ((tabs (tab-bar-tabs)))
    (unless tab
      (let ((prev-tab (tab-bar-find-prev-tab tabs)))
        (setq tab (if prev-tab
                      (car (cdr prev-tab))
                    (car tabs)))))
    (if select-tab
        (setq tabs (delq tab tabs))
      (let* ((i-tab (- (length tabs) (length (memq tab tabs))))
             (i-select
              (cond
               ((eq tab-bar-close-tab-select 'left)
                (1- i-tab))
               ((eq tab-bar-close-tab-select 'right)
                ;; Do nothing: the next tab will take
                ;; the index of the closed tab
                i-tab)
               (t 0))))
        (setq tabs (delq tab tabs)
              i-select (max 0 (min (1- (length tabs)) i-select))
              select-tab (nth i-select tabs))))
    (set-frame-parameter nil 'tabs tabs)
    (tab-bar-select-tab select-tab)))

(defun tab-bar-close-tab (tab)
  "Close the specified TAB.
After closing the current tab switch to the tab
specified by `tab-bar-close-tab-select'."
  (interactive (list (tab-bar-read-tab-name "Close tab by name: ")))
  (when tab
    (if (eq (car tab) 'current-tab)
        (tab-bar-close-current-tab tab)
      ;; Close non-current tab, no need to switch to another tab
      (set-frame-parameter nil 'tabs (delq tab (tab-bar-tabs)))
      (force-mode-line-update))))


;;; Non-graphical access to frame-local tabs (named window configurations)

(defun tab-make ()
  "Create a new named window configuration without having to click a tab."
  (interactive)
  (tab-bar-new-tab)
  (unless tab-bar-mode
    (message "Added new tab with the current window configuration")))

(defun tab-delete ()
  "Delete the current window configuration without clicking a close button."
  (interactive)
  (tab-bar-close-current-tab)
  (unless tab-bar-mode
    (message "Deleted the current tab")))

;; Short aliases
;; (defalias 'tab-switch 'tab-bar-switch-to-next-tab)
(defalias 'tab-select 'tab-bar-select-tab)
(defalias 'tab-previous 'tab-bar-switch-to-prev-tab)
(defalias 'tab-next 'tab-bar-switch-to-next-tab)

(defun tab-bar-list ()
  "Display a list of named window configurations.
The list is displayed in the buffer `*Tabs*'.

In this list of window configurations you can delete or select them.
Type ? after invocation to get help on commands available.
Type q to remove the list of window configurations from the display.

The first column shows `D' for for a window configuration you have
marked for deletion."
  (interactive)
  (let ((dir default-directory)
        (minibuf (minibuffer-selected-window)))
    (let ((tab-bar-mode t)) ; don't enable tab-bar-mode if it's disabled
      (tab-bar-new-tab))
    ;; Handle the case when it's called in the active minibuffer.
    (when minibuf (select-window (minibuffer-selected-window)))
    (delete-other-windows)
    ;; Create a new window to replace the existing one, to not break the
    ;; window parameters (e.g. prev/next buffers) of the window just saved
    ;; to the window configuration.  So when a saved window is restored,
    ;; its parameters left intact.
    (split-window) (delete-window)
    (let ((switch-to-buffer-preserve-window-point nil))
      (switch-to-buffer (tab-bar-list-noselect)))
    (setq default-directory dir))
  (message "Commands: d, x; RET; q to quit; ? for help."))

(defun tab-bar-list-noselect ()
  "Create and return a buffer with a list of window configurations.
The list is displayed in a buffer named `*Tabs*'.

For more information, see the function `tab-bar-list'."
  (let* ((tabs (delq nil (mapcar (lambda (tab) ; remove current tab
                                   (unless (eq (car tab) 'current-tab)
                                     tab))
                                 (tab-bar-tabs))))
         ;; Sort by recency
         (tabs (sort tabs (lambda (a b) (< (cdr (assq 'time b))
                                           (cdr (assq 'time a)))))))
    (with-current-buffer (get-buffer-create
                          (format " *Tabs*<%s>" (or (frame-parameter nil 'window-id)
                                                    (frame-parameter nil 'name))))
      (erase-buffer)
      (tab-bar-list-mode)
      (setq buffer-read-only nil)
      ;; Vertical alignment to the center of the frame
      (insert-char ?\n (/ (- (frame-height) (length tabs) 1) 2))
      ;; Horizontal alignment to the center of the frame
      (setq tab-bar-list-column (- (/ (frame-width) 2) 15))
      (dolist (tab tabs)
        (insert (propertize
                 (format "%s %s\n"
                         (make-string tab-bar-list-column ?\040)
                         (propertize
                          (cdr (assq 'name tab))
                          'mouse-face 'highlight
                          'help-echo "mouse-2: select this window configuration"))
                 'tab tab)))
      (goto-char (point-min))
      (goto-char (or (next-single-property-change (point) 'tab) (point-min)))
      (when (> (length tabs) 1)
        (tab-bar-list-next-line))
      (move-to-column tab-bar-list-column)
      (set-buffer-modified-p nil)
      (current-buffer))))

(defvar tab-bar-list-column 3)
(make-variable-buffer-local 'tab-bar-list-column)

(defvar tab-bar-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "q"    'quit-window)
    (define-key map "\C-m" 'tab-bar-list-select)
    (define-key map "d"    'tab-bar-list-delete)
    (define-key map "k"    'tab-bar-list-delete)
    (define-key map "\C-d" 'tab-bar-list-delete-backwards)
    (define-key map "\C-k" 'tab-bar-list-delete)
    (define-key map "x"    'tab-bar-list-execute)
    (define-key map " "    'tab-bar-list-next-line)
    (define-key map "n"    'tab-bar-list-next-line)
    (define-key map "p"    'tab-bar-list-prev-line)
    (define-key map "\177" 'tab-bar-list-backup-unmark)
    (define-key map "?"    'describe-mode)
    (define-key map "u"    'tab-bar-list-unmark)
    (define-key map [mouse-2] 'tab-bar-list-mouse-select)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for `tab-bar-list-mode' buffers.")

(define-derived-mode tab-bar-list-mode nil "Window Configurations"
  "Major mode for selecting a window configuration.
Each line describes one window configuration in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<tab-bar-list-mode-map>
\\[tab-bar-list-mouse-select] -- select window configuration you click on.
\\[tab-bar-list-select] -- select current line's window configuration.
\\[tab-bar-list-delete] -- mark that window configuration to be deleted, and move down.
\\[tab-bar-list-delete-backwards] -- mark that window configuration to be deleted, and move up.
\\[tab-bar-list-execute] -- delete marked window configurations.
\\[tab-bar-list-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[tab-bar-list-backup-unmark] -- back up a line and remove marks."
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun tab-bar-list-current-tab (error-if-non-existent-p)
  "Return window configuration described by this line of the list."
  (let* ((where (save-excursion
                  (beginning-of-line)
                  (+ 2 (point) tab-bar-list-column)))
         (tab (and (not (eobp)) (get-text-property where 'tab))))
    (or tab
        (if error-if-non-existent-p
            (user-error "No window configuration on this line")
          nil))))


(defun tab-bar-list-next-line (&optional arg)
  (interactive)
  (forward-line arg)
  (beginning-of-line)
  (move-to-column tab-bar-list-column))

(defun tab-bar-list-prev-line (&optional arg)
  (interactive)
  (forward-line (- arg))
  (beginning-of-line)
  (move-to-column tab-bar-list-column))

(defun tab-bar-list-unmark (&optional backup)
  "Cancel all requested operations on window configuration on this line and move down.
Optional prefix arg means move up."
  (interactive "P")
  (beginning-of-line)
  (move-to-column tab-bar-list-column)
  (let* ((buffer-read-only nil))
    (delete-char 1)
    (insert " "))
  (forward-line (if backup -1 1))
  (move-to-column tab-bar-list-column))

(defun tab-bar-list-backup-unmark ()
  "Move up and cancel all requested operations on window configuration on line above."
  (interactive)
  (forward-line -1)
  (tab-bar-list-unmark)
  (forward-line -1)
  (move-to-column tab-bar-list-column))

(defun tab-bar-list-delete (&optional arg)
  "Mark window configuration on this line to be deleted by \\<tab-bar-list-mode-map>\\[tab-bar-list-execute] command.
Prefix arg is how many window configurations to delete.
Negative arg means delete backwards."
  (interactive "p")
  (let ((buffer-read-only nil))
    (if (or (null arg) (= arg 0))
        (setq arg 1))
    (while (> arg 0)
      (delete-char 1)
      (insert ?D)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (delete-char 1)
      (insert ?D)
      (forward-line -1)
      (setq arg (1+ arg)))
    (move-to-column tab-bar-list-column)))

(defun tab-bar-list-delete-backwards (&optional arg)
  "Mark window configuration on this line to be deleted by \\<tab-bar-list-mode-map>\\[tab-bar-list-execute] command.
Then move up one line.  Prefix arg means move that many lines."
  (interactive "p")
  (tab-bar-list-delete (- (or arg 1))))

(defun tab-bar-list-delete-from-list (tab)
  "Delete the window configuration from both lists."
  (set-frame-parameter nil 'tabs (delq tab (tab-bar-tabs))))

(defun tab-bar-list-execute ()
  "Delete window configurations marked with \\<tab-bar-list-mode-map>\\[tab-bar-list-delete] commands."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (while (re-search-forward
              (format "^%sD" (make-string tab-bar-list-column ?\040))
              nil t)
        (forward-char -1)
        (let ((tab (tab-bar-list-current-tab nil)))
          (when tab
            (tab-bar-list-delete-from-list tab)
            (beginning-of-line)
            (delete-region (point) (progn (forward-line 1) (point))))))))
  (beginning-of-line)
  (move-to-column tab-bar-list-column)
  (when tab-bar-mode
    (force-mode-line-update)))

(defun tab-bar-list-select ()
  "Select this line's window configuration.
This command deletes and replaces all the previously existing windows
in the selected frame."
  (interactive)
  (let* ((select-tab (tab-bar-list-current-tab t)))
    (kill-buffer (current-buffer))
    ;; Delete the current window configuration
    (tab-bar-close-current-tab nil select-tab)
    ;; (tab-bar-select-tab select-tab)
    ))

(defun tab-bar-list-mouse-select (event)
  "Select the window configuration whose line you click on."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (tab-bar-list-select))


(defvar ctl-x-6-map (make-sparse-keymap)
  "Keymap for tab commands.")
(defalias 'ctl-x-6-prefix ctl-x-6-map)
(define-key ctl-x-map "6" 'ctl-x-6-prefix)

(defun switch-to-buffer-other-tab (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another tab.
Like \\[switch-to-buffer-other-frame] (which see), but creates a new tab."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other tab: ")))
  (tab-bar-new-tab)
  (delete-other-windows)
  (switch-to-buffer buffer-or-name norecord))

(defun find-file-other-tab (filename &optional wildcards)
  "Edit file FILENAME, in another tab.
Like \\[find-file-other-frame] (which see), but creates a new tab."
  (interactive
   (find-file-read-args "Find file in other tab: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
        (progn
          (setq value (nreverse value))
          (switch-to-buffer-other-tab (car value))
          (mapc 'switch-to-buffer (cdr value))
          value)
      (switch-to-buffer-other-tab value))))

(define-key ctl-x-6-map "2" 'tab-bar-new-tab)
(define-key ctl-x-6-map "0" 'tab-bar-close-current-tab)
(define-key ctl-x-6-map "b" 'switch-to-buffer-other-tab)
(define-key ctl-x-6-map "f" 'find-file-other-tab)
(define-key ctl-x-6-map "\C-f" 'find-file-other-tab)


(provide 'tab-bar)

;;; tab-bar.el ends here

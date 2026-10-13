;;; tab-bar-tree.el --- The tab bar's tabs as a tree in a side window -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; Prototype.  Shows the tab bar's tabs, one per line and grouped by
;; tab group, in a fixed-width side window.  Groups can be folded.
;;
;; Labels come from `tab-bar-tab-name-format-function' and
;; `tab-bar-tab-group-format-function', so the tree shows the same
;; hints, marks, indicators and colors as the tab bar.
;;
;; The tab bar itself is not touched: its visibility and contents stay
;; under its own options.

;;; Code:

(require 'tab-bar)
(require 'face-remap)

(defgroup tab-bar-tree nil
  "The tab bar's tabs as a tree in a side window."
  :group 'tab-bar)

(defcustom tab-bar-tree-side 'left
  "Side of the frame to show the tree on."
  :type '(choice (const left) (const right))
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-indent 3
  "Indentation of a grouped tab, in columns."
  :type 'natnum
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-tab-background nil
  "Non-nil means labels keep the background of their faces.
By default the tree draws labels on the default background; the
current tab is told by its indicator.  Takes effect on the next render,
e.g. \\[tab-bar-tree-refresh]."
  :type 'boolean
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-width 32
  "Width of the tree window, in columns.
Applied on every render; a frame narrower than twice this value gets no
tree."
  :type 'natnum
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-header-left-function #'tab-bar-tree-header-group
  "Function of the current tab returning the header's left part.
The result is cut to fit the window."
  :type 'function
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-header-right-function #'ignore
  "Function of the current tab returning the header's right part.
The part is flush right; nil means none."
  :type 'function
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-refresh-commands
  '(tab-bar-rename-tab)
  "Commands that modify a tab in place; the tree is refreshed after each.
Other tab changes are caught by tab-bar hooks and `tab-bar-tabs-set'.
Read when the mode is enabled."
  :type '(repeat function)
  :group 'tab-bar-tree)

(defvar tab-bar-tree--hooks
  '((tab-bar-tab-post-select-functions . tab-bar-tree--show)
    (tab-bar-tab-post-open-functions . tab-bar-tree--show)
    (tab-bar-tab-post-change-group-functions . tab-bar-tree--show)
    ;; Label colors depend on the theme.
    (enable-theme-functions . tab-bar-tree--theme)
    (disable-theme-functions . tab-bar-tree--theme)
    ;; Window parameters are not restored from a desktop file.
    (desktop-after-read-hook . tab-bar-tree--show))
  "Hooks the mode adds a function to, as (HOOK . FUNCTION).")
;; No window change hooks: they run during redisplay.  As a result, the
;; name of a tab without an explicit name is updated on tab events only.

(defvar tab-bar-tree-buffer "*tab-bar-tree*"
  "Name of the tree buffer.")

(defun tab-bar-tree--buffer ()
  "Return the tree buffer, creating it if needed."
  (or (get-buffer tab-bar-tree-buffer)
      (with-current-buffer (get-buffer-create tab-bar-tree-buffer)
        (tab-bar-tree-mode)
        (current-buffer))))

(defvar tab-bar-tree-collapsed nil
  "Names of the folded groups.")

(defvar-keymap tab-bar-tree-mode-map
  :doc "Keymap for `tab-bar-tree-mode'."
  :parent special-mode-map
  "RET"     #'tab-bar-tree-select
  "TAB"     #'tab-bar-tree-select
  ;; `mouse-drag-region' on the press would select the tree window.
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'tab-bar-tree-select
  ;; Otherwise a double click toggles a group twice.
  "<double-mouse-1>" #'ignore
  "<triple-mouse-1>" #'ignore
  "n"       #'next-line
  "p"       #'previous-line
  "g"       #'tab-bar-tree-refresh)

(defun tab-bar-tree-select (&optional event)
  "Select the tab on the current line, or toggle the group on it.
EVENT is the mouse event, if any."
  (interactive (list last-input-event))
  (let* ((position (and (mouse-event-p event) (event-start event)))
         (window (if position (posn-window position) (selected-window)))
         ;; Read the property at line start, wherever point is.
         (line (with-selected-window window
                 (when position (goto-char (posn-point position)))
                 (get-text-property (line-beginning-position) 'tab-bar-tree))))
    ;; Leave the tree window before selecting a tab, so it is not saved
    ;; as the selected window of the old tab.  Toggling a group from the
    ;; keyboard keeps focus.  `get-mru-window' skips the tree window: it
    ;; is dedicated, `no-other-window' and selected.
    (when (and (eq window (selected-window))
               (or position (eq (car-safe line) 'tab)))
      (when-let* ((other (get-mru-window nil nil t t)))
        (select-window other)))
    (pcase line
      (`(tab . ,i) (tab-bar-select-tab i))
      (`(group . ,group)
       (setq tab-bar-tree-collapsed
             (if (member group tab-bar-tree-collapsed)
                 (delete group tab-bar-tree-collapsed)
               (cons group tab-bar-tree-collapsed)))
       (tab-bar-tree--update)))))

(defun tab-bar-tree--group-string (tab i current-p open-p)
  "Return the group label of TAB at index I.
CURRENT-P is passed to the formatter; OPEN-P is passed via
`tab-bar-show-inactive-group-tabs', which the formatter reads."
  (let ((tab-bar-show-inactive-group-tabs open-p))
    (funcall tab-bar-tab-group-format-function tab i current-p)))

(defun tab-bar-tree--goto-line (line)
  "Move point to the start of LINE and return point."
  (goto-char (point-min))
  (forward-line (1- line))
  (point))

(defun tab-bar-tree--render ()
  "Render the selected frame's tabs into the tree buffer and return it."
  (let* ((buf (tab-bar-tree--buffer))
         (tabs (funcall tab-bar-tabs-function))
         (current-group (funcall tab-bar-tab-group-function
                                 (assq 'current-tab tabs))))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             ;; Saved as line numbers: `erase-buffer' resets every
             ;; window's start and point.
             (line (line-number-at-pos))
             (windows (mapcar (lambda (window)
                                (list window
                                      (line-number-at-pos (window-start window))
                                      (line-number-at-pos (window-point window))))
                              (get-buffer-window-list buf nil t)))
             (previous-group nil)
             (i 0))
        ;; The current group is never folded, as on the tab bar.
        (setq tab-bar-tree-collapsed (delete current-group tab-bar-tree-collapsed))
        (erase-buffer)
        (dolist (tab tabs)
          (setq i (1+ i))
          (let* ((group (funcall tab-bar-tab-group-function tab))
                 (current-p (eq (car tab) 'current-tab))
                 (collapsed (and group (member group tab-bar-tree-collapsed))))
            (unless (equal group previous-group)
              (setq previous-group group)
              (when group
                (insert (propertize (tab-bar-tree--group-string
                                     tab i (equal group current-group)
                                     (not collapsed))
                                    'tab-bar-tree (cons 'group group)
                                    'mouse-face 'highlight)
                        "\n")))
            ;; A folded group still shows the current tab.
            (unless (and collapsed (not current-p))
              ;; The property covers the indentation too, so RET at
              ;; column 0 works.
              (insert (propertize
                       (concat (make-string (if group tab-bar-tree-indent 0) ?\s)
                               (funcall tab-bar-tab-name-format-function tab i))
                       'tab-bar-tree (cons 'tab i)
                       'mouse-face 'highlight)
                      "\n"))))
        (pcase-dolist (`(,window ,start ,point) windows)
          (set-window-start window (tab-bar-tree--goto-line start) t)
          (set-window-point window (tab-bar-tree--goto-line point)))
        (tab-bar-tree--goto-line line))
      ;; Above the first tab, so an ungrouped one does not read as a
      ;; group label.
      (let* ((tab (assq 'current-tab tabs))
             (right (or (funcall tab-bar-tree-header-right-function tab) ""))
             (left (funcall tab-bar-tree-header-left-function tab)))
        (setq header-line-format
              (list
               " "
               ;; `%' escaped: the header line is a mode line construct.
               (string-replace
                "%" "%%"
                ;; The window is pinned to `tab-bar-tree-width' columns;
                ;; its text area loses the fringe and the rest.
                (tab-bar-tree--header-fit
                 left
                 (- (* tab-bar-tree-width (frame-char-width))
                    left-fringe-width
                    (tab-bar-tree--header-width
                     (if (string-empty-p right) "  " (concat "  " right " "))))))
               'mode-line-format-right-align
               right
               " ")))
      (tab-bar-tree--remap-faces))
    buf))

(defun tab-bar-tree-header-group (tab)
  "Return TAB's group in bold, or a dash if none."
  (if-let* ((group (funcall tab-bar-tab-group-function tab)))
      (propertize group 'face 'bold)
    (propertize "—" 'face 'shadow)))

(defun tab-bar-tree--header-width (string)
  "Return STRING's pixel width in the tree's header line."
  (let ((string (copy-sequence string)))
    (add-face-text-property 0 (length string) 'header-line t string)
    (string-pixel-width string (tab-bar-tree--buffer))))

(defun tab-bar-tree--header-fit (string pixels)
  "Return STRING cut with an ellipsis to PIXELS in the tree's header line."
  (let ((string (copy-sequence string)))
    (add-face-text-property 0 (length string) 'header-line t string)
    (truncate-string-pixelwise string pixels (tab-bar-tree--buffer)
                               (truncate-string-ellipsis))))

(defvar-local tab-bar-tree--remap-cookies nil
  "Face remapping cookies added by `tab-bar-tree--remap-faces'.")

(defvar-local tab-bar-tree--remap-background nil
  "Background the label faces are remapped to, or nil.")

(defvar tab-bar-tree--faces
  '(tab-bar tab-bar-tab tab-bar-tab-inactive tab-bar-tab-ungrouped
    tab-bar-tab-group-current tab-bar-tab-group-inactive)
  "Faces whose background is remapped to the default background.")

(defun tab-bar-tree--remap-faces ()
  "Remap label backgrounds according to `tab-bar-tree-tab-background'.
Called on every render; remaps only when the target background changes."
  (let ((background (unless tab-bar-tree-tab-background
                      (face-background 'default nil 'default))))
    (unless (equal background tab-bar-tree--remap-background)
      (mapc #'face-remap-remove-relative tab-bar-tree--remap-cookies)
      (setq tab-bar-tree--remap-cookies nil
            tab-bar-tree--remap-background background)
      (when background
        (dolist (face tab-bar-tree--faces)
          (push (face-remap-add-relative face :background background)
                tab-bar-tree--remap-cookies))))))

(define-derived-mode tab-bar-tree-mode special-mode "TabTree"
  "Major mode of the tree buffer."
  (setq cursor-type nil
        truncate-lines t
        mode-line-format nil
        header-line-format nil)
  ;; Lines never wrap: no margins and no right fringe.  A narrow left
  ;; fringe keeps labels off the window edge.
  (setq-local left-fringe-width 4
              right-fringe-width 0
              left-margin-width 0
              right-margin-width 0)
  ;; As in `speedbar-window-mode': no horizontal scrolling in a narrow
  ;; window.
  (setq-local auto-hscroll-mode nil)
  (setq-local window-size-fixed 'width)
  ;; `tab-bar-tree--delete-window' refuses while the mode is on, which
  ;; would leave the side window showing another buffer.
  (add-hook 'kill-buffer-hook #'tab-bar-tree--kill-buffer nil t))

(defun tab-bar-tree--delete-window (window)
  "Delete WINDOW if `global-tab-bar-tree-mode' is off.
Value of the `delete-window' parameter.  Lets a tree window restored
from a window configuration saved while the mode was on be deleted."
  (unless global-tab-bar-tree-mode
    (tab-bar-tree--forget-window window)))

(defvar tab-bar-tree--window-parameters
  '((no-other-window . t)
    (no-delete-other-windows . t)
    (delete-window . tab-bar-tree--delete-window))
  "Parameters of the tree window.")

(defvar tab-bar-tree--persistent-parameters nil
  "Parameters the mode added to `window-persistent-parameters'.")

(defun tab-bar-tree--window (buffer)
  "Show BUFFER in the selected frame's tree window and return the window.
Reuses an existing window, re-applying its width, parameters and
dedication: a window restored from a desktop file or a tab's `ws' has no
parameters, and `quit-restore-window' may un-dedicate it."
  (when-let* ((window (display-buffer-in-side-window
                       buffer
                       `((side . ,tab-bar-tree-side)
                         (slot . 0)
                         (window-width . ,tab-bar-tree-width)
                         ;; `side', not t: `quit-restore-window' deletes
                         ;; a window dedicated with t outright.
                         (dedicated . side)
                         (window-parameters
                          . ,tab-bar-tree--window-parameters))))
              ;; Pixelwise: a tab's window configuration may restore a
              ;; width off by less than a column.
              (delta (- (* tab-bar-tree-width (frame-char-width))
                        (window-pixel-width window))))
    (unless (zerop delta)
      ;; IGNORE = WINDOW: skip its own `window-size-fixed'.
      (window-resize window delta t window t))
    window))

(defvar tab-bar-tree--updating nil
  "Non-nil while the tree is being drawn.
The first read of a frame's tabs runs `tab-bar-tab-post-open-functions',
which would draw the tree recursively.")

(defun tab-bar-tree--windows ()
  "Return the selected frame's windows showing the tree buffer."
  (when-let* ((buffer (get-buffer tab-bar-tree-buffer)))
    (get-buffer-window-list buffer nil (selected-frame))))

(defun tab-bar-tree--draw ()
  "Render the tree and show it in the selected frame; return the window."
  (unless tab-bar-tree--updating
    (let ((tab-bar-tree--updating t))
      (tab-bar-tree--window (tab-bar-tree--render)))))

(defun tab-bar-tree--show (&rest _)
  "Render the tree and show it in the selected frame; return the window.
Return nil on a child, minibuffer-only, unsplittable or narrow frame."
  (let ((frame (selected-frame)))
    (unless (or (frame-parent frame)
                (eq (frame-parameter frame 'minibuffer) 'only)
                (frame-parameter frame 'unsplittable)
                (and (not (tab-bar-tree--windows))
                     (< (frame-width frame) (* 2 tab-bar-tree-width))))
      (tab-bar-tree--draw))))

(defun tab-bar-tree--update (&rest _)
  "Re-render the tree if it is shown in the selected frame."
  (when (tab-bar-tree--windows)
    (tab-bar-tree--draw)))

(defun tab-bar-tree--theme (theme)
  "Re-render the tree after THEME is enabled or disabled."
  ;; `enable-theme' also enables `user' for every theme.
  (unless (eq theme 'user)
    (tab-bar-tree--update)))

;;;###autoload
(defun tab-bar-tree-refresh ()
  "Render the tree and show it."
  (interactive)
  (tab-bar-tree--show))

;;;###autoload
(defun tab-bar-tree-get-focus ()
  "Select the tree window, showing it first if needed.
Clicks in the tree do not select its window; this command does."
  (interactive)
  (when-let* ((window (tab-bar-tree--show)))
    (select-window window)))

(defun tab-bar-tree--forget-window (window)
  "Delete WINDOW, ignoring its `delete-window' parameter."
  (let ((ignore-window-parameters t))
    (delete-window window)))

(defun tab-bar-tree--forget-windows ()
  "Delete the selected frame's tree windows; return non-nil if any."
  (let ((windows (tab-bar-tree--windows)))
    (mapc #'tab-bar-tree--forget-window windows)
    windows))

(defun tab-bar-tree--kill-buffer ()
  "Delete the windows showing the current buffer, which is being killed."
  (mapc #'tab-bar-tree--forget-window
        (get-buffer-window-list (current-buffer) nil t)))

(defun tab-bar-tree--strip-tab (tab)
  "Remove the tree window from TAB's window configuration and state.
Installs TAB's windows in the selected frame; the caller restores them."
  (let ((wc (alist-get 'wc tab))
        (ws (alist-get 'ws tab))
        ;; Same binding as in `tab-bar-select-tab'.
        (window-restore-killed-buffer-windows
         (or tab-bar-select-restore-windows
             window-restore-killed-buffer-windows)))
    (cond
     ((and (window-configuration-p wc)
           (eq (window-configuration-frame wc) (selected-frame)))
      (set-window-configuration wc nil t))
     (ws
      (window-state-put ws (frame-root-window) 'safe)))
    (when (tab-bar-tree--forget-windows)
      ;; Update both: desktop and `tab-bar-move-tab-to-frame' use `ws'.
      (when-let* ((cell (assq 'wc tab)))
        (setcdr cell (current-window-configuration)))
      (when-let* ((cell (assq 'ws tab)))
        (setcdr cell (window-state-get (frame-root-window) 'writable))))))

(defun tab-bar-tree--strip-tabs ()
  "Remove the tree window from the other tabs of all frames and closed tabs.
The current tabs keep it; killing the tree buffer deletes it there."
  ;; Without `tab-bar-select-tab', which would run
  ;; `tab-bar-tab-post-select-functions' (e.g. a theme switch) per tab.
  (dolist (frame (frame-list))
    (unless (frame-parent frame)
      (with-selected-frame frame
        (when (frame-parameter frame 'tabs)
          (save-excursion
            (let ((restore (current-window-configuration)))
              (unwind-protect
                  (progn
                    (dolist (tab (funcall tab-bar-tabs-function))
                      (unless (eq (car tab) 'current-tab)
                        (tab-bar-tree--strip-tab tab)))
                    (dolist (closed tab-bar-closed-tabs)
                      (when (eq (alist-get 'frame closed) frame)
                        (tab-bar-tree--strip-tab (alist-get 'tab closed)))))
                (set-window-configuration restore nil t)))))))))

;;;###autoload
(define-minor-mode global-tab-bar-tree-mode
  "Show the tabs of the tab bar as a tree in a side window."
  :global t
  ;; Depth 90: run after the hooks' other functions, e.g.
  ;; `tab-bar-move-tab-to-group' or a mark being cleared.
  (pcase-dolist (`(,hook . ,function) tab-bar-tree--hooks)
    (if global-tab-bar-tree-mode
        (add-hook hook function 90)
      (remove-hook hook function)))
  ;; Closing and moving tabs run no hook but end in `tab-bar-tabs-set'.
  (dolist (symbol (cons 'tab-bar-tabs-set tab-bar-tree-refresh-commands))
    (if global-tab-bar-tree-mode
        (advice-add symbol :after #'tab-bar-tree--update)
      (advice-remove symbol #'tab-bar-tree--update)))
  (if global-tab-bar-tree-mode
      (progn
        ;; Value t: kept in window states (e.g. by
        ;; `window-toggle-side-windows'), not in desktop files.
        (dolist (parameter (mapcar #'car tab-bar-tree--window-parameters))
          (unless (assq parameter window-persistent-parameters)
            (push (cons parameter t) window-persistent-parameters)
            (push parameter tab-bar-tree--persistent-parameters)))
        (tab-bar-tree--show))
    (dolist (parameter tab-bar-tree--persistent-parameters)
      (setq window-persistent-parameters
            (assq-delete-all parameter window-persistent-parameters)))
    (setq tab-bar-tree--persistent-parameters nil)
    ;; Strip the window from all tabs before killing the buffer;
    ;; otherwise saved configurations restore an "Old buffer" window.
    (when-let* ((buffer (get-buffer tab-bar-tree-buffer)))
      (tab-bar-tree--strip-tabs)
      (kill-buffer buffer))))

(provide 'tab-bar-tree)
;;; tab-bar-tree.el ends here

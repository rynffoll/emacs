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

(defcustom tab-bar-tree-display-alist
  '((side . left) (slot . -100) (window-width . 32))
  "Display parameters for `display-buffer-in-side-window'.
`window-width', in columns, is applied on every render; a frame narrower
than twice it gets no tree."
  :type 'alist
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

(defcustom tab-bar-tree-button "≡"
  "Label of the mode toggle button.
Shown by `tab-bar-tree-format-button' and `tab-bar-tree-header-button'."
  :type 'string
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-hide-tab-bar nil
  "Non-nil means hide the tab bar while the mode is on.
The tabs are then shown only in the tree; `tab-bar-show' is restored
when the mode is turned off."
  :type 'boolean
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
    (desktop-after-read-hook . tab-bar-tree--show)
    (delete-frame-functions . tab-bar-tree--delete-frame))
  "Hooks the mode adds a function to, as (HOOK . FUNCTION).")
;; No window change hooks: they run during redisplay.  As a result, the
;; name of a tab without an explicit name is updated on tab events only.

(defvar tab-bar-tree-buffer "*tab-bar-tree*"
  "Base name of the tree buffers; each frame's ends in its `frame-id'.")

(defvar-local tab-bar-tree--frame nil
  "Frame whose tabs the tree buffer shows.")
;; Kept if `tab-bar-tree-mode' is called again in the buffer.
(put 'tab-bar-tree--frame 'permanent-local t)

(defvar-local tab-bar-tree-collapsed nil
  "Names of the folded groups.")

(defun tab-bar-tree--buffer-name (&optional frame)
  "Return the name of FRAME's tree buffer."
  (format "%s<%d>" tab-bar-tree-buffer (frame-id frame)))

(defun tab-bar-tree--buffer (&optional frame)
  "Return FRAME's tree buffer, creating it if needed."
  (let ((name (tab-bar-tree--buffer-name frame)))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (tab-bar-tree-mode)
          (setq tab-bar-tree--frame (or frame (selected-frame)))
          (current-buffer)))))

(defun tab-bar-tree--buffers ()
  "Return the tree buffers of all frames."
  (seq-filter (lambda (buffer)
                (eq (buffer-local-value 'major-mode buffer) 'tab-bar-tree-mode))
              (buffer-list)))

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
                 (get-text-property (line-beginning-position) 'tab-bar-tree)))
         (tab-p (eq (car-safe line) 'tab))
         (frame (buffer-local-value 'tab-bar-tree--frame (window-buffer window))))
    (cond
     ;; Tree in a child frame, e.g. a popup: dismissed on tab selection.
     ((frame-parent (window-frame window))
      (when tab-p
        (quit-window nil window)
        (select-frame-set-input-focus frame)))
     ;; Leave the tree window before selecting a tab, so it is not saved
     ;; as the selected window of the old tab.  Toggling a group from the
     ;; keyboard keeps focus.  `get-mru-window' skips the tree window: it
     ;; is dedicated, `no-other-window' and selected.
     ((and (eq window (selected-window)) (or position tab-p))
      (when-let* ((other (get-mru-window nil nil t t)))
        (select-window other))))
    (with-selected-frame frame
      (pcase line
        (`(tab . ,i) (tab-bar-select-tab i))
        (`(group . ,group)
         ;; A click leaves another buffer current.
         (with-current-buffer (window-buffer window)
           (setq tab-bar-tree-collapsed
                 (if (member group tab-bar-tree-collapsed)
                     (delete group tab-bar-tree-collapsed)
                   (cons group tab-bar-tree-collapsed))))
         ;; Updates every window on the buffer.
         (tab-bar-tree-render))))))

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

;;;###autoload
(defun tab-bar-tree-render ()
  "Render the selected frame's tree; return its buffer.
Also for display outside the side window, e.g. in a child frame."
  (let* ((buf (tab-bar-tree--buffer))
         (tabs (funcall tab-bar-tabs-function))
         (tab (assq 'current-tab tabs))
         (current-group (funcall tab-bar-tab-group-function tab)))
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
      ;; Spaces: a leading one, a gap before RIGHT and a trailing one.
      (let* ((right (tab-bar-tree--header-face
                     (concat " " (funcall tab-bar-tree-header-right-function tab)
                             " ")))
             (left (tab-bar-tree--header-face
                    (concat " " (funcall tab-bar-tree-header-left-function tab))))
             (right-width (string-pixel-width right buf))
             ;; Header text area: the pinned window minus the fringe
             ;; and the divider.
             (right-edge (- (tab-bar-tree--pixel-width) left-fringe-width
                            (frame-right-divider-width)
                            right-width)))
        ;; Overflowing LEFT would push RIGHT off the window.
        (when (> (string-pixel-width left buf) right-edge)
          (setq left (truncate-string-pixelwise
                      left right-edge buf (truncate-string-ellipsis))))
        ;; The header line is a mode line construct: `%' is escaped.
        (setq header-line-format
              (list (string-replace "%" "%%" left)
                    ;; Not `mode-line-format-right-align': it measures
                    ;; what follows it in `mode-line-format' only.
                    (propertize
                     " " 'display
                     ;; `right': any window width; broken on a terminal
                     ;; beside another window (bug#59620).
                     `(space :align-to ,(if (display-graphic-p)
                                            `(- right (,right-width))
                                          `(,right-edge))))
                    (string-replace "%" "%%" right))))
      (tab-bar-tree--remap-faces))
    buf))

(defun tab-bar-tree-header-group (tab)
  "Return TAB's group as \"group:NAME\", or \"group:none\" if it has none."
  ;; As in `marginalia-annotate-tab'.
  (propertize (format "group:%s" (or (funcall tab-bar-tab-group-function tab)
                                     'none))
              'face 'shadow))

(defun tab-bar-tree--header-face (string)
  "Return a copy of STRING with the `header-line' face added."
  (let ((string (copy-sequence string)))
    (add-face-text-property 0 (length string) 'header-line t string)
    string))

(defun tab-bar-tree--width ()
  "Return the tree window's width in columns."
  (alist-get 'window-width tab-bar-tree-display-alist))

(defun tab-bar-tree--pixel-width ()
  "Return the tree window's pinned pixel width."
  (* (tab-bar-tree--width) (frame-char-width)))

(defvar-local tab-bar-tree--remap-cookies nil
  "Face remapping cookies added by `tab-bar-tree--remap-faces'.")

(defvar-local tab-bar-tree--remap-key nil
  "Remapped attributes, as (LABELS-BACKGROUND HEADER-BACKGROUND HEADER-HEIGHT).")

(defvar tab-bar-tree--faces
  '(tab-bar tab-bar-tab tab-bar-tab-inactive tab-bar-tab-ungrouped
    tab-bar-tab-group-current tab-bar-tab-group-inactive)
  "Faces whose background is remapped to the default background.")

(defun tab-bar-tree--remap-faces ()
  "Remap label backgrounds according to `tab-bar-tree-tab-background'.
The header line takes the tab bar's background and height: it stands
in for the tab bar.  Called on every render; remaps only on a change."
  (let* ((labels (unless tab-bar-tree-tab-background
                   (face-background 'default nil 'default)))
         ;; Copied, not inherited: `tab-bar' is remapped here too.
         (header (face-background 'tab-bar nil 'default))
         (height (face-attribute 'tab-bar :height nil 'default))
         (key (list labels header height)))
    (unless (equal key tab-bar-tree--remap-key)
      (mapc #'face-remap-remove-relative tab-bar-tree--remap-cookies)
      (setq tab-bar-tree--remap-cookies
            ;; All three: themes style the active and inactive header line
            ;; on their own, not always by inheritance.
            (mapcar (lambda (face)
                      (face-remap-add-relative face :background header
                                               :height height))
                    '(header-line header-line-active header-line-inactive))
            tab-bar-tree--remap-key key)
      (when labels
        (dolist (face tab-bar-tree--faces)
          (push (face-remap-add-relative face :background labels)
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
                       `(,@tab-bar-tree-display-alist
                         ;; `side', not t: `quit-restore-window' deletes
                         ;; a window dedicated with t outright.
                         (dedicated . side)
                         (window-parameters
                          . ,tab-bar-tree--window-parameters))))
              ;; Pixelwise: a tab's window configuration may restore a
              ;; width off by less than a column.
              (delta (- (tab-bar-tree--pixel-width)
                        (window-pixel-width window))))
    (unless (zerop delta)
      ;; IGNORE t: a side window sharing the column, e.g. `dired-side',
      ;; also fixes its width; the tree sets the column's.
      (window-resize window delta t t t))
    window))

(defvar tab-bar-tree--updating nil
  "Non-nil while the tree is being drawn.
The first read of a frame's tabs runs `tab-bar-tab-post-open-functions',
which would draw the tree recursively.")

(defun tab-bar-tree--windows ()
  "Return the selected frame's tree windows.
They may show another frame's tree buffer, e.g. after `clone-frame'."
  (seq-filter (lambda (window)
                (eq (buffer-local-value 'major-mode (window-buffer window))
                    'tab-bar-tree-mode))
              (window-list nil 'nomini)))

(defun tab-bar-tree--draw ()
  "Render the tree and show it in the selected frame; return the window."
  (unless tab-bar-tree--updating
    (let ((tab-bar-tree--updating t))
      (tab-bar-tree--window (tab-bar-tree-render)))))

(defun tab-bar-tree--show (&rest _)
  "Render the tree and show it in the selected frame; return the window.
Return nil on a child, minibuffer-only, unsplittable or narrow frame."
  (let ((frame (selected-frame)))
    (unless (or (frame-parent frame)
                (eq (frame-parameter frame 'minibuffer) 'only)
                (frame-parameter frame 'unsplittable)
                (and (not (tab-bar-tree--windows))
                     (< (frame-width frame) (* 2 (tab-bar-tree--width)))))
      (tab-bar-tree--draw))))

(defun tab-bar-tree--update (&rest _)
  "Re-render the tree if it is shown in the selected frame."
  (when (tab-bar-tree--windows)
    (tab-bar-tree--draw)))

(defun tab-bar-tree--theme (theme)
  "Re-render the trees of all frames after THEME is enabled or disabled."
  ;; `enable-theme' also enables `user' for every theme.
  (unless (eq theme 'user)
    (dolist (buffer (tab-bar-tree--buffers))
      (let ((frame (buffer-local-value 'tab-bar-tree--frame buffer)))
        (when (frame-live-p frame)
          (with-selected-frame frame
            (tab-bar-tree--update)))))))

(defun tab-bar-tree--delete-frame (frame)
  "Kill FRAME's tree buffer."
  (when-let* ((buffer (get-buffer (tab-bar-tree--buffer-name frame))))
    (kill-buffer buffer)))

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

(defun tab-bar-tree--kill-buffer ()
  "Delete the side windows showing the current buffer, which is being killed.
Other windows, e.g. a child frame's sole one, are left to `kill-buffer'."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (when (window-parameter window 'window-side)
      (tab-bar-tree--forget-window window))))

(defun tab-bar-tree--restore-windows (function frame windows type)
  "Delete tree windows among WINDOWS; pass the rest to FUNCTION.
Advice for `tab-bar-select-restore-windows': tabs saved with the mode
on show tree buffers killed by turning it off.  Only its default value
is covered.  Matched by name: a killed buffer has no mode, a window
restored from `ws' no parameters."
  (funcall function frame
           (seq-remove
            (pcase-lambda (`(,window ,buffer))
              (when (string-prefix-p tab-bar-tree-buffer
                                     (if (bufferp buffer)
                                         (buffer-last-name buffer)
                                       buffer))
                (when (window-live-p window)
                  (tab-bar-tree--forget-window window))
                t))
            windows)
           type))

;;;###autoload
(defun tab-bar-tree-format-button ()
  "Produce the tab bar button toggling `global-tab-bar-tree-mode'.
An item for `tab-bar-format'."
  `((tab-bar-tree menu-item ,(concat " " tab-bar-tree-button)
                  global-tab-bar-tree-mode :help "Toggle the tab tree")))

(defvar-keymap tab-bar-tree--toggle-map
  :doc "Keymap of `tab-bar-tree-header-button'."
  ;; On press, as `tab-bar-tree-format-button': the layout shifts under
  ;; the mouse, so the release lands on the other button.
  "<header-line> <down-mouse-1>" #'global-tab-bar-tree-mode
  "<header-line> <mouse-1>" #'ignore)

(defun tab-bar-tree-header-button ()
  "Return the header line button toggling `global-tab-bar-tree-mode'.
For `tab-bar-tree-header-left-function'."
  (propertize tab-bar-tree-button
              'keymap tab-bar-tree--toggle-map
              'help-echo "Toggle the tab tree"))

(defvar tab-bar-tree--tab-bar-show nil
  "`tab-bar-show' saved by `tab-bar-tree-hide-tab-bar', as a list.")

(defun tab-bar-tree--hide-tab-bar (hide)
  "Hide the tab bar if HIDE is non-nil, else restore it."
  ;; `setopt': the :set of `tab-bar-show' applies it to every frame.
  (cond
   ((and hide tab-bar-tree-hide-tab-bar)
    (setq tab-bar-tree--tab-bar-show (list tab-bar-show))
    (setopt tab-bar-show nil))
   ((and (not hide) tab-bar-tree--tab-bar-show)
    (setopt tab-bar-show (car tab-bar-tree--tab-bar-show))
    (setq tab-bar-tree--tab-bar-show nil))))

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
        ;; Not removed with the mode: killed tree buffers appear after it.
        (advice-add 'tab-bar-select-restore-windows :around
                    #'tab-bar-tree--restore-windows)
        (tab-bar-tree--hide-tab-bar t)
        (tab-bar-tree--show))
    (dolist (parameter tab-bar-tree--persistent-parameters)
      (setq window-persistent-parameters
            (assq-delete-all parameter window-persistent-parameters)))
    (setq tab-bar-tree--persistent-parameters nil)
    ;; Other tabs drop the window on selection:
    ;; `tab-bar-tree--restore-windows'.
    (mapc #'kill-buffer (tab-bar-tree--buffers))
    (tab-bar-tree--hide-tab-bar nil)))

(provide 'tab-bar-tree)
;;; tab-bar-tree.el ends here

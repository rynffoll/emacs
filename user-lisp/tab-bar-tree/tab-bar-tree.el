;;; tab-bar-tree.el --- The tab bar's tabs as a tree in a side window -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: convenience, frames
;; URL: https://github.com/rynffoll/emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Prototype.  Shows the tab bar's tabs in a fixed-width side window,
;; one per line, grouped by tab group.  Groups can be folded.
;;
;; Labels come from `tab-bar-tab-name-format-function' and
;; `tab-bar-tab-group-format-function', so the tree shows the same
;; hints, marks, indicators and colors as the tab bar.
;;
;; `tab-bar-tree-mode' shows the tree and hides the tab bar.  The
;; tree's header line replaces the tab bar; it is built from
;; `tab-bar-tree-header-format' as the tab bar is from `tab-bar-format'.

;;; Code:

(require 'tab-bar)
(require 'face-remap)

(defgroup tab-bar-tree nil
  "The tab bar's tabs as a tree in a side window."
  :group 'tab-bar
  :prefix "tab-bar-tree-")

(defcustom tab-bar-tree-display-alist
  '((side . left) (slot . -100) (window-width . 32))
  "Display parameters for `display-buffer-in-side-window'.
`window-width', in columns, is applied on every render.  A tree window
is not created on a frame narrower than twice that width."
  :type 'alist
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-indent 3
  "Indentation of a grouped tab, in columns."
  :type 'natnum
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-tab-background nil
  "Non-nil means labels keep the background of their faces.
If nil, labels use the default background and the current tab is
distinguished only by its indicator.  Takes effect on the next render,
e.g. \\[tab-bar-tree-refresh]."
  :type 'boolean
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-tab-status-function nil
  "Function returning the status lines shown under a tab.
Called with TAB and I, as `tab-bar-tab-name-format-function'; returns a
list of strings.  Call `tab-bar-tree-update' when they change."
  :type '(choice (const :tag "None" nil) function)
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-header-format '(tab-bar-tree-format-button
                                        tab-bar-separator
                                        tab-bar-tree-header-format-group
                                        tab-bar-tree-header-format-align-right
                                        tab-bar-format-global)
  "Template for the items of the tree's header line.
As in `tab-bar-format', each item is a function returning a string, a
list of menu-item elements, or nil.  Functions are called in the buffer
of the frame's selected window.  Items after
`tab-bar-tree-header-format-align-right' are right-aligned.  The header
line is rebuilt on each render and by `tab-bar-tree-update-header-line'."
  :type 'hook
  :options '(tab-bar-tree-format-button
             tab-bar-tree-header-format-group
             tab-bar-tree-header-format-group-truncated
             tab-bar-separator
             tab-bar-tree-header-format-align-right
             tab-bar-format-global)
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-bar-tree-update-header-line))
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-button "≡"
  "Label of the button that toggles `tab-bar-tree-mode'.
Used by `tab-bar-tree-format-button'."
  :type 'string
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-refresh-commands
  '(tab-bar-rename-tab)
  "Commands that modify a tab in place; the tree is refreshed after each.
Other tab changes are caught by tab-bar hooks and advice on
`tab-bar-tabs-set'.  Read when the mode is enabled."
  :type '(repeat function)
  :group 'tab-bar-tree)

(defvar tab-bar-tree--hook-alist
  '((tab-bar-tab-post-select-functions tab-bar-tree--show)
    ;; The automatic name of the current tab follows its selected window.
    (window-buffer-change-functions tab-bar-tree--window-change)
    (window-selection-change-functions tab-bar-tree--window-change)
    (tab-bar-tab-post-open-functions tab-bar-tree--show)
    (tab-bar-tab-post-change-group-functions tab-bar-tree--show)
    ;; Label colors depend on the theme.
    (enable-theme-functions tab-bar-tree--theme-changed)
    (disable-theme-functions tab-bar-tree--theme-changed)
    ;; Window parameters are not restored from a desktop file.
    (desktop-after-read-hook tab-bar-tree--show)
    (after-make-frame-functions tab-bar-tree--make-frame)
    ;; Depth -90, the counterpart of t: before `server-handle-delete-frame'.
    ;; On a client's last frame it deletes the client's terminal, and
    ;; later functions do not run.
    (delete-frame-functions tab-bar-tree--delete-frame -90))
  "List of (HOOK FUNCTION [DEPTH]) entries added by the mode.
DEPTH defaults to t: after the hook's other functions, e.g.
`tab-bar-move-tab-to-group' or one clearing a mark.")

(defvar tab-bar-tree-buffer-name "*tab-bar-tree*"
  "Base name of the tree buffers.
A frame's tree buffer name ends in its `frame-id'.")

(defvar-local tab-bar-tree--frame nil
  "Frame whose tabs the tree buffer shows.")
;; Survives a repeated call of `tab-bar-tree-buffer-mode' in the buffer.
(put 'tab-bar-tree--frame 'permanent-local t)

(defvar-local tab-bar-tree--collapsed nil
  "Names of the folded groups.")

(defvar-local tab-bar-tree--tab-name nil
  "Name of the current tab at the last render.")

(defun tab-bar-tree--buffer-name (&optional frame)
  "Return the name of FRAME's tree buffer."
  (format "%s<%d>" tab-bar-tree-buffer-name (frame-id frame)))

(defun tab-bar-tree--buffer (&optional frame)
  "Return FRAME's tree buffer, creating it if needed."
  (let ((name (tab-bar-tree--buffer-name frame)))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (tab-bar-tree-buffer-mode)
          (setq tab-bar-tree--frame (or frame (selected-frame)))
          (current-buffer)))))

(defun tab-bar-tree--buffers ()
  "Return the tree buffers of all frames."
  (match-buffers '(major-mode . tab-bar-tree-buffer-mode)))

(defvar-keymap tab-bar-tree-buffer-mode-map
  :doc "Keymap for `tab-bar-tree-buffer-mode'."
  :parent special-mode-map
  "RET"     #'tab-bar-tree-select
  "TAB"     #'tab-bar-tree-select
  ;; Keep `mouse-drag-region' from selecting the tree window on press.
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'tab-bar-tree-select
  ;; Keep a double click from toggling a group twice.
  "<double-mouse-1>" #'ignore
  "<triple-mouse-1>" #'ignore
  "n"       #'next-line
  "p"       #'previous-line
  "g"       #'tab-bar-tree-refresh)

(defun tab-bar-tree--leave-window ()
  "Select the most recently used window other than the tree window.
`get-mru-window' skips the tree window because it is dedicated,
`no-other-window' and selected."
  (when-let* ((other (get-mru-window nil nil t t)))
    (select-window other)))

(defun tab-bar-tree-select (&optional event)
  "Select the tab on the current line, or toggle the group on it.
If EVENT is a mouse event, act on the clicked line instead."
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
     ;; In a child frame, e.g. a popup, selecting a tab dismisses the tree.
     ((frame-parent (window-frame window))
      (when tab-p
        (quit-window nil window)
        (select-frame-set-input-focus frame)))
     ;; Leave the tree window before selecting a tab, so that it is not
     ;; saved as the old tab's selected window.  Toggling a group from
     ;; the keyboard keeps focus.
     ((and (eq window (selected-window)) (or position tab-p))
      (tab-bar-tree--leave-window)))
    (with-selected-frame frame
      (pcase line
        (`(tab . ,i) (tab-bar-select-tab i))
        (`(group . ,group)
         ;; After a click, another buffer is current.
         (with-current-buffer (window-buffer window)
           (setq tab-bar-tree--collapsed
                 (if (member group tab-bar-tree--collapsed)
                     (delete group tab-bar-tree--collapsed)
                   (cons group tab-bar-tree--collapsed))))
         ;; Updates every window showing the buffer.
         (tab-bar-tree-render))))))

(defun tab-bar-tree--group-string (tab i current-p open-p)
  "Return the group label of TAB at index I.
Pass CURRENT-P to `tab-bar-tab-group-format-function' directly, and
OPEN-P as the value of `tab-bar-show-inactive-group-tabs', which it
reads."
  (let ((tab-bar-show-inactive-group-tabs open-p))
    (funcall tab-bar-tab-group-format-function tab i current-p)))

(defun tab-bar-tree--goto-line (line)
  "Move point to the start of LINE and return point."
  (goto-char (point-min))
  (forward-line (1- line))
  (point))

;;;###autoload
(defun tab-bar-tree-render ()
  "Render the selected frame's tree and return its buffer.
Usable for display outside the side window too, e.g. in a child frame."
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
        (setq tab-bar-tree--collapsed
              (delete current-group tab-bar-tree--collapsed)
              tab-bar-tree--tab-name (alist-get 'name tab))
        (erase-buffer)
        (dolist (tab tabs)
          (setq i (1+ i))
          (let* ((group (funcall tab-bar-tab-group-function tab))
                 (current-p (eq (car tab) 'current-tab))
                 (collapsed (and group (member group tab-bar-tree--collapsed))))
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
              (let ((indent (make-string (if group tab-bar-tree-indent 0) ?\s)))
                ;; `tab-bar-tree' covers the indentation and status
                ;; lines, so RET works there; `mouse-face' covers the
                ;; label only.
                (insert (propertize
                         (concat indent
                                 (propertize
                                  (funcall tab-bar-tab-name-format-function tab i)
                                  'mouse-face 'highlight)
                                 (mapconcat
                                  (lambda (status)
                                    (concat "\n" indent "  "
                                            (propertize status 'face 'shadow)))
                                  (and tab-bar-tree-tab-status-function
                                       (funcall tab-bar-tree-tab-status-function
                                                tab i))))
                         'tab-bar-tree (cons 'tab i))
                        "\n")))))
        (pcase-dolist (`(,window ,start ,point) windows)
          (set-window-start window (tab-bar-tree--goto-line start) t)
          (set-window-point window (tab-bar-tree--goto-line point)))
        (tab-bar-tree--goto-line line))
      ;; Remap first: the header line measures its items with the
      ;; buffer's faces.
      (tab-bar-tree--remap-faces)
      (tab-bar-tree--set-header-line))
    buf))

(defun tab-bar-tree--header-format-list (format-list)
  "Return items of FORMAT-LIST concatenated, as `tab-bar-format-list'.
FORMAT-LIST is `tab-bar-tree-header-format' or part of it."
  (if-let* ((align (memq 'tab-bar-tree-header-format-align-right
                         format-list)))
      ;; The rest is formatted once, as `tab-bar-format-list' does.
      (let ((rest (tab-bar-tree--header-format-list (cdr align))))
        (concat (tab-bar-tree--header-format-list
                 (butlast format-list (length align)))
                (tab-bar-tree-header-format-align-right rest)
                rest))
    (mapconcat (lambda (item)
                 (let ((value (funcall item)))
                   (if (stringp value)
                       value
                     (mapconcat #'tab-bar-tree--menu-item value))))
               format-list)))

(defun tab-bar-tree--menu-item (item)
  "Return the label of tab bar menu ITEM.
Make the label a button if ITEM binds a command."
  (pcase-let ((`(,_ menu-item ,label ,binding . ,props) item))
    (if (and (commandp binding) (not (eq binding 'ignore)))
        (propertize label
                    'keymap (define-keymap
                              ;; Bind the press, as `tab-bar-mouse-down-1'
                              ;; does: the layout shifts under the mouse.
                              "<header-line> <down-mouse-1>" binding
                              "<header-line> <mouse-1>" #'ignore)
                    'help-echo (plist-get props :help))
      label)))

(defvar-local tab-bar-tree--header-line nil
  "The tree's header line, from `tab-bar-tree-header-format'.")

(defconst tab-bar-tree--header-line-format
  '(:eval (string-replace "%" "%%" tab-bar-tree--header-line)))
(put 'tab-bar-tree--header-line-format 'risky-local-variable t)

(defun tab-bar-tree--set-header-line ()
  "Set the tree buffer's header line from `tab-bar-tree-header-format'.
Call the items in the buffer of the frame's selected window or, if that
is a minibuffer window, of the window the minibuffer was entered from."
  (let ((window (frame-selected-window)))
    (when (window-minibuffer-p window)
      (setq window (or (minibuffer-selected-window) window)))
    (setq tab-bar-tree--header-line
          (tab-bar-tree--header-face
           (with-current-buffer (window-buffer window)
             (tab-bar-tree--header-format-list tab-bar-tree-header-format))))))

(defun tab-bar-tree-header-format-align-right (&optional rest)
  "Align the rest of the header line items to the right.
REST is the formatted rest.  An item for `tab-bar-tree-header-format',
as `tab-bar-format-align-right' for `tab-bar-format'."
  ;; Not `mode-line-format-right-align': it only measures what follows
  ;; it in `mode-line-format'.
  (let* ((rest (or rest (tab-bar-tree--header-format-list
                         (cdr (memq 'tab-bar-tree-header-format-align-right
                                    tab-bar-tree-header-format)))))
         (width (string-pixel-width (tab-bar-tree--header-face rest)
                                    (tab-bar-tree--buffer))))
    (propertize " " 'display
                ;; `right' fits any window width but is broken on a
                ;; terminal next to another window (bug#59620): there,
                ;; the pinned width minus a one-column border.
                (if (window-system)
                    `(space :align-to (- right (,width)))
                  `(space :align-to
                          (,(- (tab-bar-tree--pixel-width) 1 width)))))))

(defun tab-bar-tree-header-format-group ()
  "Return the current tab's group as \"group:NAME\", or \"group:none\".
An item for `tab-bar-tree-header-format'."
  ;; Same format as `marginalia-annotate-tab'.
  (propertize (format "group:%s"
                      (or (funcall tab-bar-tab-group-function
                                   (assq 'current-tab
                                         (funcall tab-bar-tabs-function)))
                          'none))
              'face 'shadow))

(defun tab-bar-tree-header-format-group-truncated ()
  "Return `tab-bar-tree-header-format-group' truncated as a tab name.
See `tab-bar-tab-name-format-truncated'.  An item for
`tab-bar-tree-header-format'."
  (tab-bar-tab-name-format-truncated
   (tab-bar-tree-header-format-group) nil nil))

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
  "Last remapped attributes, as (LABELS-BACKGROUND HEADER-BACKGROUND HEIGHT).")

(defvar tab-bar-tree--faces
  '(tab-bar tab-bar-tab tab-bar-tab-inactive tab-bar-tab-ungrouped
    tab-bar-tab-group-current tab-bar-tab-group-inactive)
  "Faces whose background is remapped to the default background.")

(defun tab-bar-tree--remap-faces ()
  "Remap the tree buffer's faces to match the tab bar.
Text gets the tab bar's height; the header line, which replaces the tab
bar, also gets its background.  Label backgrounds follow
`tab-bar-tree-tab-background'.  Remap only when these attributes change."
  (let* ((labels (unless tab-bar-tree-tab-background
                   (face-background 'default nil 'default)))
         ;; Copied, not inherited: `tab-bar' is remapped here too.
         (header (face-background 'tab-bar nil 'default))
         (height (face-attribute 'tab-bar :height nil 'default))
         (key (list labels header height)))
    (unless (equal key tab-bar-tree--remap-key)
      (mapc #'face-remap-remove-relative tab-bar-tree--remap-cookies)
      (setq tab-bar-tree--remap-cookies
            ;; All three: themes may style the active and inactive header
            ;; lines directly rather than by inheritance.
            (cons (face-remap-add-relative 'default :height height)
                  (mapcar (lambda (face)
                            (face-remap-add-relative face :background header
                                                     :height height))
                          '(header-line header-line-active
                            header-line-inactive)))
            tab-bar-tree--remap-key key)
      (when labels
        (dolist (face tab-bar-tree--faces)
          (push (face-remap-add-relative face :background labels)
                tab-bar-tree--remap-cookies))))))

(define-derived-mode tab-bar-tree-buffer-mode special-mode "TabTree"
  "Major mode for the tab tree buffer."
  (setq cursor-type nil
        truncate-lines t
        mode-line-format nil
        header-line-format 'tab-bar-tree--header-line-format)
  ;; Lines are truncated, so no margins or right fringe.  A narrow left
  ;; fringe keeps labels off the window edge.
  (setq-local left-fringe-width 4
              right-fringe-width 0
              left-margin-width 0
              right-margin-width 0)
  ;; No horizontal scrolling in a narrow window, as in
  ;; `speedbar-window-mode'.
  (setq-local auto-hscroll-mode nil)
  (setq-local window-size-fixed 'width)
  ;; Delete the side windows here: `tab-bar-tree--delete-window' refuses
  ;; while the mode is on, leaving them showing another buffer.
  (add-hook 'kill-buffer-hook #'tab-bar-tree--kill-buffer nil t))

(defvar tab-bar-tree-mode)

(defun tab-bar-tree--delete-window (window)
  "Delete WINDOW if `tab-bar-tree-mode' is off.
Value of the `delete-window' window parameter.  Allows deleting a tree
window restored from a configuration saved while the mode was on."
  (unless tab-bar-tree-mode
    (tab-bar-tree--forget-window window)))

(defvar tab-bar-tree--window-parameters
  '((no-other-window . t)
    (no-delete-other-windows . t)
    (delete-window . tab-bar-tree--delete-window))
  "Parameters of the tree window.")

(defvar tab-bar-tree--persistent-parameters nil
  "Parameters the mode added to `window-persistent-parameters'.")

(defvar tab-bar-tree--advised nil
  "Functions the mode advised to re-render the tree.")

(defun tab-bar-tree--window (buffer)
  "Show BUFFER in the selected frame's tree window and return the window.
Reapply width, parameters and dedication to a reused window: a window
restored from a desktop file or a tab's `ws' has no parameters, and
`quit-restore-window' may undedicate it."
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
      ;; also fixes its width; the tree sets the column width.
      (window-resize window delta t t t))
    window))

(defvar tab-bar-tree--updating nil
  "Non-nil while the tree is being drawn.
Prevents recursion: the first read of a frame's tabs runs
`tab-bar-tab-post-open-functions'.")

(defun tab-bar-tree--windows ()
  "Return the selected frame's tree windows.
They may show another frame's tree buffer, e.g. after `clone-frame'."
  (seq-filter (lambda (window)
                (eq (buffer-local-value 'major-mode (window-buffer window))
                    'tab-bar-tree-buffer-mode))
              (window-list nil 'nomini)))

(defun tab-bar-tree--draw ()
  "Show the rendered tree in the selected frame; return its window."
  (unless tab-bar-tree--updating
    (let ((tab-bar-tree--updating t))
      (tab-bar-tree--window (tab-bar-tree-render)))))

(defun tab-bar-tree--show (&rest _)
  "Show the tree in the selected frame if it can hold one; return its window.
Return nil on a child, minibuffer-only or unsplittable frame, or on one
too narrow for a new tree window."
  (let ((frame (selected-frame)))
    (unless (or (frame-parent frame)
                (eq (frame-parameter frame 'minibuffer) 'only)
                (frame-parameter frame 'unsplittable)
                (and (not (tab-bar-tree--windows))
                     (< (frame-width frame) (* 2 (tab-bar-tree--width)))))
      (tab-bar-tree--draw))))

(defun tab-bar-tree--update-frame (&rest _)
  "Re-render the selected frame's tree if a window shows it.
The window may be in another frame, e.g. a child frame."
  ;; PERF: Render only: redisplaying the buffer in its window redraws
  ;; the whole frame.
  (when-let* (((not tab-bar-tree--updating))
              (buffer (get-buffer (tab-bar-tree--buffer-name)))
              ((get-buffer-window-list buffer nil t)))
    (let ((tab-bar-tree--updating t))
      (tab-bar-tree-render))))

;;;###autoload
(defun tab-bar-tree-update (&rest _)
  "Re-render the trees shown in all frames.
For use as a hook function or advice when tab labels or status lines change.
Arguments are ignored."
  (dolist (buffer (tab-bar-tree--buffers))
    (let ((frame (buffer-local-value 'tab-bar-tree--frame buffer)))
      (when (frame-live-p frame)
        (with-selected-frame frame
          (tab-bar-tree--update-frame))))))

(defvar tab-bar-tree--name-timer nil
  "Timer of a pending render for a changed tab name, or nil.")

(defun tab-bar-tree--window-change (frame)
  "Re-render FRAME's tree if the current tab's automatic name changed.
For window change hooks.  They run during redisplay, so the render is
deferred to a timer."
  ;; PERF: Cheapest checks first; most calls end before the name.
  (when-let* (((not tab-bar-tree--name-timer))
              ((eq frame (selected-frame)))
              ((not (alist-get 'explicit-name
                               (assq 'current-tab (frame-parameter frame 'tabs)))))
              (buffer (get-buffer (tab-bar-tree--buffer-name frame)))
              ((not (eq (window-buffer) buffer)))
              ((get-buffer-window-list buffer nil t))
              ((not (equal (funcall tab-bar-tab-name-function)
                           (buffer-local-value 'tab-bar-tree--tab-name buffer)))))
    (setq tab-bar-tree--name-timer
          (run-with-timer 0 nil #'tab-bar-tree--name-changed frame))))

(defun tab-bar-tree--name-changed (frame)
  "Re-render FRAME's tree after a tab name change."
  (setq tab-bar-tree--name-timer nil)
  (when (frame-live-p frame)
    (with-selected-frame frame
      (tab-bar-tree--update-frame))))

(defun tab-bar-tree--theme-changed (theme)
  "Re-render the trees of all frames after THEME is enabled or disabled."
  ;; `enable-theme' also enables the `user' theme each time.
  (unless (eq theme 'user)
    (tab-bar-tree-update)))

(defun tab-bar-tree--make-frame (frame)
  "Show the tree in FRAME once it is set up."
  ;; Deferred: `clone-frame' copies the windows after this hook, and with
  ;; them a tree window showing the original frame's buffer.
  (run-with-timer 0 nil #'tab-bar-tree--show-frame frame))

(defun tab-bar-tree--show-frame (frame)
  "Show the tree in FRAME if it is live."
  (when (frame-live-p frame)
    (with-selected-frame frame
      (tab-bar-tree--show))))

(defun tab-bar-tree--delete-frame (frame)
  "Kill FRAME's tree buffer."
  (when-let* ((buffer (get-buffer (tab-bar-tree--buffer-name frame))))
    (kill-buffer buffer)))

;;;###autoload
(defun tab-bar-tree-update-header-line (&rest _)
  "Rebuild the selected frame's tree header line.
For use as a hook function or advice when `tab-bar-tree-header-format'
items change.  Arguments are ignored."
  (when-let* ((buffer (get-buffer (tab-bar-tree--buffer-name))))
    (with-current-buffer buffer
      (tab-bar-tree--set-header-line)
      (force-mode-line-update))))

;;;###autoload
(defun tab-bar-tree-refresh ()
  "Re-render the tree, showing it if hidden."
  (interactive)
  (tab-bar-tree--show))

;;;###autoload
(defun tab-bar-tree-get-focus ()
  "Select the tree window, showing it first if needed.
Clicks in the tree do not select its window."
  (interactive)
  (when-let* ((window (tab-bar-tree--show)))
    (select-window window)))

(defun tab-bar-tree--forget-window (window)
  "Delete WINDOW, ignoring its `delete-window' parameter."
  (let ((ignore-window-parameters t))
    (delete-window window)))

(defun tab-bar-tree--kill-buffer ()
  "Delete the side windows showing the current buffer.
Run from `kill-buffer-hook'.  Other windows, e.g. a child frame's sole
window, are left to `kill-buffer'."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (when (window-parameter window 'window-side)
      (tab-bar-tree--forget-window window))))

(defun tab-bar-tree--restore-windows (function frame windows type)
  "Delete tree windows among WINDOWS, then call FUNCTION.
FUNCTION gets FRAME, the remaining windows and TYPE.
Around advice for the function `tab-bar-select-restore-windows': tabs
saved while the mode was on show tree buffers killed by turning it off.
Covers only the option's default value.  Match by buffer name: a killed
buffer has no mode, and a window restored from `ws' has no parameters."
  (funcall function frame
           (seq-remove
            (pcase-lambda (`(,window ,buffer))
              (when (string-prefix-p tab-bar-tree-buffer-name
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
  "Return the tab bar button that toggles `tab-bar-tree-mode'.
An item for `tab-bar-format' and `tab-bar-tree-header-format'."
  `((tab-bar-tree menu-item ,(concat " " tab-bar-tree-button)
                  tab-bar-tree-mode :help "Toggle the tab tree")))

;;;###autoload
(define-minor-mode tab-bar-tree-mode
  "Show the tabs of the tab bar as a tree in a side window.
The tab bar is hidden while the mode is on."
  :global t
  (pcase-dolist (`(,hook ,function ,depth) tab-bar-tree--hook-alist)
    (if tab-bar-tree-mode
        (add-hook hook function (or depth t))
      (remove-hook hook function)))
  ;; Closing and moving tabs run no hook but end in `tab-bar-tabs-set'.
  ;; Remove what was advised, not what the option holds now.
  (dolist (symbol tab-bar-tree--advised)
    (advice-remove symbol #'tab-bar-tree--update-frame))
  (setq tab-bar-tree--advised
        (and tab-bar-tree-mode
             (cons 'tab-bar-tabs-set tab-bar-tree-refresh-commands)))
  (dolist (symbol tab-bar-tree--advised)
    (advice-add symbol :after #'tab-bar-tree--update-frame))
  (if tab-bar-tree-mode
      (progn
        ;; Value t: kept in window states (e.g. by
        ;; `window-toggle-side-windows'), not in desktop files.
        (dolist (parameter (mapcar #'car tab-bar-tree--window-parameters))
          (unless (assq parameter window-persistent-parameters)
            (push (cons parameter t) window-persistent-parameters)
            (push parameter tab-bar-tree--persistent-parameters)))
        ;; Kept when the mode is off: tabs still reference killed tree buffers.
        (advice-add 'tab-bar-select-restore-windows :around
                    #'tab-bar-tree--restore-windows)
        ;; `setopt': the :set of `tab-bar-show' applies it to every frame.
        (setopt tab-bar-show nil)
        (tab-bar-tree--show))
    (dolist (parameter tab-bar-tree--persistent-parameters)
      (setq window-persistent-parameters
            (assq-delete-all parameter window-persistent-parameters)))
    (setq tab-bar-tree--persistent-parameters nil)
    ;; Leave the tree window first: advice on `kill-buffer' may only bury
    ;; the current buffer.
    (when (derived-mode-p 'tab-bar-tree-buffer-mode)
      (tab-bar-tree--leave-window))
    ;; Other tabs drop the tree window on selection, see
    ;; `tab-bar-tree--restore-windows'.
    (mapc #'kill-buffer (tab-bar-tree--buffers))
    (setopt tab-bar-show t)))

(provide 'tab-bar-tree)
;;; tab-bar-tree.el ends here

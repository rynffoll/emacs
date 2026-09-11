;;; tab-bar-tree.el --- The tab bar's tabs as a tree in a side window -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "31.1"))

;;; Commentary:

;; A prototype.  The tabs of the tab bar, one per line, in a side window
;; pinned to the same width in every tab, with their groups above them
;; and foldable.
;;
;; Nothing here draws a tab: the labels come from
;; `tab-bar-tab-name-format-function' and
;; `tab-bar-tab-group-format-function', so whatever a config does to the
;; row — hints, marks, indicators, colors — is what the tree shows, and
;; a change to one is a change to both.
;;
;; The tab bar itself is left alone.  Whether it is on and what it shows
;; are its own options, and a tree beside it is no reason to answer
;; them.

;;; Code:

(require 'tab-bar)
(require 'face-remap)

(defgroup tab-bar-tree nil
  "The tab bar's tabs as a tree in a side window."
  :group 'tab-bar)

(defcustom tab-bar-tree-side 'left
  "Side of the frame the tree is shown on."
  :type '(choice (const left) (const right))
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-indent 3
  "Columns a tab is indented by under its group."
  :type 'natnum
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-tab-background nil
  "Whether a label keeps the background its face gives it.
A background tells tabs apart where they sit side by side; down a list
the same plates read as a column of boxes, and the indicator — filled
for the tab you are on — says it without them.  Non-nil draws them
anyway, as the row does.  A change shows on the next render, which
\[tab-bar-tree-refresh] asks for."
  :type 'boolean
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-width 32
  "Width of the tree's window, in columns.
A number rather than a formula off `tab-bar-tab-name-truncated-max' and
the rest of a label's parts: those still bound how a label is cut, but
nothing here recomputes their sum on every render to match, so a tab
count crossing a digit — nine to ten, say — does not widen the window
out from under whatever else is open beside it.  Set by hand instead,
once, to whatever fits.  Only read where a window is first made, so a
change needs `global-tab-bar-tree-mode' turned off and back on to take hold."
  :type 'natnum
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-refresh-hooks
  '(tab-bar-tab-post-select-functions
    tab-bar-tab-post-open-functions
    tab-bar-tab-pre-close-functions
    tab-bar-tab-post-change-group-functions
    window-buffer-change-functions
    window-selection-change-functions)
  "Hooks the tree is brought up to date from.
`tab-bar-tree--refresh' is added to each while the mode is on, whatever
arguments a hook calls it with.  The last two are core's, not tab-bar's:
an unnamed tab's own name is really its selected window's buffer name,
read fresh each time core is asked for it — nothing marks that name
stale, so these are what notice a buffer switch or an `other-window'
changed it.  Both run during redisplay, once per frame actually
redrawn rather than once per command, so nothing here needs debouncing
the way a hook that fired for its own sake once per change would."
  :type '(repeat variable)
  :group 'tab-bar-tree)

(defcustom tab-bar-tree-refresh-commands
  '(tab-bar-move-tab-to)
  "Commands the tree refreshes after, having no hook of their own to answer.
Selecting, opening and closing a tab, or changing one's group, each run
a hook in `tab-bar-tree-refresh-hooks'; moving one does not, so each
command here gets `tab-bar-tree--refresh' as `:after' advice instead, for
as long as the mode stays on.  `tab-bar-move-tab' and
`tab-bar-move-tab-backward' both end up calling this one, so advising
it alone covers both.  A change to either option only takes effect the
next time the mode is turned on."
  :type '(repeat function)
  :group 'tab-bar-tree)

(defvar tab-bar-tree-buffer "*tab-bar-tree*"
  "Name of the buffer the tree is rendered into.")

(defvar tab-bar-tree-collapsed nil
  "Names of the groups whose tabs are folded away.")

(defvar-keymap tab-bar-tree-mode-map
  :doc "Keymap of the tree buffer.
Named for the mode, which is how `define-derived-mode' finds it."
  :parent special-mode-map
  "RET"     #'tab-bar-tree-select
  "TAB"     #'tab-bar-tree-select
  ;; The press is what would select the window — `mouse-drag-region'
  ;; does that before any binding of the release runs — and the tree is
  ;; read from wherever you already are.
  "<down-mouse-1>" #'ignore
  "<mouse-1>" #'tab-bar-tree-select
  "n"       #'next-line
  "p"       #'previous-line
  "g"       #'tab-bar-tree-refresh)

(defun tab-bar-tree-select (&optional event)
  "Visit the tab on this line, or fold the group on it.
EVENT is the click that asked, if a click did.  A click has already
selected this window as part of being dispatched, before this command
ever ran, so it hands focus back before acting — `tab-bar-select-tab'
would otherwise see this window as current and save it as such for the
tab being left.  A key does not: `tab-bar-tree-get-focus' is the only
thing meant to bring focus here."
  (interactive (list last-input-event))
  (let* ((position (and (mouse-event-p event) (event-start event)))
         (window (if position (posn-window position) (selected-window)))
         ;; Selected only for as long as the line is read: a line
         ;; answers as a whole, from its start, wherever on it a key or
         ;; a click left point.
         (line (with-selected-window window
                 (when position (goto-char (posn-point position)))
                 (get-text-property (line-beginning-position) 'tab-bar-tree))))
    ;; `get-mru-window' skips this window on its own: dedicated,
    ;; `no-other-window', and, the click having just selected it, the
    ;; selected one.
    (when position
      (when-let* ((other (get-mru-window nil nil t t)))
        (select-window other)))
    (pcase line
      (`(tab . ,i) (tab-bar-select-tab i))
      (`(group . ,group)
       (setq tab-bar-tree-collapsed
             (if (member group tab-bar-tree-collapsed)
                 (delete group tab-bar-tree-collapsed)
               (cons group tab-bar-tree-collapsed)))
       (tab-bar-tree--render)))))

(defun tab-bar-tree--group-string (tab i current-p open-p)
  "Format TAB's group label at index I, CURRENT-P and OPEN-P as they read.
The formatter takes only CURRENT-P and reads openness off
`tab-bar-show-inactive-group-tabs' — which is exactly what a tree folds
by hand, so it is bound rather than passed."
  (let ((tab-bar-show-inactive-group-tabs open-p))
    (funcall tab-bar-tab-group-format-function tab i current-p)))

(defun tab-bar-tree--tab-string (tab i)
  "Format TAB's own label at index I.
Two arguments: the formatter reads the name off the tab itself and runs
`tab-bar-tab-name-format-functions' over it."
  (funcall tab-bar-tab-name-format-function tab i))

(defun tab-bar-tree--render ()
  "Render the tree into `tab-bar-tree-buffer' and return that buffer."
  (let ((buf (get-buffer-create tab-bar-tree-buffer)))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (point (point))
             (previous-group nil)
             (i 0)
             (current-group (funcall tab-bar-tab-group-function
                                     (tab-bar--current-tab-find))))
        ;; The group you are in is never folded: its tabs have nowhere
        ;; else to go, same as on the tab bar itself.  Enforced here
        ;; rather than where a fold is toggled or a tab is selected, so
        ;; leaving a fold on the group you have just switched into, by
        ;; either path, cannot outlive this render.
        (setq tab-bar-tree-collapsed (delete current-group tab-bar-tree-collapsed))
        (erase-buffer)
        (dolist (tab (funcall tab-bar-tabs-function))
          (setq i (1+ i))
          (let* ((group (alist-get 'group tab))
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
            ;; A folded group still shows the tab you are on: the tree
            ;; says where you are before it says how it is folded.
            (unless (and collapsed (not current-p))
              ;; The indent carries the line's property too, so a key
              ;; that leaves point at column zero reaches the same tab a
              ;; click on the label does.
              (insert (propertize
                       (concat (make-string (if group tab-bar-tree-indent 0) ?\s)
                               (tab-bar-tree--tab-string tab i))
                       'tab-bar-tree (cons 'tab i)
                       'mouse-face 'highlight)
                      "\n"))))
        (goto-char (min point (point-max))))
      (unless (derived-mode-p 'tab-bar-tree-mode)
        (tab-bar-tree-mode))
      (tab-bar-tree--remap-faces))
    buf))

(defvar-local tab-bar-tree--remap-cookies nil
  "What `tab-bar-tree--remap-faces' has to take back before it runs again.")

(defvar tab-bar-tree--faces
  '(tab-bar tab-bar-tab tab-bar-tab-inactive tab-bar-tab-ungrouped
    tab-bar-tab-group-current tab-bar-tab-group-inactive)
  "Faces the tree draws on its own background instead of theirs.")

(defun tab-bar-tree--remap-faces ()
  "Put the labels on the background `tab-bar-tree-tab-background' asks for.
Redone on each render rather than once: the option can change, and so
can the color, which is the theme's."
  (mapc #'face-remap-remove-relative tab-bar-tree--remap-cookies)
  (setq tab-bar-tree--remap-cookies nil)
  (unless tab-bar-tree-tab-background
    (let ((background (face-background 'default nil 'default)))
      (dolist (face tab-bar-tree--faces)
        (when (facep face)
          (push (face-remap-add-relative face :background background)
                tab-bar-tree--remap-cookies))))))

(define-derived-mode tab-bar-tree-mode special-mode "TabTree"
  "Major mode of the tree buffer."
  (setq buffer-read-only t
        cursor-type nil
        truncate-lines t
        mode-line-format nil)
  ;; Nothing is edited here, so no window has to say where point is —
  ;; and the tree is read from a window other than the selected one,
  ;; which is where the hollow box would otherwise be drawn.
  (setq-local cursor-in-non-selected-windows nil)
  ;; No margins, and no right fringe: nothing here wraps or continues.
  ;; The left one stays, at a few pixels rather than the default: bare,
  ;; a label's own left edge sits flush against the window's.
  (setq-local left-fringe-width 4
              right-fringe-width 0
              left-margin-width 0
              right-margin-width 0)
  ;; As `speedbar-window-mode' does: point past the edge of a narrow
  ;; window would otherwise drag the whole view sideways.
  (setq-local auto-hscroll-mode nil))

(defun tab-bar-tree--window ()
  "Return the tree's window, creating and configuring it if there is none.
Callers stay off a child frame themselves; see `tab-bar-tree--refresh'.
Fringes are not among the parameters set here: `tab-bar-tree-mode'
already makes them buffer-local, and a window takes those on for any
buffer it shows without being told to."
  (let* ((buffer (get-buffer-create tab-bar-tree-buffer))
         (window (or (get-buffer-window buffer (selected-frame))
                     (display-buffer-in-side-window
                      buffer
                      `((side . ,tab-bar-tree-side)
                        (slot . 0)
                        (window-width . ,tab-bar-tree-width)
                        ;; `side', not t: `quit-restore-window' tries to
                        ;; delete a plain-dedicated window outright on
                        ;; `quit-window', a side one only past that.
                        (dedicated . side)
                        (preserve-size . (t . nil))
                        (window-parameters
                         . ((no-other-window . t)
                            (no-delete-other-windows . t)
                            (window-size-fixed . width)
                            ;; `quit-window' and `C-x 0' both end up
                            ;; deleting a window through the public
                            ;; `delete-window', which honors this —
                            ;; `tab-bar-tree--forget-window' clears it
                            ;; first, for closing the window down when
                            ;; the mode itself does.
                            (delete-window . ignore)
                            (mode-line-format . none)
                            (header-line-format . none))))))))
    ;; `quit-restore-window' un-dedicates a window along some of its own
    ;; paths before finding out it cannot delete it either; reasserted
    ;; here rather than trusted to stick.
    (set-window-dedicated-p window 'side)
    window))

(defvar tab-bar-tree--updating nil
  "Non-nil while `tab-bar-tree--refresh' is already running.
Creating its window or buffer is itself a window or buffer change, so a
hook watching for exactly that — `window-buffer-change-functions' among
`tab-bar-tree-refresh-hooks' — would otherwise call this right back
from inside itself.")

(defun tab-bar-tree--refresh (&rest _)
  "Render the tree and show it.
Skipped on a child frame — a `posframe' or the like — same as
`tab-bar-tree--window' skips showing one there, and for the same
reason, but earlier: the buffer is one shared by every frame, and
rendering it for a child frame's own, near-empty tabs would overwrite
what the real frame just showed, for anyone still looking at it."
  (unless (or tab-bar-tree--updating
             (frame-parameter (selected-frame) 'parent-frame))
    (let ((tab-bar-tree--updating t))
      (tab-bar-tree--render)
      (tab-bar-tree--window))))

;;;###autoload
(defun tab-bar-tree-refresh ()
  "Render the tree and show it.
The public entry point — bound to \\[tab-bar-tree-refresh] — for
`tab-bar-tree--refresh', which every hook and command in
`tab-bar-tree-refresh-hooks' and `tab-bar-tree-refresh-commands' calls
directly instead, the same way any of them would call this."
  (interactive)
  (tab-bar-tree--refresh))

;;;###autoload
(defun tab-bar-tree-get-focus ()
  "Select the tree's window, showing one first if there is none.
A click is not a reliable way there: Emacs selects a window as part of
dispatching the click that lands on it, before any binding of that
click runs, so a click cannot itself be made to land in the tree
without also leaving it selected — this does the same on purpose,
without one."
  (interactive)
  (when-let* ((window (tab-bar-tree--refresh)))
    (select-window window)))

(defun tab-bar-tree--forget-window (window)
  "Delete WINDOW, undoing the parameters that refuse that."
  (set-window-parameter window 'no-delete-other-windows nil)
  (set-window-parameter window 'delete-window nil)
  (delete-window window))

(defun tab-bar-tree--strip-tab (tab)
  "Return TAB with any window on the tree's buffer taken out of its state.
Each tab keeps its own window configuration — which is how the window
this mode opens comes to live in one tab and not another in the first
place — so it is TAB's turn on the frame that is searched, briefly and
off whatever is really on screen; the caller puts that back."
  (let ((wc (alist-get 'wc tab))
        (ws (alist-get 'ws tab))
        (window-restore-killed-buffer-windows nil))
    (cond
     ((and (window-configuration-p wc)
           (eq (window-configuration-frame wc) (selected-frame)))
      (set-window-configuration wc nil t)
      (when-let* ((window (get-buffer-window tab-bar-tree-buffer)))
        (tab-bar-tree--forget-window window)
        (setf (alist-get 'wc tab) (current-window-configuration))))
     (ws
      (window-state-put ws nil 'safe)
      (when-let* ((window (get-buffer-window tab-bar-tree-buffer)))
        (tab-bar-tree--forget-window window)
        (setf (alist-get 'ws tab)
              (window-state-get (frame-root-window) 'writable))))))
  tab)

(defun tab-bar-tree--kill-window ()
  "Close the tree's window on every tab, not only the one on screen.
Going through `tab-bar-select-tab' to reach the others would run
`tab-bar-tab-post-select-functions' for each — `per-tab-group-theme'
answers to that by loading a theme, once per tab switched through — so
this materializes a tab's own configuration on the frame instead of
switching to it, which nothing is subscribed to."
  (when-let* ((window (get-buffer-window tab-bar-tree-buffer t)))
    (tab-bar-tree--forget-window window))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current (tab-bar--current-tab-index tabs))
         (restore (current-window-configuration)))
    (unwind-protect
        (dotimes (i (length tabs))
          (unless (eq i current)
            (setf (nth i tabs) (tab-bar-tree--strip-tab (nth i tabs)))))
      (set-window-configuration restore nil t))))

;;;###autoload
(define-minor-mode global-tab-bar-tree-mode
  "Show the tabs of the tab bar as a tree in a side window."
  :global t
  (if global-tab-bar-tree-mode
      (progn
        (tab-bar-tree--refresh)
        (dolist (hook tab-bar-tree-refresh-hooks)
          (add-hook hook #'tab-bar-tree--refresh))
        (dolist (command tab-bar-tree-refresh-commands)
          (advice-add command :after #'tab-bar-tree--refresh)))
    (dolist (hook tab-bar-tree-refresh-hooks)
      (remove-hook hook #'tab-bar-tree--refresh))
    (dolist (command tab-bar-tree-refresh-commands)
      (advice-remove command #'tab-bar-tree--refresh))
    ;; The window first, on every tab, and only then the buffer: killing
    ;; the buffer first would leave the other tabs' own window
    ;; configurations pointing at a dead buffer object, which Emacs
    ;; fills back in with an "Old buffer" placeholder on the next visit
    ;; rather than just closing the window the way this does.
    (tab-bar-tree--kill-window)
    (when (get-buffer tab-bar-tree-buffer)
      (kill-buffer tab-bar-tree-buffer))))

(provide 'tab-bar-tree)
;;; tab-bar-tree.el ends here

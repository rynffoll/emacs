;;; tab-bar-mark.el --- Attention indicator for tab-bar tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: convenience, tools
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

;; Marks a tab-bar tab as needing attention with a colored symbol after
;; its name.  A notifier picks the tabs it means with `tab-bar-mark-if'
;; and a predicate: what makes a tab the right one is the caller's
;; business.
;;
;; The mark is a tab parameter, so it dies with the tab, leaving no
;; bookkeeping behind.  It stays up until its tab comes into view — the
;; selected tab of a frame holding input focus — and goes the moment it
;; does.  Which is also why marking a tab already in view does nothing:
;; it has been seen.  `tab-bar-mark-toggle' is the one exception: a mark
;; put on the current tab by hand stays until the tab is next viewed.
;;
;; Add `tab-bar-mark-tab-name-format' to
;; `tab-bar-tab-name-format-functions' yourself, after
;; `tab-bar-tab-name-format-truncated' — see its docstring for why
;; there.  This mode only owns the mark's own lifecycle, not where it
;; is shown.

;;; Code:

(require 'tab-bar)
(require 'seq)

(defgroup tab-bar-mark nil
  "Attention indicator for tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-mark-")

(defcustom tab-bar-mark-symbol "●"
  "Symbol shown after a marked tab's name."
  :type 'string
  :group 'tab-bar-mark)

(defface tab-bar-mark '((t :inherit warning))
  "Face for `tab-bar-mark-symbol'."
  :group 'tab-bar-mark)

(defun tab-bar-mark--set (tab state)
  "Set TAB's mark parameter to STATE, non-nil for a mark.
Return non-nil if that changed anything: a redisplay forced for nothing
costs a full pass over every window of every frame."
  (let ((cell (assq 'tab-bar-mark tab)))
    (cond (cell  (unless (eq (cdr cell) state)
                   (setcdr cell state)
                   t))
          (state (nconc tab (list (cons 'tab-bar-mark state)))
                 t))))

(defun tab-bar-mark--in-view-p (tab frame)
  "Whether TAB is FRAME's selected tab and FRAME holds input focus.
Cheap test first: `frame-focus-state' reads a frame parameter, and that
copies the frame's whole parameter list.  Its `unknown' counts as focus,
so on a terminal that reports none a mark never shows rather than never
goes."
  (and (eq (car tab) 'current-tab)
       (frame-focus-state frame)))

(defun tab-bar-mark--map-tabs (fn)
  "Call FN with the frame and tab of every tab on every frame.
Child frames are skipped: no tab bar there, so no mark to see."
  (dolist (frame (frame-list))
    (unless (frame-parent frame)
      (dolist (tab (frame-parameter frame 'tabs))
        (funcall fn frame tab)))))

;;;###autoload
(defun tab-bar-mark-p (tab)
  "Whether TAB carries an attention mark."
  (alist-get 'tab-bar-mark tab))

;;;###autoload
(defun tab-bar-mark-toggle ()
  "Toggle the attention mark on the current tab."
  (interactive)
  (let ((tab (tab-bar--current-tab-find)))
    (when (tab-bar-mark--set tab (not (tab-bar-mark-p tab)))
      (force-mode-line-update t))))

;;;###autoload
(defun tab-bar-mark-if (predicate)
  "Mark every tab PREDICATE returns non-nil for, on every frame.
PREDICATE is called with one argument, the tab, which is only valid for
the duration of the call.  A tab in view is left alone: it has been
seen.  Return the number of tabs left marked."
  (let ((marked 0) changed)
    (tab-bar-mark--map-tabs
     (lambda (frame tab)
       (when (and (funcall predicate tab)
                  (not (tab-bar-mark--in-view-p tab frame)))
         (setq marked (1+ marked))
         (when (tab-bar-mark--set tab t)
           (setq changed t)))))
    (when changed (force-mode-line-update t))
    marked))

(defun tab-bar-mark--clear-on-view (&rest _)
  "Clear the mark on every tab that is now in view.
Serves both hooks: selecting a tab fires no focus change, and coming
back from another application fires no tab selection.  Every frame,
since `after-focus-change-function' does not promise that the focused
frame is the selected one."
  (let (cleared)
    (tab-bar-mark--map-tabs
     (lambda (frame tab)
       (when (and (tab-bar-mark-p tab)
                  (tab-bar-mark--in-view-p tab frame)
                  (tab-bar-mark--set tab nil))
         (setq cleared t))))
    (when cleared (force-mode-line-update t))))

;;;###autoload
(define-minor-mode tab-bar-mark-mode
  "Show a colored attention symbol on tab-bar tabs marked via `tab-bar-mark-if'.
Does not place the symbol itself — add `tab-bar-mark-tab-name-format'
to `tab-bar-tab-name-format-functions' where it belongs; this mode only
owns the mark's own lifecycle."
  :group 'tab-bar-mark
  :global t
  (if tab-bar-mark-mode
      (progn
        (add-hook 'tab-bar-tab-post-select-functions #'tab-bar-mark--clear-on-view)
        (add-function :after after-focus-change-function #'tab-bar-mark--clear-on-view)
        ;; Marks outlive the mode, and one may sit on the tab in view.
        (tab-bar-mark--clear-on-view))
    (remove-hook 'tab-bar-tab-post-select-functions #'tab-bar-mark--clear-on-view)
    (remove-function after-focus-change-function #'tab-bar-mark--clear-on-view))
  (force-mode-line-update t))

;;;###autoload
(defun tab-bar-mark-group-p (group)
  "Whether any tab of GROUP carries an attention mark.
Nil with `tab-bar-mark-mode' off, as the other formatting helpers here
are no-ops then, so a label formatter may ask unconditionally: a group
that is not the current one draws none of its tabs, and the mark inside
it is then drawn nowhere else."
  (and tab-bar-mark-mode
       group
       ;; The marked tabs and not every tab: usually none or one, where
       ;; the group of a tab is a call to a function a config may
       ;; replace.
       (seq-some (lambda (tab)
                   (equal (funcall tab-bar-tab-group-function tab) group))
                 (tab-bar-mark-marked-tabs))))

;;;###autoload
(defun tab-bar-mark-marked-tabs (&optional frame)
  "Return the tabs of FRAME that carry an attention mark."
  (seq-filter #'tab-bar-mark-p (funcall tab-bar-tabs-function frame)))

(defun tab-bar-mark--switch-names ()
  "Return the marked tabs' names, the most recently visited one first.
Ordered by `tab-bar--tabs-recent', which is asked only once there is
something to sort: an empty list is nothing it can tell from no list at
all, and it answers with every tab of the frame."
  (when-let* ((marked (tab-bar-mark-marked-tabs)))
    (mapcar (lambda (tab) (alist-get 'name tab))
            (tab-bar--tabs-recent marked))))

;;;###autoload
(defun tab-bar-mark-switch-to-tab (name)
  "Switch to the marked tab by NAME.
Shaped after `tab-bar-switch-to-tab', and switching through it: the
default values are the marked tabs sorted by recency, so
\<minibuffer-local-map>\[next-history-element] answers with the most
recently visited one, the second most recent, and so on.  Selecting a
tab is also what takes its mark off."
  (interactive
   (let ((names (tab-bar-mark--switch-names)))
     (unless names (user-error "No marked tabs"))
     (list (completing-read
            (format-prompt "Switch to marked tab" (car names))
            ;; The table says what its candidates are, so a front end
            ;; annotates them without being told: `tab' is a category
            ;; marginalia ships an annotator for.
            (completion-table-with-metadata names '((category . tab)))
            nil t nil nil names))))
  (tab-bar-switch-to-tab name))

;;;###autoload
(defun tab-bar-mark-tab-name-format (name tab _i)
  "Suffix NAME with `tab-bar-mark-symbol' when TAB is marked.
A no-op with `tab-bar-mark-mode' off, so callers may list this in
`tab-bar-tab-name-format-functions' unconditionally: it belongs after
`tab-bar-tab-name-format-truncated' — ahead of it the symbol would be
truncated away along with the name — and ahead of the hint and face
formatters, so a hint number lands in front of the name and the tab's
own face and padding cover the symbol."
  (if (and tab-bar-mark-mode (tab-bar-mark-p tab))
      (concat name " " (propertize tab-bar-mark-symbol 'face 'tab-bar-mark))
    name))

(provide 'tab-bar-mark)
;;; tab-bar-mark.el ends here

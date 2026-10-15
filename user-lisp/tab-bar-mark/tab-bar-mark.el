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

;; Attention marks for tab-bar tabs: colored symbols after the tab name.
;; Callers select tabs with `tab-bar-mark-set-if' and a predicate.  A tab
;; can carry several marks, one per type in `tab-bar-mark-type-alist'.
;;
;; Marks are stored as a tab parameter and disappear with the tab.  A
;; mark is removed when its tab comes into view (the selected tab of a
;; focused frame), unless its type is persistent; for the same reason a
;; non-persistent mark is not set on a tab in view.  A mark set with
;; `tab-bar-mark-toggle' stays until the tab is viewed again.
;;
;; The mode only manages marks.  To display them, add
;; `tab-bar-mark-tab-name-format' to `tab-bar-tab-name-format-functions'
;; after `tab-bar-tab-name-format-truncated'.

;;; Code:

(require 'tab-bar)
(require 'seq)

(defgroup tab-bar-mark nil
  "Attention indicator for tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-mark-")

(defcustom tab-bar-mark-type-alist
  '((:warning :symbol "●" :face warning)
    (:error   :symbol "●" :face error)
    (:success :symbol "●" :face success)
    (:flag    :symbol "⚑" :face shadow :persistent t))
  "Types of mark, as (TYPE . PROPERTIES), drawn in this order.
PROPERTIES is a plist:
  :symbol      string shown for the mark
  :face        face of that string
  :persistent  non-nil to keep the mark when its tab comes into view
A type missing here is drawn as `:warning'."
  :type '(alist :key-type (symbol :tag "Type")
                :value-type (plist :options ((:symbol string)
                                             (:face face)
                                             (:persistent boolean))))
  :group 'tab-bar-mark)

(defun tab-bar-mark--type (type)
  "Return the properties `tab-bar-mark-type-alist' has for TYPE."
  (or (alist-get type tab-bar-mark-type-alist)
      (alist-get :warning tab-bar-mark-type-alist)))

(defun tab-bar-mark--set (tab type state)
  "Set TYPE's mark on TAB if STATE is non-nil, else remove it.
TYPE t with STATE nil removes all marks.  Return non-nil if TAB changed,
so callers force a redisplay only when needed."
  (let* ((types (tab-bar-mark-p tab))
         (new (cond (state (if (memq type types)
                               types
                             (append types (list type))))
                    ((eq type t) nil)
                    (t (remq type types)))))
    (unless (equal new types)
      ;; On the cdr: a new entry goes after the tab's type symbol.
      (setf (alist-get 'tab-bar-mark (cdr tab)) new)
      t)))

(defun tab-bar-mark--in-view-p (tab frame)
  "Return non-nil if TAB is FRAME's selected tab and FRAME has focus.
The tab type is checked first: `frame-focus-state' copies the frame's
parameter list.  Its `unknown' value counts as focus, so on a terminal
without focus reports marks are not shown rather than never removed."
  (and (eq (car tab) 'current-tab)
       (frame-focus-state frame)))

(defun tab-bar-mark--map-tabs (fn)
  "Call FN with the frame and tab for every tab of every frame.
Child frames, which have no tab bar, are skipped."
  (dolist (frame (get-mru-frames t t))
    (dolist (tab (frame-parameter frame 'tabs))
      (funcall fn frame tab))))

(defun tab-bar-mark--set-where (type state test)
  "Set TYPE's mark to STATE on every tab TEST returns non-nil for.
TEST is called with the frame and the tab.  Return the number of tabs
whose marks changed, or nil if none did."
  (let ((count 0))
    (tab-bar-mark--map-tabs
     (lambda (frame tab)
       (when (and (funcall test frame tab)
                  (tab-bar-mark--set tab type state))
         (setq count (1+ count)))))
    (when (> count 0)
      (force-mode-line-update t)
      count)))

;;;###autoload
(defun tab-bar-mark-p (tab &optional type)
  "Return the types of TAB's marks, or non-nil if one is TYPE."
  (let ((types (alist-get 'tab-bar-mark tab)))
    ;; Value t: a mark saved before mark types existed (desktop).
    (when (and types (atom types))
      (setq types (list :warning)))
    (if type (memq type types) types)))

(defun tab-bar-mark--annotate (candidate)
  "Return the annotation for mark type CANDIDATE: its symbol and persistence."
  (let ((type (intern candidate))
        ;; Align to a column after the longest name, as marginalia does.
        (width (+ 4 (seq-max
                     (mapcar (lambda (entry) (length (symbol-name (car entry))))
                             tab-bar-mark-type-alist)))))
    (concat (make-string (max 1 (- width (length candidate))) ?\s)
            (tab-bar-mark-string (list type))
            (and (plist-get (tab-bar-mark--type type) :persistent)
                 (propertize "   persistent" 'face 'completions-annotations)))))

;;;###autoload
(defun tab-bar-mark-toggle (&optional type)
  "Toggle TYPE's mark on the current tab; TYPE defaults to `:flag'.
Interactively, ask for TYPE when `tab-bar-mark-type-alist' has more than one."
  (interactive
   (list (and (cdr tab-bar-mark-type-alist)
              (let ((set (tab-bar-mark-p (tab-bar--current-tab-find))))
                (intern (completing-read
                         "Toggle mark: "
                         ;; Own category; otherwise completion frameworks
                         ;; annotate the candidates as Lisp symbols.
                         (completion-table-with-metadata
                          ;; Types set on the tab in bold, others dimmed.
                          (mapcar (lambda (entry)
                                    (propertize (symbol-name (car entry))
                                                'face (if (memq (car entry) set)
                                                          'bold
                                                        'shadow)))
                                  tab-bar-mark-type-alist)
                          '((category . tab-bar-mark-type)
                            (annotation-function . tab-bar-mark--annotate)
                            (display-sort-function . identity)))
                         nil t nil nil (symbol-name :flag)))))))
  (let ((type (or type :flag))
        (tab (tab-bar--current-tab-find)))
    (when (tab-bar-mark--set tab type (not (tab-bar-mark-p tab type)))
      (force-mode-line-update t))))

;;;###autoload
(defun tab-bar-mark-set-if (predicate &optional type)
  "Set TYPE's mark on every tab of every frame that satisfies PREDICATE.
TYPE defaults to `:warning'.  PREDICATE is called with the tab, which is
valid only during the call.  Unless TYPE is persistent, tabs in view are
skipped.  Return the number of changed tabs, or nil if none changed."
  (let* ((type (or type :warning))
         (persistent (plist-get (tab-bar-mark--type type) :persistent)))
    (tab-bar-mark--set-where
     type t (lambda (frame tab)
              (and (funcall predicate tab)
                   (or persistent
                       (not (tab-bar-mark--in-view-p tab frame))))))))

;;;###autoload
(defun tab-bar-mark-unset-if (predicate &optional type)
  "Remove TYPE's mark from every tab of every frame that satisfies PREDICATE.
TYPE defaults to `:warning'; t removes all marks.  Return the number of
changed tabs, or nil if none changed."
  (tab-bar-mark--set-where (or type :warning) nil
                           (lambda (_frame tab) (funcall predicate tab))))

(defun tab-bar-mark--clear-on-view (&rest _)
  "Remove non-persistent marks from every tab in view.
Runs on tab selection and on focus change; neither covers the other.
Checks every frame: `after-focus-change-function' does not guarantee
that the focused frame is the selected one."
  (let (seen types)
    (tab-bar-mark--map-tabs
     (lambda (frame tab)
       (when-let* ((marks (tab-bar-mark-p tab))
                   ((tab-bar-mark--in-view-p tab frame)))
         (push tab seen)
         (setq types (seq-union types marks)))))
    ;; Via `tab-bar-mark-unset-if', and only for marks that are set, so
    ;; advice on it runs only when a mark is removed.
    (dolist (type types)
      (unless (plist-get (tab-bar-mark--type type) :persistent)
        (tab-bar-mark-unset-if (lambda (tab) (memq tab seen)) type)))))

;;;###autoload
(define-minor-mode tab-bar-mark-mode
  "Show colored attention symbols on tabs marked by `tab-bar-mark-set-if'.
Manages mark removal only; to display marks, add
`tab-bar-mark-tab-name-format' to `tab-bar-tab-name-format-functions'."
  :group 'tab-bar-mark
  :global t
  (if tab-bar-mark-mode
      (progn
        (add-hook 'tab-bar-tab-post-select-functions #'tab-bar-mark--clear-on-view)
        (add-function :after after-focus-change-function #'tab-bar-mark--clear-on-view)
        ;; Marks persist while the mode is off; clear the tab in view.
        (tab-bar-mark--clear-on-view))
    (remove-hook 'tab-bar-tab-post-select-functions #'tab-bar-mark--clear-on-view)
    (remove-function after-focus-change-function #'tab-bar-mark--clear-on-view))
  (force-mode-line-update t))

;;;###autoload
(defun tab-bar-mark-group-p (group)
  "Return the mark types of GROUP's tabs, in drawing order.
Return nil if `tab-bar-mark-mode' is off, like the other display
helpers, so group label formatters can call it unconditionally."
  (and tab-bar-mark-mode
       group
       ;; Only marked tabs: computing a tab's group calls a function.
       (tab-bar-mark--sort
        (seq-uniq
         (seq-mapcat (lambda (tab)
                       (and (equal (funcall tab-bar-tab-group-function tab)
                                   group)
                            (tab-bar-mark-p tab)))
                     (tab-bar-mark-marked-tabs))))))

(defun tab-bar-mark--sort (types)
  "Return TYPES sorted by `tab-bar-mark-type-alist', unknown types last."
  (let ((order (mapcar #'car tab-bar-mark-type-alist)))
    (seq-sort-by (lambda (type) (or (seq-position order type) (length order)))
                 #'< types)))

;;;###autoload
(defun tab-bar-mark-string (types &optional face)
  "Return the symbols of mark TYPES, concatenated in drawing order.
FACE, if non-nil, is added under each symbol's own face."
  (mapconcat (lambda (type)
               (let ((spec (tab-bar-mark--type type)))
                 (propertize (plist-get spec :symbol)
                             'face (if face
                                       (list (plist-get spec :face) face)
                                     (plist-get spec :face)))))
             (tab-bar-mark--sort types)
             ""))

;;;###autoload
(defun tab-bar-mark-marked-tabs (&optional frame)
  "Return FRAME's tabs that have a mark."
  (seq-filter #'tab-bar-mark-p (funcall tab-bar-tabs-function frame)))

(defun tab-bar-mark--switch-names ()
  "Return the names of marked tabs, most recently visited first.
`tab-bar--tabs-recent' is not called with an empty list: it treats nil
as \"all tabs of the frame\"."
  (when-let* ((marked (tab-bar-mark-marked-tabs)))
    (mapcar (lambda (tab) (alist-get 'name tab))
            (tab-bar--tabs-recent marked))))

;;;###autoload
(defun tab-bar-mark-switch-to-tab (name)
  "Switch to the marked tab NAME.
Like `tab-bar-switch-to-tab', which it calls.  The default values are
the marked tabs, most recent first, available via
\<minibuffer-local-map>\[next-history-element].  Selecting the tab
removes its non-persistent marks."
  (interactive
   (let ((names (tab-bar-mark--switch-names)))
     (unless names (user-error "No marked tabs"))
     (list (completing-read
            (format-prompt "Switch to marked tab" (car names))
            ;; Category `tab': marginalia has an annotator for it.
            (completion-table-with-metadata names '((category . tab)))
            nil t nil nil names))))
  (tab-bar-switch-to-tab name))

;;;###autoload
(defun tab-bar-mark-tab-name-format (name tab _i)
  "Append the symbols of TAB's marks to NAME.
No-op if `tab-bar-mark-mode' is off.  In
`tab-bar-tab-name-format-functions', put it after
`tab-bar-tab-name-format-truncated', so truncation does not cut the
symbols, and before the hint and face formatters, so the hint precedes
the name and the tab face covers the symbols."
  (if-let* ((types (and tab-bar-mark-mode (tab-bar-mark-p tab))))
      (concat name " " (tab-bar-mark-string types))
    name))

(provide 'tab-bar-mark)
;;; tab-bar-mark.el ends here

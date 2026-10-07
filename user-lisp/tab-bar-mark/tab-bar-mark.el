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
;; its name.  A notifier picks the tabs it means with
;; `tab-bar-mark-set-if' and a predicate: what makes a tab the right one
;; is the caller's business.  A tab carries any number of marks, one per
;; type of `tab-bar-mark-type-alist', each with its own symbol.
;;
;; The marks are a tab parameter, so they die with the tab, leaving no
;; bookkeeping behind.  A mark stays up until its tab comes into view —
;; the selected tab of a frame holding input focus — and goes the moment
;; it does, unless its type is persistent.  Which is also why setting a
;; mark that is not persistent on a tab already in view does nothing:
;; it has been seen.  `tab-bar-mark-toggle' is the one exception: a mark put on the
;; current tab by hand stays until the tab is next viewed.
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
  "Put TYPE's mark on TAB if STATE is non-nil, else take it off.
TYPE t with STATE nil takes every mark off.  Return non-nil if that
changed anything: a redisplay forced for nothing costs a full pass over
every window of every frame."
  (let* ((types (tab-bar-mark-p tab))
         (new (cond (state (if (memq type types)
                               types
                             (append types (list type))))
                    ((eq type t) nil)
                    (t (remq type types)))))
    (unless (equal new types)
      ;; On the cdr, so a new entry goes in behind the tab's own type.
      (setf (alist-get 'tab-bar-mark (cdr tab)) new)
      t)))

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
    ;; A lone t is a mark from before there were types, restored by desktop.
    (when (and types (atom types))
      (setq types (list :warning)))
    (if type (memq type types) types)))

(defun tab-bar-mark--annotate (candidate)
  "Return the mark of the type named CANDIDATE, and whether it persists."
  (let ((type (intern candidate))
        ;; A column past the longest name, as marginalia lines them up.
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
                         ;; A category of its own, or the types pass for
                         ;; Lisp symbols and get annotated as such.
                         (completion-table-with-metadata
                          ;; In bold, the types the tab carries; dimmed,
                          ;; the rest.
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
  "Put TYPE's mark on every tab PREDICATE returns non-nil for, on every frame.
TYPE defaults to `:warning'.  PREDICATE is called with the tab, valid
only for the duration of the call.  Unless TYPE is persistent, a tab in
view is left alone: it has been seen.  Return the number of tabs whose
marks changed, or nil if none did."
  (let* ((type (or type :warning))
         (persistent (plist-get (tab-bar-mark--type type) :persistent)))
    (tab-bar-mark--set-where
     type t (lambda (frame tab)
              (and (funcall predicate tab)
                   (or persistent
                       (not (tab-bar-mark--in-view-p tab frame))))))))

;;;###autoload
(defun tab-bar-mark-unset-if (predicate &optional type)
  "Take TYPE's mark off every tab PREDICATE returns non-nil for, on every frame.
TYPE defaults to `:warning'; t takes every mark off.  Return the number
of tabs whose marks changed, or nil if none did."
  (tab-bar-mark--set-where (or type :warning) nil
                           (lambda (_frame tab) (funcall predicate tab))))

(defun tab-bar-mark--clear-on-view (&rest _)
  "Take the marks that are not persistent off every tab now in view.
Serves both hooks: selecting a tab fires no focus change, and coming
back from another application fires no tab selection.  Every frame,
since `after-focus-change-function' does not promise that the focused
frame is the selected one."
  (let (seen types)
    (tab-bar-mark--map-tabs
     (lambda (frame tab)
       (when-let* ((marks (tab-bar-mark-p tab))
                   ((tab-bar-mark--in-view-p tab frame)))
         (push tab seen)
         (setq types (seq-union types marks)))))
    ;; Only through `tab-bar-mark-unset-if', and only for a mark there
    ;; is, so whatever follows it runs when a mark actually goes.
    (dolist (type types)
      (unless (plist-get (tab-bar-mark--type type) :persistent)
        (tab-bar-mark-unset-if (lambda (tab) (memq tab seen)) type)))))

;;;###autoload
(define-minor-mode tab-bar-mark-mode
  "Show colored attention symbols on tabs marked by `tab-bar-mark-set-if'.
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
  "Return the types of the marks the tabs of GROUP carry, in drawing order.
Nil with `tab-bar-mark-mode' off, as the other formatting helpers here
are no-ops then, so a label formatter may ask unconditionally: a group
that is not the current one draws none of its tabs, and the mark inside
it is then drawn nowhere else."
  (and tab-bar-mark-mode
       group
       ;; The marked tabs and not every tab: usually none or one, where
       ;; the group of a tab is a call to a function a config may
       ;; replace.
       (tab-bar-mark--sort
        (seq-uniq
         (seq-mapcat (lambda (tab)
                       (and (equal (funcall tab-bar-tab-group-function tab)
                                   group)
                            (tab-bar-mark-p tab)))
                     (tab-bar-mark-marked-tabs))))))

(defun tab-bar-mark--sort (types)
  "Return TYPES in the order of `tab-bar-mark-type-alist', unknown ones last."
  (let ((order (mapcar #'car tab-bar-mark-type-alist)))
    (seq-sort-by (lambda (type) (or (seq-position order type) (length order)))
                 #'< types)))

;;;###autoload
(defun tab-bar-mark-string (types &optional face)
  "Return the symbols of the marks of TYPES, side by side, in drawing order.
FACE, if non-nil, goes under each symbol's own face."
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
tab is also what takes off its marks that clear on view."
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
  "Suffix NAME with the symbols of TAB's marks.
A no-op with `tab-bar-mark-mode' off, so callers may list this in
`tab-bar-tab-name-format-functions' unconditionally: it belongs after
`tab-bar-tab-name-format-truncated' — ahead of it the symbol would be
truncated away along with the name — and ahead of the hint and face
formatters, so a hint number lands in front of the name and the tab's
own face and padding cover the symbol."
  (if-let* ((types (and tab-bar-mark-mode (tab-bar-mark-p tab))))
      (concat name " " (tab-bar-mark-string types))
    name))

(provide 'tab-bar-mark)
;;; tab-bar-mark.el ends here

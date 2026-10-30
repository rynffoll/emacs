;;; tab-bar-status.el --- Status of tab-bar tabs -*- lexical-binding: t; -*-

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

;; The status of tab-bar tabs, in two forms:
;;
;; - The current status, reported by sources such as a program in a
;;   terminal: a state, e.g. `working', `blocked' or `done', and a line
;;   of text.  States follow the terminal program status protocol,
;;   OSC 7501; any symbol works.  A source sets its status with
;;   `tab-bar-status-set' and a predicate that finds its tabs.
;;   `tab-bar-status-tab-lines' returns the status lines of a tab, a
;;   value for `tab-bar-tree-tab-status-function'; a spinner replaces a
;;   missing text.  The status lives as long as its source.
;;
;; - Marks, colored symbols after the tab name that keep a status until
;;   the tab is viewed.  Entering a state of
;;   `tab-bar-status-state-mark-alist' marks the tabs of the source;
;;   `tab-bar-status-mark-if' marks tabs directly, and
;;   `tab-bar-status-toggle-mark' sets a flag by hand.  Marks are stored
;;   in a tab parameter and die with the tab.  A tab is in view when it
;;   is the selected tab of a focused frame.  Non-permanent marks are
;;   removed when their tab comes into view and are never set on a tab
;;   in view.  To display marks, add `tab-bar-status-tab-name-format' to
;;   `tab-bar-tab-name-format-functions' after
;;   `tab-bar-tab-name-format-truncated'.
;;
;; `tab-bar-status-change-hook' runs when either changes.

;;; Code:

(require 'tab-bar)
(require 'seq)

(defgroup tab-bar-status nil
  "Status of tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-status-")

(defcustom tab-bar-status-mark-type-alist
  '((:warning :symbol "●" :face warning)
    (:error   :symbol "●" :face error)
    (:success :symbol "●" :face success)
    (:flag    :symbol "⚑" :face shadow :permanent t))
  "Alist of mark types (TYPE . PROPERTIES), in display order.
PROPERTIES is a plist:
  :symbol      string to display
  :face        face of :symbol
  :permanent   non-nil to keep the mark when its tab comes into view
A type not listed here is displayed as `:warning'."
  :type '(alist :key-type (symbol :tag "Type")
                :value-type (plist :options ((:symbol string)
                                             (:face face)
                                             (:permanent boolean))))
  :group 'tab-bar-status)

(defcustom tab-bar-status-state-mark-alist
  '((blocked . :warning) (error . :error) (done . :success))
  "Alist of states and the mark types they set.
A source entering one of the states marks its tabs."
  :type '(alist :key-type (symbol :tag "State")
                :value-type (symbol :tag "Mark type"))
  :group 'tab-bar-status)

(defcustom tab-bar-status-hidden-states '(done idle)
  "List of states without a status line.
Done is shown by its mark; idle has nothing to show."
  :type '(repeat (symbol :tag "State"))
  :group 'tab-bar-status)

(defcustom tab-bar-status-change-hook nil
  "Hook run when marks or status lines change, including spinner frames."
  :type 'hook
  :group 'tab-bar-status)

(defcustom tab-bar-status-spinner-frames '("․" "‥" "…")
  "Spinner frames for a status without text."
  :type '(repeat string)
  :group 'tab-bar-status)

(defcustom tab-bar-status-spinner-interval 0.5
  "Spinner frame interval, in seconds.
Applied on spinner start."
  :type 'number
  :group 'tab-bar-status)

(defvar tab-bar-status-mode)


;;;; Marks

(defun tab-bar-status--marks-changed ()
  "Redisplay the tab bars and run `tab-bar-status-change-hook'."
  (force-mode-line-update t)
  (run-hooks 'tab-bar-status-change-hook))

(defun tab-bar-status--type-properties (type)
  "Return the `tab-bar-status-mark-type-alist' plist of TYPE, or of `:warning'."
  (or (alist-get type tab-bar-status-mark-type-alist)
      (alist-get :warning tab-bar-status-mark-type-alist)))

(defun tab-bar-status--set-mark (tab type on)
  "Set TYPE's mark on TAB if ON is non-nil, else remove it.
If TYPE is t and ON is nil, remove all marks.  Return non-nil if TAB
changed, so callers redisplay only when needed."
  (let* ((types (tab-bar-status-marks tab))
         (new (cond (on (if (memq type types)
                            types
                          (append types (list type))))
                    ((eq type t) nil)
                    (t (remq type types)))))
    (unless (equal new types)
      ;; Use the cdr so a new entry goes after the tab's type symbol.
      ;; No marks: drop the entry, so desktop saves none.
      (setf (alist-get 'tab-bar-status-marks (cdr tab) nil 'remove) new)
      t)))

(defun tab-bar-status--in-view-p (tab frame)
  "Return non-nil if TAB is FRAME's selected tab and FRAME has focus.
A focus state of `unknown' counts as focused, so a terminal without
focus reports still clears the marks of its selected tab."
  (and (eq (car tab) 'current-tab)
       (frame-focus-state frame)))

(defun tab-bar-status--map-tabs (fn)
  "Call FN with FRAME and TAB for every tab of every frame.
Skip child frames, which have no tab bar."
  (dolist (frame (get-mru-frames t t))
    (dolist (tab (frame-parameter frame 'tabs))
      (funcall fn frame tab))))

(defun tab-bar-status--mark-if (type on test)
  "Set TYPE's mark to ON on every tab for which TEST returns non-nil.
TEST is called with FRAME and TAB.  Return the number of changed tabs,
or nil if none changed."
  (let ((count 0))
    (tab-bar-status--map-tabs
     (lambda (frame tab)
       (when (and (funcall test frame tab)
                  (tab-bar-status--set-mark tab type on))
         (setq count (1+ count)))))
    (when (> count 0)
      (tab-bar-status--marks-changed)
      count)))

;;;###autoload
(defun tab-bar-status-marks (tab &optional type)
  "Return the list of TAB's mark types.
If TYPE is non-nil, return non-nil if TAB has a mark of TYPE."
  (let ((types (alist-get 'tab-bar-status-marks tab)))
    (if type (memq type types) types)))

(defun tab-bar-status--annotate (candidate)
  "Return the annotation of mark type CANDIDATE: its symbol and permanence."
  (let ((type (intern candidate))
        ;; Align past the longest name, as marginalia does.
        (width (+ 4 (seq-max
                     (mapcar (lambda (entry) (length (symbol-name (car entry))))
                             tab-bar-status-mark-type-alist)))))
    (concat (make-string (max 1 (- width (length candidate))) ?\s)
            (tab-bar-status--format-type type)
            (and (plist-get (tab-bar-status--type-properties type) :permanent)
                 (propertize "   permanent" 'face 'completions-annotations)))))

;;;###autoload
(defun tab-bar-status-toggle-mark (&optional type)
  "Toggle TYPE's mark on the current tab; TYPE defaults to `:flag'.
Interactively, prompt for TYPE if `tab-bar-status-mark-type-alist' has
several."
  (interactive
   (list (and (cdr tab-bar-status-mark-type-alist)
              (let ((set (tab-bar-status-marks
                          (assq 'current-tab (funcall tab-bar-tabs-function)))))
                (intern (completing-read
                         "Toggle tab mark: "
                         ;; Own category, or completion UIs annotate the
                         ;; candidates as Lisp symbols.
                         (completion-table-with-metadata
                          ;; Bold if set on the tab, else shadow.
                          (mapcar (lambda (entry)
                                    (propertize (symbol-name (car entry))
                                                'face (if (memq (car entry) set)
                                                          'bold
                                                        'shadow)))
                                  tab-bar-status-mark-type-alist)
                          '((category . tab-bar-status-mark-type)
                            (annotation-function . tab-bar-status--annotate)
                            (display-sort-function . identity)))
                         nil t nil nil (symbol-name :flag)))))))
  (let ((type (or type :flag))
        (tab (assq 'current-tab (funcall tab-bar-tabs-function))))
    (when (tab-bar-status--set-mark tab type
                                    (not (tab-bar-status-marks tab type)))
      (tab-bar-status--marks-changed))))

;;;###autoload
(defun tab-bar-status-mark-if (predicate &optional type)
  "Set TYPE's mark on every tab of every frame that satisfies PREDICATE.
TYPE defaults to `:warning'.  PREDICATE is called with the tab, valid
only during the call.  Skip tabs in view unless TYPE is permanent.
Return the number of changed tabs, or nil if none changed."
  (let* ((type (or type :warning))
         (permanent (plist-get (tab-bar-status--type-properties type)
                               :permanent)))
    (tab-bar-status--mark-if
     type t (lambda (frame tab)
              (and (funcall predicate tab)
                   (or permanent
                       (not (tab-bar-status--in-view-p tab frame))))))))

;;;###autoload
(defun tab-bar-status-unmark-if (predicate &optional type)
  "Remove TYPE's mark from every tab of every frame that satisfies PREDICATE.
TYPE defaults to `:warning'; t means all marks.  Return the number of
changed tabs, or nil if none changed."
  (tab-bar-status--mark-if (or type :warning) nil
                           (lambda (_frame tab) (funcall predicate tab))))

;;;###autoload
(defun tab-bar-status-mark-state (predicate state)
  "Mark the tabs PREDICATE matches with the mark type of STATE.
Types come from `tab-bar-status-state-mark-alist'.  Do nothing unless
`tab-bar-status-mode' is on."
  (when-let* ((tab-bar-status-mode)
              (type (alist-get state tab-bar-status-state-mark-alist)))
    (tab-bar-status-mark-if predicate type)))

(defun tab-bar-status--clear-on-view (&rest _)
  "Remove non-permanent marks from every tab in view.
Run on both tab selection and focus change, as neither covers the other.
Check all frames, as `after-focus-change-function' may run while the
focused frame is not the selected one."
  (let (changed)
    (tab-bar-status--map-tabs
     (lambda (frame tab)
       (when-let* ((marks (tab-bar-status-marks tab))
                   ((tab-bar-status--in-view-p tab frame)))
         (dolist (type marks)
           (unless (plist-get (tab-bar-status--type-properties type)
                              :permanent)
             (when (tab-bar-status--set-mark tab type nil)
               (setq changed t)))))))
    (when changed
      (tab-bar-status--marks-changed))))

;;;###autoload
(defun tab-bar-status-marked-tabs (&optional frame)
  "Return FRAME's tabs that have a mark."
  (seq-filter #'tab-bar-status-marks (funcall tab-bar-tabs-function frame)))

;;;###autoload
(defun tab-bar-status-marked-group-tabs (group)
  "Return the marked tabs of GROUP, or nil if `tab-bar-status-mode' is off."
  (and tab-bar-status-mode
       group
       ;; PERF: Filter marked tabs only, as each group lookup is a funcall.
       (seq-filter (lambda (tab)
                     (equal (funcall tab-bar-tab-group-function tab) group))
                   (tab-bar-status-marked-tabs))))

(defun tab-bar-status--sort (types)
  "Return TYPES in `tab-bar-status-mark-type-alist' order, unknown types last."
  (let ((order (mapcar #'car tab-bar-status-mark-type-alist)))
    (sort types :key (lambda (type)
                       (or (seq-position order type) (length order))))))

(defun tab-bar-status--superscript (n)
  "Return natural number N as a superscript digit, or \"⁹⁺\" if above 9."
  (if (> n 9)
      "⁹⁺"
    (string (aref "⁰¹²³⁴⁵⁶⁷⁸⁹" n))))

(defun tab-bar-status--format-type (type &optional count)
  "Return TYPE's mark symbol, with COUNT as a superscript if non-nil."
  (let ((spec (tab-bar-status--type-properties type)))
    (apply #'propertize
           (concat (plist-get spec :symbol)
                   (and count (tab-bar-status--superscript count)))
           'face (plist-get spec :face)
           (and count
                (list 'help-echo
                      (format "%s: %d"
                              (string-trim-left (symbol-name type) ":")
                              count))))))

;;;###autoload
(defun tab-bar-status-summary (tabs &optional face)
  "Return the mark types of TABS with their counts, like Flymake's counters.
Return \"\" if `tab-bar-status-mode' is off.  If FACE is non-nil, append
it to the faces of the whole string."
  (if (not tab-bar-status-mode)
      ""
    (let (counts)
      (dolist (tab tabs)
        (dolist (type (tab-bar-status-marks tab))
          (setf (alist-get type counts) (1+ (alist-get type counts 0)))))
      (let ((summary (mapconcat (lambda (type)
                                  (tab-bar-status--format-type
                                   type (alist-get type counts)))
                                (tab-bar-status--sort (mapcar #'car counts))
                                " ")))
        (when face
          (add-face-text-property 0 (length summary) face t summary))
        summary))))

(defun tab-bar-status--marked-tab-names ()
  "Return the names of marked non-current tabs, most recent first.
Do not pass nil to `tab-bar--tabs-recent': it means all tabs."
  (when-let* ((marked (tab-bar-status-marked-tabs)))
    (mapcar (lambda (tab) (alist-get 'name tab))
            (tab-bar--tabs-recent marked))))

;;;###autoload
(defun tab-bar-status-switch-to-marked-tab (name)
  "Switch to the marked tab NAME with `tab-bar-switch-to-tab'.
The default values are the marked tabs, most recent first, available via
\\<minibuffer-local-map>\\[next-history-element].  Selecting the tab
removes its non-permanent marks."
  (interactive
   (let ((names (tab-bar-status--marked-tab-names)))
     (unless names (user-error "No marked tabs"))
     (list (completing-read
            (format-prompt "Switch to marked tab" (car names))
            ;; Category `tab' gets marginalia's tab annotator.
            (completion-table-with-metadata names '((category . tab)))
            nil t nil nil names))))
  (tab-bar-switch-to-tab name))

;;;###autoload
(defun tab-bar-status-tab-name-format (name tab _i)
  "Append the symbols of TAB's marks to NAME.
Return NAME unchanged if `tab-bar-status-mode' is off.  In
`tab-bar-tab-name-format-functions', put this after
`tab-bar-tab-name-format-truncated', so truncation keeps the symbols,
and before the hint and face formatters, so the hint precedes the name
and the tab face covers the symbols."
  (if-let* ((types (and tab-bar-status-mode (tab-bar-status-marks tab))))
      (concat name " "
              (mapconcat #'tab-bar-status--format-type
                         (tab-bar-status--sort types) " "))
    name))

;;;###autoload
(defun tab-bar-status-tab-group-format (name tab _i &optional current-p)
  "Append the mark counts of TAB's group to group label NAME if folded.
The group is open if CURRENT-P or `tab-bar-show-inactive-group-tabs' is
non-nil; an open group shows marks on its tabs.  For group label
formatters, e.g. `tab-bar-theme-tab-group-format-functions'."
  (if-let* (((not (or current-p tab-bar-show-inactive-group-tabs)))
            (tabs (tab-bar-status-marked-group-tabs
                   (funcall tab-bar-tab-group-function tab))))
      (concat name " " (tab-bar-status-summary tabs))
    name))


;;;; Current status

(defvar tab-bar-status--entries nil
  "Alist of (SOURCE STATE TEXT . PREDICATE).")

(defvar tab-bar-status--spinner-index 0
  "Count of spinner frames shown.")

(defvar tab-bar-status--spinner-timer nil
  "Spinner timer, or nil.")

(defun tab-bar-status--spinner-frame ()
  "Return the current spinner frame."
  (if tab-bar-status-spinner-frames
      (nth (% tab-bar-status--spinner-index
              (length tab-bar-status-spinner-frames))
           tab-bar-status-spinner-frames)
    ""))

(defun tab-bar-status--line (entry)
  "Return (STATE . TEXT) of ENTRY if it has a status line, else nil."
  (pcase-let ((`(,_source ,state ,text . ,_predicate) entry))
    (and state
         (not (memq state tab-bar-status-hidden-states))
         (cons state text))))

;;;###autoload
(defun tab-bar-status-tab-lines (tab _i)
  "Return the status lines of TAB, as \"STATE: TEXT\".
A spinner replaces a missing TEXT.  For
`tab-bar-tree-tab-status-function'."
  (seq-keep (lambda (entry)
              (when-let* ((line (tab-bar-status--line entry))
                          ((funcall (cdddr entry) tab)))
                (format "%s: %s" (car line)
                        (or (cdr line) (tab-bar-status--spinner-frame)))))
            tab-bar-status--entries))

(defun tab-bar-status--textless-p (entry)
  "Return non-nil if ENTRY has a status line without text."
  (when-let* ((line (tab-bar-status--line entry)))
    (null (cdr line))))

(defun tab-bar-status--spinner-shown-p ()
  "Return non-nil if a line without text belongs to a tab of any frame."
  (seq-some (lambda (entry)
              (and (tab-bar-status--textless-p entry)
                   (catch 'found
                     (tab-bar-status--map-tabs
                      (lambda (_frame tab)
                        (when (funcall (cdddr entry) tab)
                          (throw 'found t))))
                     nil)))
            tab-bar-status--entries))

(defun tab-bar-status--stop-spinner ()
  "Cancel the spinner timer, if any."
  (when tab-bar-status--spinner-timer
    (cancel-timer tab-bar-status--spinner-timer)
    (setq tab-bar-status--spinner-timer nil)))

(defun tab-bar-status--tick ()
  "Advance the spinner, or stop it if no tab shows one.
The next status change starts it again."
  (if (not (tab-bar-status--spinner-shown-p))
      (tab-bar-status--stop-spinner)
    (setq tab-bar-status--spinner-index (1+ tab-bar-status--spinner-index))
    (run-hooks 'tab-bar-status-change-hook)))

(defun tab-bar-status--lines-changed ()
  "Run `tab-bar-status-change-hook'; spin while a line has no text."
  (if (not (seq-some #'tab-bar-status--textless-p tab-bar-status--entries))
      (tab-bar-status--stop-spinner)
    (unless tab-bar-status--spinner-timer
      (setq tab-bar-status--spinner-timer
            (run-with-timer tab-bar-status-spinner-interval
                            tab-bar-status-spinner-interval
                            #'tab-bar-status--tick))))
  (run-hooks 'tab-bar-status-change-hook))

(defun tab-bar-status--forget ()
  "Remove the status of the killed buffer."
  (tab-bar-status-remove (current-buffer)))

;;;###autoload
(defun tab-bar-status-set (source predicate state &optional text)
  "Set the status of SOURCE to STATE with TEXT on the tabs PREDICATE matches.
SOURCE is any key, e.g. a terminal buffer; a buffer's status is removed
when the buffer is killed.  PREDICATE is called with a tab.  STATE is a
symbol; TEXT is a string, a symbol, or nil for a spinner.  Entering a
new state marks the tabs, see `tab-bar-status-mark-state'.  Do nothing
if SOURCE is a killed buffer."
  (unless (functionp predicate)
    (signal 'wrong-type-argument (list 'functionp predicate)))
  (unless (and (bufferp source) (not (buffer-live-p source)))
    (let* ((entry (assoc source tab-bar-status--entries))
           (old (tab-bar-status--line entry))
           (new (cons source (cons state (cons text predicate)))))
      (unless (eq state (cadr entry))
        (tab-bar-status-mark-state predicate state))
      (if entry
          (setcdr entry (cdr new))
        (push new tab-bar-status--entries)
        (when (bufferp source)
          (with-current-buffer source
            (add-hook 'kill-buffer-hook #'tab-bar-status--forget nil t))))
      (unless (equal old (tab-bar-status--line new))
        (tab-bar-status--lines-changed)))))

;;;###autoload
(defun tab-bar-status-remove (source)
  "Remove the status of SOURCE."
  (when-let* ((entry (assoc source tab-bar-status--entries)))
    (setq tab-bar-status--entries (delq entry tab-bar-status--entries))
    (when (tab-bar-status--line entry)
      (tab-bar-status--lines-changed))))


;;;###autoload
(define-minor-mode tab-bar-status-mode
  "Toggle marks on tab-bar tabs.
When enabled, set marks on state changes, remove non-permanent marks
from tabs that come into view and let the format functions display
marks.  Status lines do not depend on the mode."
  :group 'tab-bar-status
  :global t
  (if tab-bar-status-mode
      (progn
        (add-hook 'tab-bar-tab-post-select-functions
                  #'tab-bar-status--clear-on-view)
        (add-function :after after-focus-change-function
                      #'tab-bar-status--clear-on-view)
        ;; Marks survive while the mode is off; clear tabs now in view.
        (tab-bar-status--clear-on-view))
    (remove-hook 'tab-bar-tab-post-select-functions
                 #'tab-bar-status--clear-on-view)
    (remove-function after-focus-change-function
                     #'tab-bar-status--clear-on-view))
  (force-mode-line-update t))

(provide 'tab-bar-status)
;;; tab-bar-status.el ends here

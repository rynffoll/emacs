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
;;   terminal: a state, e.g. `working', `blocked' or `done', a line of
;;   text and a progress.  States follow the terminal program status
;;   protocol, OSC 7501, see URL
;;   `https://www.superlogical.com/rex/docs/build/program-status'; more
;;   states can be added to `tab-bar-status-state-alist'.  A source sets
;;   its status with `tab-bar-status-set' and a predicate that finds its
;;   tabs.
;;   `tab-bar-status-tab-lines' returns the status lines of a tab, a
;;   value for `tab-bar-tree-tab-status-function'; a missing text is
;;   shown as the progress, or a spinner if there is no progress.  The
;;   status lives as long as its source.
;;
;; - Indicators, colored symbols after the tab name that keep a status
;;   until the tab is viewed.  Entering a state with an `:indicator' in
;;   `tab-bar-status-state-alist' sets indicators on the tabs of the
;;   source; `tab-bar-status-set-indicator-if' sets indicators on tabs
;;   directly, and `tab-bar-status-toggle-indicator' sets one by hand,
;;   e.g. a flag.  Indicators are stored in a tab parameter and die with
;;   the tab.  A tab is in view when it is the selected tab of a focused
;;   frame.  Non-permanent indicators are removed when their tab comes
;;   into view and are never set on a tab in view.  To display
;;   indicators, add `tab-bar-status-tab-name-format' to
;;   `tab-bar-tab-name-format-functions' after
;;   `tab-bar-tab-name-format-truncated'.
;;
;; `tab-bar-status-change-hook' runs when either changes.
;;
;; `tab-bar-status-system-taskbar' shows indicators and progress in the
;; system taskbar.

;;; Code:

(require 'tab-bar)
(require 'seq)


(defgroup tab-bar-status nil
  "Status of tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-status-")

(defcustom tab-bar-status-indicator-alist
  '((:warning :symbol "●" :face warning)
    (:error   :symbol "●" :face error)
    (:success :symbol "●" :face success)
    (:flag    :symbol "⚑" :face shadow :permanent t))
  "Alist of indicator types (TYPE . PROPERTIES), in display order.
PROPERTIES is a plist:
  :symbol      string to display
  :face        face of :symbol
  :permanent   non-nil to keep the indicator when its tab comes into view
A type not listed here is not displayed."
  :type '(alist :key-type (symbol :tag "Type")
                :value-type (plist :options ((:symbol string)
                                             (:face face)
                                             (:permanent boolean))))
  :group 'tab-bar-status)

(defcustom tab-bar-status-state-alist
  '((idle)
    (working :line t)
    (done    :indicator :success)
    (blocked :indicator :warning :line t)
    (error   :indicator :error   :line t))
  "Alist of states (STATE . PROPERTIES).
States are those of the terminal program status protocol (OSC 7501).
PROPERTIES is a plist:
  :indicator   the indicator type a source entering STATE sets on its
               tabs
  :line        non-nil to show a status line
A state not listed here shows nothing."
  :type '(alist :key-type (symbol :tag "State")
                :value-type (plist :options ((:indicator symbol)
                                             (:line boolean))))
  :group 'tab-bar-status)

(defcustom tab-bar-status-change-hook nil
  "Hook run when indicators or statuses change, including spinner frames."
  :type 'hook
  :group 'tab-bar-status)

(defcustom tab-bar-status-spinner-frames '("․" "‥" "…")
  "Spinner frames for a status without text or progress."
  :type '(repeat string)
  :group 'tab-bar-status)

(defcustom tab-bar-status-spinner-interval 0.5
  "Spinner frame interval, in seconds.
Applied on spinner start."
  :type 'number
  :group 'tab-bar-status)


(defvar tab-bar-status-mode)

(defun tab-bar-status--indicators-changed ()
  "Redisplay the tab bars and run `tab-bar-status-change-hook'."
  (force-mode-line-update t)
  (run-hooks 'tab-bar-status-change-hook))

(defun tab-bar-status--type-properties (type)
  "Return the `tab-bar-status-indicator-alist' plist of TYPE, or nil."
  (alist-get type tab-bar-status-indicator-alist))

(defun tab-bar-status--set-indicator (tab type on)
  "Set TYPE's indicator on TAB if ON is non-nil, else remove it.
If TYPE is t and ON is nil, remove all indicators.  Return non-nil if TAB
changed, so callers redisplay only when needed."
  (let* ((types (tab-bar-status-indicators tab))
         (new (cond (on (if (memq type types)
                            types
                          (append types (list type))))
                    ((eq type t) nil)
                    (t (remq type types)))))
    (unless (equal new types)
      ;; Use the cdr so a new entry goes after the tab's type symbol.
      ;; No indicators: drop the entry, so desktop saves none.
      (setf (alist-get 'tab-bar-status-indicators (cdr tab) nil 'remove) new)
      t)))

(defun tab-bar-status--in-view-p (tab frame)
  "Return non-nil if TAB is FRAME's selected tab and FRAME has focus.
A focus state of `unknown' counts as focused, so a terminal without
focus reports still clears the indicators of its selected tab."
  (and (eq (car tab) 'current-tab)
       (frame-focus-state frame)))

(defun tab-bar-status--map-tabs (fn)
  "Call FN with FRAME and TAB for every tab of every frame.
Skip child frames, which have no tab bar."
  (dolist (frame (get-mru-frames t t))
    (dolist (tab (frame-parameter frame 'tabs))
      (funcall fn frame tab))))

(defun tab-bar-status--set-indicator-if (type on test)
  "Set TYPE's indicator to ON on every tab for which TEST returns non-nil.
TEST is called with FRAME and TAB.  Return the number of changed tabs,
or nil if none changed."
  (let ((count 0))
    (tab-bar-status--map-tabs
     (lambda (frame tab)
       (when (and (funcall test frame tab)
                  (tab-bar-status--set-indicator tab type on))
         (setq count (1+ count)))))
    (when (> count 0)
      (tab-bar-status--indicators-changed)
      count)))

(defun tab-bar-status--annotate (candidate)
  "Return the annotation of indicator CANDIDATE: its symbol and permanence."
  (let ((type (intern candidate))
        ;; Align past the longest name, as marginalia does.
        (width (+ 4 (seq-max
                     (mapcar (lambda (entry) (length (symbol-name (car entry))))
                             tab-bar-status-indicator-alist)))))
    (concat (make-string (max 1 (- width (length candidate))) ?\s)
            (tab-bar-status--format-type type)
            (and (plist-get (tab-bar-status--type-properties type) :permanent)
                 (propertize "   permanent" 'face 'completions-annotations)))))

(defun tab-bar-status--clear-on-view (&rest _)
  "Remove non-permanent indicators from every tab in view.
Run on both tab selection and focus change, as neither covers the other.
Check all frames, as `after-focus-change-function' may run while the
focused frame is not the selected one."
  (let (changed)
    (tab-bar-status--map-tabs
     (lambda (frame tab)
       (when-let* ((types (tab-bar-status-indicators tab))
                   ((tab-bar-status--in-view-p tab frame)))
         (dolist (type types)
           (unless (plist-get (tab-bar-status--type-properties type)
                              :permanent)
             (when (tab-bar-status--set-indicator tab type nil)
               (setq changed t)))))))
    (when changed
      (tab-bar-status--indicators-changed))))

(defun tab-bar-status--sort (types)
  "Return TYPES in `tab-bar-status-indicator-alist' order.
Drop types not listed there."
  (let ((order (mapcar #'car tab-bar-status-indicator-alist)))
    (sort (seq-filter (lambda (type) (memq type order)) types)
          :key (lambda (type) (seq-position order type)))))

(defun tab-bar-status--superscript (n)
  "Return natural number N as a superscript digit, or \"⁹⁺\" if above 9."
  (if (> n 9)
      "⁹⁺"
    (string (aref "⁰¹²³⁴⁵⁶⁷⁸⁹" n))))

(defun tab-bar-status--format-type (type &optional count strings)
  "Return TYPE's indicator string, with COUNT as a superscript if non-nil.
STRINGS is an alist of indicator types and strings that replace their
`:symbol'."
  (let ((spec (tab-bar-status--type-properties type)))
    (apply #'propertize
           (concat (or (alist-get type strings)
                       (plist-get spec :symbol))
                   (and count (tab-bar-status--superscript count)))
           'face (plist-get spec :face)
           (and count
                (list 'help-echo
                      (format "%s: %d"
                              (string-trim-left (symbol-name type) ":")
                              count))))))

(defun tab-bar-status--indicated-tab-names ()
  "Return the names of non-current tabs with indicators, most recent first.
Do not pass nil to `tab-bar--tabs-recent': it means all tabs."
  (when-let* ((tabs (tab-bar-status-indicated-tabs)))
    (mapcar (lambda (tab) (alist-get 'name tab))
            (tab-bar--tabs-recent tabs))))

(defvar tab-bar-status--entries nil
  "Alist of (SOURCE STATE TEXT PROGRESS . PREDICATE).")

(defun tab-bar-status--predicate (entry)
  "Return the tab predicate of ENTRY."
  (nthcdr 4 entry))

(defun tab-bar-status--progress (entry)
  "Return the progress of ENTRY."
  (nth 3 entry))

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
  (pcase-let ((`(,_source ,state ,text . ,_rest) entry))
    (and state
         (plist-get (alist-get state tab-bar-status-state-alist) :line)
         (cons state text))))

(defun tab-bar-status--textless-p (entry)
  "Return non-nil if ENTRY has a status line without text or progress."
  (when-let* ((line (tab-bar-status--line entry)))
    (not (or (cdr line) (tab-bar-status--progress entry)))))

(defun tab-bar-status--spinner-shown-p ()
  "Return non-nil if any tab of any frame shows a spinner."
  (seq-some (lambda (entry)
              (and (tab-bar-status--textless-p entry)
                   (catch 'found
                     (tab-bar-status--map-tabs
                      (lambda (_frame tab)
                        (when (funcall (tab-bar-status--predicate entry) tab)
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


(defvar tab-bar-status-indicator-set-functions nil
  "Abnormal hook run after an indicator is set on tabs.
Each function is called with the indicator TYPE.  Run by
`tab-bar-status-set-indicator-if' when it changes any tab.")

;;;###autoload
(defun tab-bar-status-indicators (tab &optional type)
  "Return the list of TAB's indicator types.
If TYPE is non-nil, return non-nil if TAB has an indicator of TYPE."
  (let ((types (alist-get 'tab-bar-status-indicators tab)))
    (if type (memq type types) types)))

;;;###autoload
(defun tab-bar-status-toggle-indicator (type)
  "Toggle TYPE's indicator on the current tab.
Interactively, prompt for TYPE, one of `tab-bar-status-indicator-alist'."
  (interactive
   (let ((set (tab-bar-status-indicators
               (assq 'current-tab (funcall tab-bar-tabs-function)))))
     (list (intern (completing-read
                    "Toggle tab indicator: "
                    ;; Own category, or completion UIs annotate the
                    ;; candidates as Lisp symbols.
                    (completion-table-with-metadata
                     ;; Bold if set on the tab, else shadow.
                     (mapcar (lambda (entry)
                               (propertize (symbol-name (car entry))
                                           'face (if (memq (car entry) set)
                                                     'bold
                                                   'shadow)))
                             tab-bar-status-indicator-alist)
                     '((category . tab-bar-status-indicator-type)
                       (annotation-function . tab-bar-status--annotate)
                       (display-sort-function . identity)))
                    nil t)))))
  (let ((tab (assq 'current-tab (funcall tab-bar-tabs-function))))
    (when (tab-bar-status--set-indicator
           tab type (not (tab-bar-status-indicators tab type)))
      (tab-bar-status--indicators-changed))))

;;;###autoload
(defun tab-bar-status-set-indicator-if (predicate type)
  "Set TYPE's indicator on every tab of every frame that satisfies PREDICATE.
PREDICATE is called with the tab, valid only during the call.  Skip tabs
in view unless TYPE is permanent.  Return the number of changed tabs, or
nil if none changed."
  (let ((permanent (plist-get (tab-bar-status--type-properties type)
                              :permanent)))
    (when-let* ((count (tab-bar-status--set-indicator-if
                        type t (lambda (frame tab)
                                 (and (funcall predicate tab)
                                      (or permanent
                                          (not (tab-bar-status--in-view-p
                                                tab frame))))))))
      (run-hook-with-args 'tab-bar-status-indicator-set-functions type)
      count)))

;;;###autoload
(defun tab-bar-status-remove-indicator-if (predicate type)
  "Remove TYPE's indicator from every tab of every frame.
Only tabs that PREDICATE matches are changed.  TYPE t means all
indicators.  Return the number of changed tabs, or nil if none changed."
  (tab-bar-status--set-indicator-if
   type nil (lambda (_frame tab) (funcall predicate tab))))

;;;###autoload
(defun tab-bar-status-set-state-indicator (predicate state)
  "Set indicators on the tabs PREDICATE matches, with the type of STATE.
The type is the `:indicator' of STATE in `tab-bar-status-state-alist'.
Do nothing unless `tab-bar-status-mode' is on."
  (when-let* ((tab-bar-status-mode)
              (type (plist-get (alist-get state tab-bar-status-state-alist)
                               :indicator)))
    (tab-bar-status-set-indicator-if predicate type)))

;;;###autoload
(defun tab-bar-status-indicated-tabs (&optional frame)
  "Return FRAME's tabs that have an indicator."
  (seq-filter #'tab-bar-status-indicators
              (funcall tab-bar-tabs-function frame)))

;;;###autoload
(defun tab-bar-status-indicated-group-tabs (group)
  "Return the indicated tabs of GROUP, or nil if `tab-bar-status-mode' is off."
  (and tab-bar-status-mode
       group
       ;; PERF: Filter indicated tabs only, as each group lookup is a funcall.
       (seq-filter (lambda (tab)
                     (equal (funcall tab-bar-tab-group-function tab) group))
                   (tab-bar-status-indicated-tabs))))

;;;###autoload
(defun tab-bar-status-summary (tabs &optional face strings)
  "Return the indicator types of TABS with counts, like Flymake's counters.
Return \"\" if `tab-bar-status-mode' is off.  If FACE is non-nil, append
it to the faces of the whole string.  STRINGS is an alist of indicator
types and strings to show instead of their `:symbol'."
  (if (not tab-bar-status-mode)
      ""
    (let (counts)
      (dolist (tab tabs)
        (dolist (type (tab-bar-status-indicators tab))
          (setf (alist-get type counts) (1+ (alist-get type counts 0)))))
      (let ((summary (mapconcat (lambda (type)
                                  (tab-bar-status--format-type
                                   type (alist-get type counts) strings))
                                (tab-bar-status--sort (mapcar #'car counts))
                                " ")))
        (when face
          (add-face-text-property 0 (length summary) face t summary))
        summary))))

;;;###autoload
(defun tab-bar-status-switch-to-indicated-tab (name)
  "Switch to the indicated tab NAME with `tab-bar-switch-to-tab'.
The default values are the indicated tabs, most recent first, available via
\\<minibuffer-local-map>\\[next-history-element].  Selecting the tab
removes its non-permanent indicators."
  (interactive
   (let ((names (tab-bar-status--indicated-tab-names)))
     (unless names (user-error "No tabs with indicators"))
     (list (completing-read
            (format-prompt "Switch to tab with indicators" (car names))
            ;; Category `tab' gets marginalia's tab annotator.
            (completion-table-with-metadata names '((category . tab)))
            nil t nil nil names))))
  (tab-bar-switch-to-tab name))

;;;###autoload
(defun tab-bar-status-tab-name-format (name tab _i)
  "Append the symbols of TAB's indicators to NAME.
Return NAME unchanged if `tab-bar-status-mode' is off.  In
`tab-bar-tab-name-format-functions', put this after
`tab-bar-tab-name-format-truncated', so truncation keeps the symbols,
and before the hint and face formatters, so the hint precedes the name
and the tab face covers the symbols."
  (if-let* ((types (and tab-bar-status-mode (tab-bar-status-indicators tab))))
      (concat name " "
              (mapconcat #'tab-bar-status--format-type
                         (tab-bar-status--sort types) " "))
    name))

;;;###autoload
(defun tab-bar-status-tab-group-format (name tab _i &optional current-p)
  "Append the indicator counts of TAB's group to group label NAME if folded.
The group is open if CURRENT-P or `tab-bar-show-inactive-group-tabs' is
non-nil; an open group shows indicators on its tabs.  For group label
formatters, e.g. `tab-bar-theme-tab-group-format-functions'."
  (if-let* (((not (or current-p tab-bar-show-inactive-group-tabs)))
            (tabs (tab-bar-status-indicated-group-tabs
                   (funcall tab-bar-tab-group-function tab))))
      (concat name " " (tab-bar-status-summary tabs))
    name))

;;;###autoload
(defun tab-bar-status-tab-lines (tab _i)
  "Return the status lines of TAB, as \"STATE: TEXT\".
If TEXT is nil, show the progress instead, or a spinner if there is no
progress.  For use in `tab-bar-tree-tab-status-function'."
  (seq-keep (lambda (entry)
              (when-let* ((line (tab-bar-status--line entry))
                          ((funcall (tab-bar-status--predicate entry) tab)))
                (format "%s: %s" (car line)
                        (or (cdr line)
                            (when-let* ((pct (tab-bar-status--progress entry)))
                              (format "%d%%" pct))
                            (tab-bar-status--spinner-frame)))))
            tab-bar-status--entries))

;;;###autoload
(defun tab-bar-status-set (source predicate state &optional text progress)
  "Set the status of SOURCE to STATE with TEXT on the tabs PREDICATE matches.
SOURCE is any key, e.g. a terminal buffer; a buffer's status is removed
when the buffer is killed.  PREDICATE is called with a tab.  STATE is a
symbol; TEXT is a string, a symbol, or nil for the progress or a
spinner.  PROGRESS is a percentage, 0 to 100, or nil.  Entering a new
state sets indicators on the tabs, see `tab-bar-status-set-state-indicator'.
Do nothing if SOURCE is a killed buffer."
  (unless (functionp predicate)
    (signal 'wrong-type-argument (list 'functionp predicate)))
  (unless (and (bufferp source) (not (buffer-live-p source)))
    (let* ((entry (assoc source tab-bar-status--entries))
           (old (tab-bar-status--line entry))
           (old-progress (tab-bar-status--progress entry))
           (new (cons source
                      (cons state (cons text (cons progress predicate))))))
      (unless (eq state (cadr entry))
        (tab-bar-status-set-state-indicator predicate state))
      (if entry
          (setcdr entry (cdr new))
        (push new tab-bar-status--entries)
        (when (bufferp source)
          (with-current-buffer source
            (add-hook 'kill-buffer-hook #'tab-bar-status--forget nil t))))
      (unless (and (equal old (tab-bar-status--line new))
                   (eql old-progress progress))
        (tab-bar-status--lines-changed)))))

;;;###autoload
(defun tab-bar-status-remove (source)
  "Remove the status of SOURCE."
  (when-let* ((entry (assoc source tab-bar-status--entries)))
    (setq tab-bar-status--entries (delq entry tab-bar-status--entries))
    (when (or (tab-bar-status--line entry) (tab-bar-status--progress entry))
      (tab-bar-status--lines-changed))))

(defun tab-bar-status-progresses ()
  "Return the progress of every source that reports one, as a list."
  (seq-keep #'tab-bar-status--progress tab-bar-status--entries))

;;;###autoload
(define-minor-mode tab-bar-status-mode
  "Toggle indicators on tab-bar tabs.
When enabled, set indicators on state changes, remove non-permanent
indicators from tabs that come into view and let the format functions
display indicators.  Status lines do not depend on the mode."
  :group 'tab-bar-status
  :global t
  (if tab-bar-status-mode
      (progn
        (add-hook 'tab-bar-tab-post-select-functions
                  #'tab-bar-status--clear-on-view)
        (add-function :after after-focus-change-function
                      #'tab-bar-status--clear-on-view)
        ;; Indicators survive while the mode is off; clear tabs now in view.
        (tab-bar-status--clear-on-view))
    (remove-hook 'tab-bar-tab-post-select-functions
                 #'tab-bar-status--clear-on-view)
    (remove-function after-focus-change-function
                     #'tab-bar-status--clear-on-view))
  (force-mode-line-update t))

(provide 'tab-bar-status)
;;; tab-bar-status.el ends here

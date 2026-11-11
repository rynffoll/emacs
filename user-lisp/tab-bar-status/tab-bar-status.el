;;; tab-bar-status.el --- Status of tab-bar tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.2
;; Package-Requires: ((emacs "31.1") (tab-bar-project "0.1") (blinker "0.1"))
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

;; Show the status of buffers on their tabs.  The package stores no
;; status: backends in `tab-bar-status-functions' report the status of a
;; buffer, e.g. of a program in a terminal.  A backend calls
;; `tab-bar-status-update' when a status changes; the next display asks
;; the backends anew, once for all tabs.  A buffer's status shows on the
;; tabs that `tab-bar-status-tab-predicate' matches.
;;
;; Statuses and their order come from `tab-bar-status-alist', each with
;; an indicator, a symbol with a face:
;;
;; - `tab-bar-status-tab-name-format' appends indicators to a tab name,
;;   for `tab-bar-tab-name-format-functions';
;; - `tab-bar-status-tab-group-format' appends counts to a folded group;
;; - `tab-bar-status-summary' counts tabs per status;
;; - `tab-bar-status-tab-messages' returns the messages of a tab's
;;   buffers, for `tab-bar-tree-tab-status-function';
;; - `tab-bar-status-switch-to-tab' goes to a tab with a status.
;;
;; The only statuses kept here are marks, set on a tab by hand with
;; `tab-bar-status-toggle' and stored in a tab parameter.
;;
;; `tab-bar-status-system-taskbar' shows the summary and progress in the
;; system taskbar.

;;; Code:

(require 'tab-bar)
(require 'seq)
(require 'blinker)
(require 'tab-bar-project)


(defgroup tab-bar-status nil
  "Status of tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-status-")

(defcustom tab-bar-status-functions nil
  "Abnormal hook reporting the status of a buffer.
Each function is called with a buffer and returns nil or a plist:
  :status    a status of `tab-bar-status-alist'
  :message   string, e.g. what a program is doing, or nil
  :progress  percentage, 0 to 100, or nil
Functions run for every buffer after each `tab-bar-status-update',
so return early for buffers they do not handle."
  :type 'hook
  :group 'tab-bar-status)

(defcustom tab-bar-status-tab-predicate #'tab-bar-project-contains-p
  "Predicate returning non-nil if a buffer's status shows on a tab.
Called with the tab and the buffer's `default-directory'."
  :type 'function
  :group 'tab-bar-status)

(defcustom tab-bar-status-alist
  '((working "●" shadow)
    (blocked "●" warning)
    (done    "●" success)
    (error   "●" error)
    (flag    "⚑" shadow))
  "Alist of statuses (STATUS STRING FACE).
Statuses, and the tabs and messages that have them, are listed in the
order of this alist.  STRING and FACE make the indicator of STATUS; a
status without STRING has none.  A status not listed here shows
nothing.  Call `tab-bar-status-update' after a change."
  :type '(repeat (list (symbol :tag "Status")
                       (choice (const :tag "No indicator" nil) string)
                       (choice (const :tag "No face" nil) face)))
  :group 'tab-bar-status)

(defcustom tab-bar-status-blink-statuses '(working)
  "Statuses whose indicator blinks after a tab name and in the summary.
Blinking runs only while a buffer has such a status; see `blinker-blend'
and `blinker-interval'."
  :type '(repeat (symbol :tag "Status"))
  :group 'tab-bar-status)

(defcustom tab-bar-status-blink-hook nil
  "Hook run when blinking indicators are dimmed or restored.
For buffers that show tab names, e.g. a tree of tabs: call
`blinker-update-buffer' on them."
  :type 'hook
  :group 'tab-bar-status)

(defcustom tab-bar-status-change-hook nil
  "Hook run by `tab-bar-status-update'."
  :type 'hook
  :group 'tab-bar-status)


(defun tab-bar-status--of (entry)
  "Return the status of ENTRY, a (BUFFER . REPORT)."
  (plist-get (cdr entry) :status))

(defun tab-bar-status--rank (status)
  "Return the position of STATUS in `tab-bar-status-alist', or nil."
  (seq-position tab-bar-status-alist status
                (lambda (entry status) (eq (car entry) status))))

(defvar tab-bar-status--entries 'unset
  "Value of `tab-bar-status--entries', or `unset'.
Reset by `tab-bar-status-update'.")

(defun tab-bar-status--collect ()
  "Return (BUFFER . REPORT) for every buffer with a status.
REPORT is what `tab-bar-status-functions' returned.  Sort them in the
order of `tab-bar-status-alist'; equal statuses keep `buffer-list'
order.  Drop statuses not in the alist."
  (let (entries)
    (when tab-bar-status-functions
      (dolist (buffer (buffer-list))
        (run-hook-wrapped
         'tab-bar-status-functions
         (lambda (fn)
           (when-let* ((report (funcall fn buffer))
                       ((assq (plist-get report :status)
                              tab-bar-status-alist)))
             (push (cons buffer report) entries))
           nil))))
    (sort (nreverse entries)
          :key (lambda (entry)
                 (tab-bar-status--rank (tab-bar-status--of entry))))))

(defun tab-bar-status--entries ()
  "Return the entries of live buffers, see `tab-bar-status--collect'.
Collect them once per `tab-bar-status-update'."
  (when (eq tab-bar-status--entries 'unset)
    (setq tab-bar-status--entries (tab-bar-status--collect)))
  (seq-filter (lambda (entry) (buffer-live-p (car entry)))
              tab-bar-status--entries))

(defun tab-bar-status--tab-entries (tab entries)
  "Return the elements of ENTRIES that show on TAB."
  (seq-filter (lambda (entry)
                (funcall tab-bar-status-tab-predicate tab
                         (buffer-local-value 'default-directory
                                             (car entry))))
              entries))

(defun tab-bar-status--marks (tab)
  "Return the statuses set on TAB by hand."
  (alist-get 'tab-bar-status-marks tab))

(defun tab-bar-status--tab-statuses (tab tab-entries)
  "Return the statuses of TAB with an indicator.
They are in the order of `tab-bar-status-alist'.  TAB-ENTRIES are
TAB's elements of `tab-bar-status--entries'."
  (let ((statuses (append (tab-bar-status--marks tab)
                          (mapcar #'tab-bar-status--of tab-entries))))
    (seq-keep (pcase-lambda (`(,status ,string))
                (and string (memq status statuses) status))
              tab-bar-status-alist)))

(defun tab-bar-status--superscript (n)
  "Return natural number N as a superscript digit, or \"⁹⁺\" if above 9."
  (if (> n 9)
      "⁹⁺"
    (string (aref "⁰¹²³⁴⁵⁶⁷⁸⁹" n))))

(defun tab-bar-status--indicator (status &optional count strings)
  "Return the indicator of STATUS, with COUNT as a superscript if non-nil.
STRINGS is an alist of statuses and strings that replace their STRING
in `tab-bar-status-alist'."
  (pcase-let ((`(,string ,face) (alist-get status tab-bar-status-alist)))
    (apply #'propertize
           (concat (or (alist-get status strings) string)
                   (and count (tab-bar-status--superscript count)))
           'face face
           (and count
                (list 'help-echo (format "%s: %d" status count))))))

(defun tab-bar-status--blinking-indicator (status &optional count strings)
  "Return the indicator of STATUS, blinking if the status blinks.
COUNT and STRINGS are as in `tab-bar-status--indicator'.  The status
blinks if it is one of `tab-bar-status-blink-statuses'; COUNT does not."
  (let ((indicator (tab-bar-status--indicator status count strings)))
    (if (memq status tab-bar-status-blink-statuses)
        (let ((end (- (length indicator)
                      (if count
                          (length (tab-bar-status--superscript count))
                        0))))
          (concat (blinker-string (substring indicator 0 end))
                  (substring indicator end)))
      indicator)))

(defun tab-bar-status--blink-p ()
  "Return non-nil if a buffer has a status that blinks."
  (and tab-bar-status-blink-statuses
       (seq-some (lambda (entry)
                   (memq (tab-bar-status--of entry)
                         tab-bar-status-blink-statuses))
                 (tab-bar-status--entries))))

(defun tab-bar-status--blink ()
  "Redisplay blinking indicators; return non-nil if a status blinks.
For `blinker-start'."
  (when (tab-bar-status--blink-p)
    ;; PERF: Update all mode lines only for a tab bar shown somewhere.
    (when (seq-some (lambda (frame)
                      (> (or (frame-parameter frame 'tab-bar-lines) 0) 0))
                    (frame-list))
      (force-mode-line-update t))
    (run-hooks 'tab-bar-status-blink-hook)
    t))

(defun tab-bar-status--affixate (candidates)
  "Return status CANDIDATES with their indicator as prefix.
The suffix is \"on\" for a mark of the current tab."
  (let ((marks (tab-bar-status--marks
                (assq 'current-tab (funcall tab-bar-tabs-function))))
        ;; Align the suffix past the longest name.
        (width (+ 2 (seq-max (mapcar #'string-width candidates)))))
    (mapcar (lambda (candidate)
              (let ((status (intern candidate)))
                (list candidate
                      (concat (tab-bar-status--indicator status) " ")
                      (if (memq status marks)
                          (concat (make-string
                                   (- width (string-width candidate)) ?\s)
                                  (propertize "on" 'face
                                              'completions-annotations))
                        ""))))
            candidates)))

(defun tab-bar-status--group-tabs (tab)
  "Return the tabs of TAB's group in the selected frame, or nil."
  (when-let* ((group (funcall tab-bar-tab-group-function tab)))
    (seq-filter (lambda (other)
                  (equal (funcall tab-bar-tab-group-function other) group))
                (funcall tab-bar-tabs-function))))

(defun tab-bar-status--tab-candidates ()
  "Return (TAB . BUFFER) for other tabs with a status.
Sort them by their first status, in the order of `tab-bar-status-alist';
among equal statuses, the most recently selected tab comes first.
BUFFER has the tab's first status, or is nil if that status is a mark."
  (let ((entries (tab-bar-status--entries))
        candidates)
    (dolist (tab (tab-bar--tabs-recent))
      (let* ((tab-entries (tab-bar-status--tab-entries tab entries))
             (status (car (tab-bar-status--tab-statuses tab tab-entries))))
        (when status
          (push (list (tab-bar-status--rank status) tab
                      (car (seq-find (lambda (entry)
                                       (eq (tab-bar-status--of entry) status))
                                     tab-entries)))
                candidates))))
    (mapcar (pcase-lambda (`(,_rank ,tab ,buffer)) (cons tab buffer))
            (sort (nreverse candidates) :key #'car))))

(defun tab-bar-status--show-buffer (buffer)
  "Select BUFFER's window in the selected frame, or pop to BUFFER."
  (if-let* ((window (get-buffer-window buffer)))
      (select-window window)
    (pop-to-buffer buffer)))


;;;###autoload
(defun tab-bar-status-update ()
  "Redisplay the statuses and run `tab-bar-status-change-hook'.
Backends call this when a status changes, or when
`tab-bar-status-functions' or `tab-bar-status-alist' changes."
  (setq tab-bar-status--entries 'unset)
  (when (tab-bar-status--blink-p)
    (blinker-start #'tab-bar-status--blink))
  (force-mode-line-update t)
  (run-hooks 'tab-bar-status-change-hook))

;;;###autoload
(defun tab-bar-status-progresses ()
  "Return the progress of every buffer that reports one."
  (seq-keep (lambda (entry) (plist-get (cdr entry) :progress))
            (tab-bar-status--entries)))

;;;###autoload
(defun tab-bar-status-summary (tabs &optional face strings)
  "Return the statuses of TABS with counts of tabs, like Flymake's counters.
If FACE is non-nil, append it to the faces of the whole string.  STRINGS
is an alist of statuses and strings to show instead of their indicator
strings.  Indicators of `tab-bar-status-blink-statuses' blink, their
counts do not."
  (let ((entries (tab-bar-status--entries))
        counts)
    (dolist (tab tabs)
      (dolist (status (tab-bar-status--tab-statuses
                       tab (tab-bar-status--tab-entries tab entries)))
        (setf (alist-get status counts) (1+ (alist-get status counts 0)))))
    (let ((summary
           (mapconcat
            (lambda (entry)
              (tab-bar-status--blinking-indicator
               (car entry) (alist-get (car entry) counts) strings))
            (seq-filter (lambda (entry) (assq (car entry) counts))
                        tab-bar-status-alist)
            " ")))
      (when face
        (add-face-text-property 0 (length summary) face t summary))
      summary)))

;;;###autoload
(defun tab-bar-status-tab-name-format (name tab _i)
  "Append the indicators of TAB's statuses to NAME.
In `tab-bar-tab-name-format-functions', put this after
`tab-bar-tab-name-format-truncated', so truncation keeps the indicators,
and before the hint and face formatters, so the hint precedes the name
and the tab face covers the indicators.  Indicators of
`tab-bar-status-blink-statuses' blink."
  (if-let* ((statuses (tab-bar-status--tab-statuses
                       tab (tab-bar-status--tab-entries
                            tab (tab-bar-status--entries)))))
      (concat name " "
              (mapconcat
               #'tab-bar-status--blinking-indicator statuses " "))
    name))

;;;###autoload
(defun tab-bar-status-tab-group-format (name tab _i &optional current-p)
  "Append the status counts of TAB's group to group label NAME if folded.
The group is open if CURRENT-P or `tab-bar-show-inactive-group-tabs' is
non-nil; an open group shows indicators on its tabs.  For group label
formatters, e.g. `tab-bar-theme-tab-group-format-functions'."
  (let ((summary (and (not (or current-p tab-bar-show-inactive-group-tabs))
                      (tab-bar-status-summary
                       (tab-bar-status--group-tabs tab)))))
    (if (seq-empty-p summary)
        name
      (concat name " " summary))))

;;;###autoload
(defun tab-bar-status-tab-messages (tab _i)
  "Return the messages of TAB's buffers.
They are in the order of `tab-bar-status-alist'.  For
`tab-bar-tree-tab-status-function'."
  (seq-keep (lambda (entry) (plist-get (cdr entry) :message))
            (tab-bar-status--tab-entries tab (tab-bar-status--entries))))

;;;###autoload
(defun tab-bar-status-toggle (status)
  "Toggle STATUS as a mark on the current tab.
A mark is a status set by hand; it shows and counts as any other.
Interactively, prompt for a status of `tab-bar-status-alist' with an
indicator; marks of the tab are marked \"on\"."
  (interactive
   (list (intern
          (completing-read
           "Toggle tab status: "
           ;; Own category, or completion UIs annotate the candidates as
           ;; Lisp symbols.
           (completion-table-with-metadata
            (seq-keep (pcase-lambda (`(,status ,string))
                        (and string (symbol-name status)))
                      tab-bar-status-alist)
            '((category . tab-bar-status)
              (affixation-function . tab-bar-status--affixate)
              (display-sort-function . identity)))
           nil t))))
  (let* ((tab (assq 'current-tab (funcall tab-bar-tabs-function)))
         (marks (tab-bar-status--marks tab)))
    ;; Use the cdr so a new entry goes after the tab's type symbol.  No
    ;; marks: drop the entry, so desktop saves none.
    (setf (alist-get 'tab-bar-status-marks (cdr tab) nil 'remove)
          (if (memq status marks)
              (remq status marks)
            (append marks (list status))))
    (tab-bar-status-update)))

;;;###autoload
(defun tab-bar-status-tab-indicators (name)
  "Return the indicators of tab NAME in the selected frame.
They are in the order of `tab-bar-status-alist'."
  (when-let* ((tab (seq-find (lambda (tab) (equal (alist-get 'name tab) name))
                             (funcall tab-bar-tabs-function))))
    (mapcar #'tab-bar-status--indicator
            (tab-bar-status--tab-statuses
             tab (tab-bar-status--tab-entries tab (tab-bar-status--entries))))))

(defun tab-bar-status--tab-annotation (name)
  "Return the indicators of tab NAME as an annotation."
  (if-let* ((indicators (tab-bar-status-tab-indicators name)))
      (concat " " (string-join indicators " "))
    ""))

;;;###autoload
(defun tab-bar-status-switch-to-tab (name)
  "Switch to the other tab NAME with a status and show its buffer.
The buffer is the one with the tab's first status, in the order of
`tab-bar-status-alist'; a tab whose first status is a mark is only
selected.  Interactively, offer the tabs with a status, sorted by their
first status, then most recently selected; the first is the default."
  (interactive
   (let ((names (mapcar (lambda (candidate) (alist-get 'name (car candidate)))
                        (tab-bar-status--tab-candidates))))
     (unless names (user-error "No other tab has a status"))
     (list (completing-read
            (format-prompt "Switch to tab with status" (car names))
            (completion-table-with-metadata
             names '((category . tab-bar-status-tab)
                     (annotation-function . tab-bar-status--tab-annotation)
                     (display-sort-function . identity)))
            nil t nil nil names))))
  (pcase-let ((`(,tab . ,buffer)
               (seq-find (lambda (candidate)
                           (equal (alist-get 'name (car candidate)) name))
                         (tab-bar-status--tab-candidates))))
    (unless tab
      (user-error "No tab %s with a status" name))
    (tab-bar-select-tab (1+ (tab-bar--tab-index tab)))
    (when buffer
      (tab-bar-status--show-buffer buffer))))

(provide 'tab-bar-status)
;;; tab-bar-status.el ends here

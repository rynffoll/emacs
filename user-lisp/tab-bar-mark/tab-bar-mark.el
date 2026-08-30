;;; tab-bar-mark.el --- Attention indicator for tab-bar tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
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

;; Marks a tab-bar tab as needing attention with a colored symbol prefixed
;; to its name.  Any notifier (ghostel, compile, a custom hook) calls
;; `tab-bar-mark' with a directory, and
;; `tab-bar-mark-tab-root-function' says which directory a given tab
;; stands for, which is how that directory is resolved to the tabs to
;; mark.
;;
;; The mark lives in a `tab-bar-mark' tab parameter, so it belongs to
;; the tab itself: it rides along through tab switches, and it is gone
;; the moment the tab is closed, with no bookkeeping to leak.
;;
;; A mark clears once its tab comes into view on a focused frame —
;; whether you select the tab, the frame regains input focus, or the
;; mark lands on a tab already in front of you.  All three run the one
;; countdown: `tab-bar-mark-unmark-delay' keeps the symbol on screen for
;; a moment first, so a mark always gets a chance to register before
;; it goes.

;;; Code:

(require 'tab-bar)

(defgroup tab-bar-mark nil
  "Attention indicator for tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-mark-")

(defcustom tab-bar-mark-tab-root-function nil
  "Function that returns the directory a tab-bar tab represents, or nil.
Called with a single TAB argument.  Left unset by default, which
disables the indicator: nothing to mark without a way to resolve a
tab to a directory."
  :type '(choice function (const nil))
  :group 'tab-bar-mark)

(defcustom tab-bar-mark-symbol "●"
  "Symbol shown after a marked tab's name."
  :type 'string
  :group 'tab-bar-mark)

(defcustom tab-bar-mark-face 'warning
  "Face applied to `tab-bar-mark-symbol'.
Defaults to `warning' so the symbol follows the active theme's own
attention color instead of a hardcoded one."
  :type 'face
  :group 'tab-bar-mark)

(defcustom tab-bar-mark-unmark-delay 3
  "Seconds a mark stays up once its tab comes into view.
Gives the symbol a moment to register before it disappears.  Set to nil
to clear immediately instead."
  :type '(choice (const :tag "Immediately" nil) number)
  :group 'tab-bar-mark)

(defun tab-bar-mark--set (tab marked)
  "Set TAB's mark parameter to MARKED.
Return non-nil when that actually changed something, so callers can
skip a redisplay nobody needs."
  (let ((cell (assq 'tab-bar-mark tab)))
    (cond ((null cell)
           (when marked
             (nconc tab (list (cons 'tab-bar-mark marked)))
             t))
          ((not (eq (cdr cell) marked))
           (setcdr cell marked)
           t))))

(defun tab-bar-mark--tab-root (tab)
  "Return the directory TAB represents, via `tab-bar-mark-tab-root-function'."
  (and tab-bar-mark-tab-root-function (funcall tab-bar-mark-tab-root-function tab)))

(defun tab-bar-mark--normalize-dir (dir)
  "Return DIR in the canonical form tab roots are compared in."
  (and dir (file-name-as-directory (expand-file-name dir))))

(defun tab-bar-mark--map-tabs-for-dir (dir fn)
  "Call FN with the frame and tab of every tab representing DIR.
Walks every frame, since the tab wanted may well be on one the user
is not looking at — that is the whole point of the indicator.  DIR is
normalized once here rather than again for every tab visited."
  (when-let* ((dir (tab-bar-mark--normalize-dir dir)))
    (dolist (frame (frame-list))
      (dolist (tab (funcall tab-bar-tabs-function frame))
        (when (equal dir (tab-bar-mark--normalize-dir
                          (tab-bar-mark--tab-root tab)))
          (funcall fn frame tab))))))

(defun tab-bar-mark--apply (dir marked)
  "Set the mark on every tab representing DIR to MARKED.
When MARKED, a tab that is already the selected tab of a focused frame
starts its countdown right away: the user is looking at it, and no
later select or focus event would arrive to clear it."
  (let (changed)
    (tab-bar-mark--map-tabs-for-dir
     dir
     (lambda (frame tab)
       (when (tab-bar-mark--set tab marked)
         (setq changed t))
       (when (and marked (eq (car tab) 'current-tab))
         (tab-bar-mark--schedule-unmark frame))))
    (when changed (force-mode-line-update t))))

;;;###autoload
(defun tab-bar-mark (dir)
  "Mark every tab representing DIR as needing attention."
  (tab-bar-mark--apply dir t))

;;;###autoload
(defun tab-bar-unmark (dir)
  "Clear the attention mark on every tab representing DIR."
  (tab-bar-mark--apply dir nil))

(defun tab-bar-mark--tab-name-format (name tab _i)
  "Suffix NAME with `tab-bar-mark-symbol' when TAB is marked.
Not parenthesized: that notation reads as \"optional part\" and this
is neither.  Runs ahead of `tab-bar-tab-name-format-hints' in the
chain, so the hint number ends up in front of NAME rather than
between it and the symbol."
  (if (alist-get 'tab-bar-mark tab)
      (concat name " " (propertize tab-bar-mark-symbol 'face tab-bar-mark-face))
    name))

(defvar tab-bar-mark--unmark-timers (make-hash-table :test 'eq)
  "Frame to its pending unmark timer.
Keyed by frame so two frames counting down at once cannot clobber
each other's pending clear.")

(defun tab-bar-mark--cancel-unmark-timer (frame)
  "Cancel FRAME's pending unmark timer, if any."
  (when-let* ((timer (gethash frame tab-bar-mark--unmark-timers)))
    (cancel-timer timer)
    (remhash frame tab-bar-mark--unmark-timers)))

(defun tab-bar-mark--cancel-all-unmark-timers ()
  "Cancel every pending unmark timer, across all frames."
  (maphash (lambda (_frame timer) (cancel-timer timer)) tab-bar-mark--unmark-timers)
  (clrhash tab-bar-mark--unmark-timers))

(defun tab-bar-mark--unmark-current-tab (frame)
  "Clear the mark on FRAME's selected tab, once its countdown is up.
Whatever tab is selected by now is the one the user has been looking
at, which is exactly the one that has earned its mark cleared."
  (remhash frame tab-bar-mark--unmark-timers)
  (when (and (frame-live-p frame)
             (tab-bar-mark--set (tab-bar--current-tab-find nil frame) nil))
    (force-mode-line-update t)))

(defun tab-bar-mark--schedule-unmark (frame)
  "Start FRAME's countdown to clear its selected tab's mark.
Only while FRAME holds input focus — an unfocused frame's tab is not
really in view, and its mark should wait for the user to come back.
Restarts a countdown already running, so the symbol always gets its full
`tab-bar-mark-unmark-delay' on screen."
  (tab-bar-mark--cancel-unmark-timer frame)
  (when (frame-focus-state frame)
    (if tab-bar-mark-unmark-delay
        (puthash frame
                 (run-with-timer tab-bar-mark-unmark-delay nil
                                 #'tab-bar-mark--unmark-current-tab frame)
                 tab-bar-mark--unmark-timers)
      (tab-bar-mark--unmark-current-tab frame))))

(defun tab-bar-mark--unmark-on-view (&rest _)
  "Start the countdown for whatever tab is now in view.
Serves both events that can bring a tab into view, hence the ignored
arguments — the two hooks call with different signatures.  Both are
needed: selecting a tab does not fire a focus change, and coming back
from another application does not fire a tab selection, so either one
alone would leave a mark stranded in plain sight."
  (tab-bar-mark--schedule-unmark (selected-frame)))

;;;###autoload
(define-minor-mode tab-bar-mark-mode
  "Show a colored attention symbol on tab-bar tabs marked via `tab-bar-mark'."
  :group 'tab-bar-mark
  :global t
  (if tab-bar-mark-mode
      (progn
        (add-to-list 'tab-bar-tab-name-format-functions #'tab-bar-mark--tab-name-format)
        (add-hook 'tab-bar-tab-post-select-functions #'tab-bar-mark--unmark-on-view)
        (add-function :after after-focus-change-function #'tab-bar-mark--unmark-on-view))
    (setq tab-bar-tab-name-format-functions
          (delq #'tab-bar-mark--tab-name-format tab-bar-tab-name-format-functions))
    (remove-hook 'tab-bar-tab-post-select-functions #'tab-bar-mark--unmark-on-view)
    (remove-function after-focus-change-function #'tab-bar-mark--unmark-on-view)
    (tab-bar-mark--cancel-all-unmark-timers)))

(provide 'tab-bar-mark)
;;; tab-bar-mark.el ends here

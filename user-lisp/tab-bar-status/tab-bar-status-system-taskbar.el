;;; tab-bar-status-system-taskbar.el --- Tab status in the system taskbar -*- lexical-binding: t; -*-

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

;; Show the status of tab-bar tabs in the system taskbar:
;;
;;   (system-taskbar-mode 1)
;;   (tab-bar-status-system-taskbar-mode 1)
;;
;; The taskbar badge shows the summary of all tabs, with the strings of
;; `tab-bar-status-system-taskbar-badge-alist', since the badge has no
;; faces; the progress indicator shows the mean progress of all buffers.
;; A blinking status alternates its string with the one in
;; `tab-bar-status-system-taskbar-blink-alist'.

;;; Code:

(require 'system-taskbar)
(require 'blinker)
(require 'tab-bar-status)


(defcustom tab-bar-status-system-taskbar-badge-alist
  '((working . "⚪")
    (blocked . "🟡")
    (done    . "🟢")
    (error   . "🔴")
    (flag    . "🚩"))
  "Alist of statuses and their strings in the taskbar badge.
The badge has no faces, so a colored string tells the statuses apart.
A status not listed here shows its indicator string from
`tab-bar-status-alist'."
  :type '(alist :key-type (symbol :tag "Status") :value-type string)
  :group 'tab-bar-status)

(defcustom tab-bar-status-system-taskbar-blink-alist
  '((working . "⚫"))
  "Alist of statuses and their strings while `blinker-dimmed-p'.
A status of `tab-bar-status-blink-statuses' not listed here does not
blink in the badge."
  :type '(alist :key-type (symbol :tag "Status") :value-type string)
  :group 'tab-bar-status)


(defun tab-bar-status-system-taskbar--badge ()
  "Display the summary of all tabs in the taskbar badge.
This is for use in `tab-bar-status-blink-hook' too."
  (let ((badge (substring-no-properties
                (tab-bar-status-summary
                 (apply #'append (mapcar (lambda (frame)
                                           (funcall tab-bar-tabs-function frame))
                                         (get-mru-frames t t)))
                 nil
                 (if (blinker-dimmed-p)
                     (append tab-bar-status-system-taskbar-blink-alist
                             tab-bar-status-system-taskbar-badge-alist)
                   tab-bar-status-system-taskbar-badge-alist)))))
    (system-taskbar-badge (and (not (string-empty-p badge)) badge))))

(defun tab-bar-status-system-taskbar--progress ()
  "Display the mean progress of all buffers in the taskbar.
Remove the progress indicator if no buffer reports progress."
  (let ((ps (tab-bar-status-progresses)))
    (system-taskbar-progress
     (and ps (/ (apply #'+ ps) 100.0 (length ps))))))

(defun tab-bar-status-system-taskbar--update ()
  "Update the taskbar badge and progress indicator.
This is for use in `tab-bar-status-change-hook'."
  (tab-bar-status-system-taskbar--badge)
  (tab-bar-status-system-taskbar--progress))


;;;###autoload
(define-minor-mode tab-bar-status-system-taskbar-mode
  "Toggle display of tab statuses and progress in the system taskbar.
The summary of all tabs is displayed in the taskbar badge, with the
strings of `tab-bar-status-system-taskbar-badge-alist'; a blinking
status alternates with `tab-bar-status-system-taskbar-blink-alist'.
The mean progress of all buffers is displayed in the taskbar progress
indicator.

This mode has no effect unless `system-taskbar-mode' is enabled."
  :global t
  :group 'tab-bar-status
  (if tab-bar-status-system-taskbar-mode
      (progn
        (add-hook 'tab-bar-status-change-hook
                  #'tab-bar-status-system-taskbar--update)
        (add-hook 'tab-bar-status-blink-hook
                  #'tab-bar-status-system-taskbar--badge)
        (tab-bar-status-system-taskbar--update))
    (remove-hook 'tab-bar-status-change-hook
                 #'tab-bar-status-system-taskbar--update)
    (remove-hook 'tab-bar-status-blink-hook
                 #'tab-bar-status-system-taskbar--badge)
    (system-taskbar-badge nil)
    (system-taskbar-progress nil)))

(provide 'tab-bar-status-system-taskbar)
;;; tab-bar-status-system-taskbar.el ends here

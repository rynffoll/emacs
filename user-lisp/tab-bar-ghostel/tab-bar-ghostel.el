;;; tab-bar-ghostel.el --- Terminal status on tab-bar tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1") (ghostel "0.59") (tab-bar-project "0.1") (tab-bar-status "0.1"))
;; Keywords: convenience, processes
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

;; Show the status a program reports in a ghostel terminal, e.g. an
;; agent working, blocked on a prompt or done, on the terminal's tab.
;; `tab-bar-ghostel-tab-predicate' finds the tab: the terminal's project
;; tab by default.
;;
;; `tab-bar-ghostel-mode' passes the root status record of a terminal to
;; `tab-bar-status-set'.  Other records only mark the tab, see
;; `tab-bar-status-mark-state'.  A desktop notification, OSC 9 or
;; OSC 777, marks the tab of a terminal that reports no program status.
;; Marks need `tab-bar-status-mode'.

;;; Code:

(require 'ghostel-status)
(require 'tab-bar-project)
(require 'tab-bar-status)

(defgroup tab-bar-ghostel nil
  "Terminal status on tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-ghostel-")

(defcustom tab-bar-ghostel-tab-predicate #'tab-bar-project-contains-p
  "Predicate returning non-nil if a tab is a terminal's.
Called with two arguments, TAB and the terminal's `default-directory'."
  :type 'function
  :group 'tab-bar-ghostel)

(defun tab-bar-ghostel--predicate (buffer)
  "Return a predicate matching the tabs of terminal BUFFER.
It reads BUFFER's `default-directory' on each call, so it follows `cd'."
  (lambda (tab)
    (and (buffer-live-p buffer)
         (funcall tab-bar-ghostel-tab-predicate tab
                  (buffer-local-value 'default-directory buffer)))))

(defun tab-bar-ghostel--status-change (id old new)
  "Show the change of status record ID from OLD to NEW.
For `ghostel-program-status-functions'.  The root record, ID \"\", sets
the terminal's status; other records only mark its tab."
  (let ((predicate (tab-bar-ghostel--predicate (current-buffer)))
        (state (plist-get new :state)))
    (cond ((not (string-empty-p id))
           (unless (eq state (plist-get old :state))
             (tab-bar-status-mark-state predicate state)))
          (state
           (tab-bar-status-set (current-buffer) predicate state
                               (or (plist-get new :msg) (plist-get new :kind))))
          (t (tab-bar-status-remove (current-buffer))))))

(defun tab-bar-ghostel--notify (_title _body)
  "Mark the current terminal's tab on a desktop notification.
For `ghostel-notification-functions'.  Skip a terminal that reports
program status: its records mark the tab by state."
  (when (and tab-bar-status-mode (not ghostel-program-status))
    (tab-bar-status-mark-if (tab-bar-ghostel--predicate (current-buffer)))))

;;;###autoload
(define-minor-mode tab-bar-ghostel-mode
  "Toggle showing the status of ghostel terminal programs on tab-bar tabs."
  :global t
  :group 'tab-bar-ghostel
  (if tab-bar-ghostel-mode
      (progn
        (add-hook 'ghostel-program-status-functions
                  #'tab-bar-ghostel--status-change)
        (add-hook 'ghostel-notification-functions #'tab-bar-ghostel--notify))
    (remove-hook 'ghostel-program-status-functions
                 #'tab-bar-ghostel--status-change)
    (remove-hook 'ghostel-notification-functions #'tab-bar-ghostel--notify)
    (mapc #'tab-bar-status-remove
          (match-buffers '(derived-mode . ghostel-mode)))))

(provide 'tab-bar-ghostel)
;;; tab-bar-ghostel.el ends here

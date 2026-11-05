;;; tab-bar-ghostel.el --- Terminal status on tab-bar tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1") (ghostel "0.60") (tab-bar-status "0.2"))
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

;; A backend of `tab-bar-status' for ghostel terminals: the status a
;; program reports, e.g. an agent working, blocked on a prompt or done,
;; shows on the terminal's tab.  The status is that of the terminal's
;; most urgent record, see `tab-bar-ghostel-statuses'; its message is
;; the record's text with the kind or progress.  Ghostel drops records
;; by itself, see `ghostel-status-auto-acknowledge'.

;;; Code:

(require 'ghostel-status)
(require 'map)
(require 'tab-bar-status)


(defgroup tab-bar-ghostel nil
  "Terminal status on tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-ghostel-")

(defcustom tab-bar-ghostel-statuses '(blocked error done working)
  "Statuses of terminal records shown on tabs, most urgent first."
  :type '(repeat symbol)
  :group 'tab-bar-ghostel)


(defun tab-bar-ghostel--message (record)
  "Return the message of RECORD, a record plist, or nil.
The text is `:msg', else `:title'; it follows the kind of
a `blocked' record, \"permission:\", or the progress, \"42%\".  The
status itself shows as the indicator."
  (pcase-let* (((map :state :kind :progress :msg :title) record)
               (prefix (pcase state
                         ('blocked (and kind (format "%s:" kind)))
                         ('working (and progress (format "%d%%" progress)))))
               (text (or msg title)))
    (and (or prefix text)
         (string-join (delq nil (list prefix text)) " "))))

;;;###autoload
(defun tab-bar-ghostel-status (buffer)
  "Return the status of terminal BUFFER for `tab-bar-status-functions'.
The status is that of the most urgent record.  A `done' record has no
message: the indicator says it all."
  (when-let* ((records (buffer-local-value 'ghostel-program-status buffer))
              (entry (seq-some
                      (lambda (status)
                        (seq-find (lambda (entry)
                                    (eq (plist-get (cdr entry) :state) status))
                                  records))
                      tab-bar-ghostel-statuses)))
    (pcase-let (((and record (map :state :progress)) (cdr entry)))
      (list :status state
            :message (and (not (eq state 'done))
                          (tab-bar-ghostel--message record))
            :progress progress))))

(defun tab-bar-ghostel--status-change (&rest _)
  "Update `tab-bar-status', for `ghostel-program-status-functions'."
  (tab-bar-status-update))


;;;###autoload
(define-minor-mode tab-bar-ghostel-mode
  "Toggle showing the status of ghostel terminal programs on tab-bar tabs."
  :global t
  :group 'tab-bar-ghostel
  (if tab-bar-ghostel-mode
      (progn
        (add-hook 'tab-bar-status-functions #'tab-bar-ghostel-status)
        (add-hook 'ghostel-program-status-functions
                  #'tab-bar-ghostel--status-change))
    (remove-hook 'tab-bar-status-functions #'tab-bar-ghostel-status)
    (remove-hook 'ghostel-program-status-functions
                 #'tab-bar-ghostel--status-change))
  (tab-bar-status-update))

(provide 'tab-bar-ghostel)
;;; tab-bar-ghostel.el ends here

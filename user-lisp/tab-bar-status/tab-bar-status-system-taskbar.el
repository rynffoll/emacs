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
;; The taskbar badge shows the indicators of all tabs, with the strings
;; of `tab-bar-status-system-taskbar-badge-alist', since the badge has
;; no faces; the progress indicator shows the mean progress of all
;; sources; setting an indicator requests attention with the urgency of
;; `tab-bar-status-system-taskbar-attention-alist'.

;;; Code:

(require 'system-taskbar)
(require 'tab-bar-status)


(defcustom tab-bar-status-system-taskbar-badge-alist
  '((:warning . "🟡")
    (:error   . "🔴")
    (:success . "🟢")
    (:flag    . "🚩"))
  "Alist of indicator types and their strings in the taskbar badge.
The badge has no faces, so a colored string tells the types apart.  A
type not listed here shows its `:symbol' from
`tab-bar-status-indicator-alist'."
  :type '(alist :key-type (symbol :tag "Indicator type") :value-type string)
  :group 'tab-bar-status)

(defcustom tab-bar-status-system-taskbar-attention-alist
  '((:warning . critical)
    (:error   . critical)
    (:success . informational))
  "Alist of indicator types and the urgency of their attention requests.
Setting an indicator of a listed type calls `system-taskbar-attention'
with its urgency, `critical' or `informational'."
  :type '(alist :key-type (symbol :tag "Indicator type")
                :value-type (choice (const critical) (const informational)))
  :group 'tab-bar-status)


(defun tab-bar-status-system-taskbar--badge ()
  "Display the indicators of all tabs in the taskbar badge."
  (system-taskbar-badge
   (substring-no-properties
    (tab-bar-status-summary
     (mapcan #'tab-bar-status-indicated-tabs (frame-list)) nil
     tab-bar-status-system-taskbar-badge-alist))))

(defun tab-bar-status-system-taskbar--progress ()
  "Display the mean progress of all sources in the taskbar.
Remove the progress indicator if no source reports progress."
  (let ((ps (tab-bar-status-progresses)))
    (system-taskbar-progress
     (and ps (/ (apply #'+ ps) 100.0 (length ps))))))

(defun tab-bar-status-system-taskbar--update ()
  "Update the taskbar badge and progress indicator.
This is for use in `tab-bar-status-change-hook'."
  (tab-bar-status-system-taskbar--badge)
  (tab-bar-status-system-taskbar--progress))

(defun tab-bar-status-system-taskbar--attention (type)
  "Request attention in the taskbar for an indicator of TYPE.
The urgency comes from `tab-bar-status-system-taskbar-attention-alist'.
This is for use in `tab-bar-status-indicator-set-functions'."
  (when-let* ((urgency
               (alist-get type tab-bar-status-system-taskbar-attention-alist)))
    (system-taskbar-attention urgency)))


;;;###autoload
(define-minor-mode tab-bar-status-system-taskbar-mode
  "Toggle display of tab indicators and progress in the system taskbar.
Indicators are displayed in the taskbar badge, with the strings of
`tab-bar-status-system-taskbar-badge-alist'.  The mean progress of all
sources is displayed in the taskbar progress indicator.  Setting an
indicator requests attention with the urgency of
`tab-bar-status-system-taskbar-attention-alist'.

This mode has no effect unless `system-taskbar-mode' is enabled."
  :global t
  :group 'tab-bar-status
  (if tab-bar-status-system-taskbar-mode
      (progn
        (add-hook 'tab-bar-status-change-hook
                  #'tab-bar-status-system-taskbar--update)
        (add-hook 'tab-bar-status-indicator-set-functions
                  #'tab-bar-status-system-taskbar--attention)
        (tab-bar-status-system-taskbar--update))
    (remove-hook 'tab-bar-status-change-hook
                 #'tab-bar-status-system-taskbar--update)
    (remove-hook 'tab-bar-status-indicator-set-functions
                 #'tab-bar-status-system-taskbar--attention)
    (system-taskbar-badge nil)
    (system-taskbar-progress nil)))

(provide 'tab-bar-status-system-taskbar)
;;; tab-bar-status-system-taskbar.el ends here

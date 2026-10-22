;;; popframe-tab-line.el --- Popframe tabs in a tab line -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, frames
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

;; Show the popframe's tabs in a tab line of its window, when there are
;; two or more:
;;
;;   (setq popframe-tabs-display-function #'popframe-tab-line-display)

;;; Code:

(require 'popframe)
(require 'tab-line)


(defvar-keymap popframe-tab-line-tab-map
  :doc "Keymap of a popframe tab."
  "<tab-line> <down-mouse-1>" #'popframe-tab-line-select-tab
  ;; Else the up-click falls through to the global `tab-line' map.
  "<tab-line> <mouse-1>" #'ignore)


(defun popframe-tab-line--tab-name-format (tab tabs)
  "Format TAB, clicks on it selected by `popframe-tab-line-select-tab'.
For `tab-line-tab-name-format-function'."
  (propertize (tab-line-tab-name-format-default tab tabs)
              'keymap popframe-tab-line-tab-map))

(defconst popframe-tab-line--format
  '(:eval (let ((tab-line-tabs-function #'popframe-tabs)
                (tab-line-tab-name-format-function
                 #'popframe-tab-line--tab-name-format))
            (tab-line-format)))
  "The popframe window's `tab-line-format' parameter.")


;;;###autoload
(defun popframe-tab-line-select-tab (event)
  "Show the buffer of the popframe tab clicked in EVENT.
Not `tab-line-select-tab': the popframe window is strongly dedicated."
  (interactive "e")
  (pcase-let ((`(,string . ,pos) (posn-string (event-start event))))
    (popframe-switch-to-tab (get-text-property pos 'tab string))))

;;;###autoload
(defun popframe-tab-line-display (window)
  "Show the popframe's tabs in a tab line of WINDOW if there are two or more.
For `popframe-tabs-display-function'."
  ;; A window parameter: the tab line is only the popframe window's.
  (set-window-parameter window 'tab-line-format
                        (if (length> (popframe-tabs) 1)
                            popframe-tab-line--format
                          ;; Not nil: that falls back to the buffer's.
                          'none)))

(provide 'popframe-tab-line)
;;; popframe-tab-line.el ends here

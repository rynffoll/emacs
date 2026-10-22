;;; popframe-magit.el --- Magit in a popframe -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1") (magit "4.0"))
;; Keywords: convenience, frames, vc
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

;; Toggle the Magit status in a popframe:
;;
;;   (add-hook 'magit-mode-hook #'popframe-magit-setup)
;;   (keymap-global-set "C-c g" #'popframe-magit-status)
;;
;; A Magit buffer in a popframe, however shown there, displays the buffers
;; it opens in the popframe too, and quitting a status buffer hides the
;; popframe.  Elsewhere, `magit-display-buffer-function' and
;; `magit-bury-buffer-function' apply as usual.

;;; Code:

(require 'magit)
(require 'popframe)


(defcustom popframe-magit-frame-parameters '((width . 0.7) (height . 0.8))
  "Frame parameters of the popframe for `popframe-magit-status'.
They override `popframe-frame-parameters'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'popframe)


(defun popframe-magit--setup ()
  "Make Magit display and bury buffers from the current one in a popframe.
Buffer-local, so the global values stay in effect elsewhere."
  (setq-local magit-display-buffer-function #'popframe-magit--display-buffer
              magit-bury-buffer-function #'popframe-magit--bury-buffer))

(defun popframe-magit--window-buffer-change (window)
  "Set up the current buffer for the popframe when WINDOW shows it there.
On the buffer-local `window-buffer-change-functions'."
  (when (and (eq (window-buffer window) (current-buffer))
             (frame-parent (window-frame window)))
    (popframe-magit--setup)))

(defun popframe-magit--display-buffer (buffer)
  "Show BUFFER in the popframe's window.
Elsewhere, call the default `magit-display-buffer-function'.
For `magit-display-buffer-function'."
  (if (frame-parent)
      (progn
        ;; Undedicated, `quit-window' restores the previous buffer.
        (set-window-dedicated-p nil nil)
        (display-buffer buffer '(display-buffer-same-window)))
    (funcall (default-value 'magit-display-buffer-function) buffer)))

(defun popframe-magit--bury-buffer (&optional kill-buffer)
  "Hide the popframe from a status buffer.
Elsewhere, call the default `magit-bury-buffer-function' with
KILL-BUFFER.  For `magit-bury-buffer-function'."
  (if (and (frame-parent) (derived-mode-p 'magit-status-mode))
      (popframe-hide)
    (funcall (default-value 'magit-bury-buffer-function) kill-buffer)))


;;;###autoload
(defun popframe-magit-setup ()
  "Make Magit use the popframe from the current buffer while it is there.
For `magit-mode-hook'."
  (add-hook 'window-buffer-change-functions
            #'popframe-magit--window-buffer-change nil t))

;;;###autoload
(defun popframe-magit-status ()
  "Toggle the current repository's Magit status in a popframe."
  (interactive)
  ;; An existing buffer is shown as is: no git calls when hiding it.
  (let ((buffer (magit-get-mode-buffer 'magit-status-mode)))
    (when (and (popframe (or buffer #'magit-status-setup-buffer)
                         popframe-magit-frame-parameters)
               buffer)
      (with-current-buffer buffer
        (magit-refresh-buffer)))))

(provide 'popframe-magit)
;;; popframe-magit.el ends here

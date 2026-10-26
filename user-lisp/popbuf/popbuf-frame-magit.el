;;; popbuf-frame-magit.el --- Magit in a popbuf-frame -*- lexical-binding: t; -*-

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

;; Toggle the Magit status in a popbuf-frame:
;;
;;   (add-hook 'magit-mode-hook #'popbuf-frame-magit-setup)
;;   (keymap-global-set "C-c g" #'popbuf-frame-magit-status)
;;
;; A Magit buffer in a popbuf-frame, however shown there, displays the buffers
;; it opens in the popbuf-frame too, and quitting a status buffer hides the
;; popbuf-frame.  Elsewhere, `magit-display-buffer-function' and
;; `magit-bury-buffer-function' apply as usual.

;;; Code:

(require 'magit)
(require 'popbuf-frame)


(defcustom popbuf-frame-magit-frame-parameters '((width . 0.7) (height . 0.8))
  "Frame parameters of the popbuf-frame for `popbuf-frame-magit-status'.
They override `popbuf-frame-parameters'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'popbuf-frame)


(defun popbuf-frame-magit--setup ()
  "Make Magit display and bury buffers from the current one in a popbuf-frame.
Buffer-local, so the global values stay in effect elsewhere."
  (setq-local magit-display-buffer-function #'popbuf-frame-magit--display-buffer
              magit-bury-buffer-function #'popbuf-frame-magit--bury-buffer))

(defun popbuf-frame-magit--window-buffer-change (window)
  "Set up the current buffer for the popbuf-frame when WINDOW shows it there.
On the buffer-local `window-buffer-change-functions'."
  (when (and (eq (window-buffer window) (current-buffer))
             (frame-parent (window-frame window)))
    (popbuf-frame-magit--setup)))

(defun popbuf-frame-magit--display-buffer (buffer)
  "Show BUFFER in the popbuf-frame's window.
Elsewhere, call the default `magit-display-buffer-function'.
For `magit-display-buffer-function'."
  (if (frame-parent)
      (progn
        ;; Undedicated, `quit-window' restores the previous buffer.
        (set-window-dedicated-p nil nil)
        (display-buffer buffer '(display-buffer-same-window)))
    (funcall (default-value 'magit-display-buffer-function) buffer)))

(defun popbuf-frame-magit--bury-buffer (&optional kill-buffer)
  "Hide the popbuf-frame from a status buffer.
Elsewhere, call the default `magit-bury-buffer-function' with
KILL-BUFFER.  For `magit-bury-buffer-function'."
  (if (and (frame-parent) (derived-mode-p 'magit-status-mode))
      (popbuf-frame-hide)
    (funcall (default-value 'magit-bury-buffer-function) kill-buffer)))


;;;###autoload
(defun popbuf-frame-magit-setup ()
  "Make Magit use the popbuf-frame from the current buffer while it is there.
For `magit-mode-hook'."
  (add-hook 'window-buffer-change-functions
            #'popbuf-frame-magit--window-buffer-change nil t))

;;;###autoload
(defun popbuf-frame-magit-status ()
  "Toggle the current repository's Magit status in a popbuf-frame."
  (interactive)
  ;; An existing buffer is shown as is: no git calls when hiding it.
  (let ((buffer (magit-get-mode-buffer 'magit-status-mode)))
    (when (and (popbuf-frame (or buffer #'magit-status-setup-buffer)
                         popbuf-frame-magit-frame-parameters)
               buffer)
      (with-current-buffer buffer
        (magit-refresh-buffer)))))

(provide 'popbuf-frame-magit)
;;; popbuf-frame-magit.el ends here

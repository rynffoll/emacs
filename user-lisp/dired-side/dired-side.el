;;; dired-side.el --- Minimal Dired sidebar -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, files
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

;; Minimal Dired sidebar.  Opens a Dired buffer in a side window.
;; Customize appearance via `dired-side-mode-hook'.
;; Each project gets its own sidebar buffer (per-tab friendly).

;;; Code:

(require 'dired)
(require 'project)

(defgroup dired-side nil
  "Minimal Dired sidebar."
  :group 'dired)

(defcustom dired-side-display-alist
  '((side . left) (slot . -1) (window-width . 35))
  "Display parameters for `display-buffer-in-side-window'."
  :type 'alist)

(defcustom dired-side-root-function #'dired-side--project-root
  "Function to determine sidebar root directory.
Called with no arguments, should return a directory path."
  :type 'function)

(defcustom dired-side-follow-file-function nil
  "Function to reveal a file in the sidebar.
Called with the file path in the sidebar buffer context.
When nil, `dired-side-follow-file' only shows the sidebar."
  :type '(choice (const :tag "None" nil) function))



(defvar dired-side-mode-map (make-sparse-keymap)
  "Keymap for `dired-side-mode'.")

(define-minor-mode dired-side-mode
  "Minor mode for Dired sidebar buffers."
  :lighter ""
  :keymap dired-side-mode-map)



(defun dired-side--project-root ()
  "Return project root or `default-directory'."
  (if-let* ((proj (project-current)))
      (project-root proj)
    default-directory))

(defun dired-side--buffer-name (dir)
  "Return sidebar buffer name for DIR."
  (let ((default-directory dir))
    (project-prefixed-buffer-name "dired-side")))

(defun dired-side--get-or-create-buffer (dir)
  "Get or create a Dired sidebar buffer for DIR."
  (let* ((name (dired-side--buffer-name dir))
         (buf (get-buffer name)))
    (or buf
        ;; Let-binding `dired-buffers' keeps the registration `dired-noselect'
        ;; pushes onto it local to this call, so the sidebar stays hidden from
        ;; future `dired-find-buffer-nocreate' lookups.
        (let ((dired-buffers nil))
          (with-current-buffer (dired-noselect dir)
            (rename-buffer name)
            (dired-side-mode 1)
            (current-buffer))))))

(defun dired-side--show (buffer)
  "Display sidebar BUFFER in a side window."
  (let ((window (display-buffer-in-side-window buffer
                                               `(,@dired-side-display-alist
                                                 (dedicated . t)
                                                 (window-parameters
                                                  . ((no-other-window . t)))))))
    (set-window-parameter window 'dired-side t)
    (window-preserve-size window t t) ; preserve width during resize operations
    window))

(defun dired-side--open ()
  "Show the sidebar for the current root and return its window."
  (dired-side--show
   (dired-side--get-or-create-buffer (funcall dired-side-root-function))))

(defun dired-side--display-buffer-condition (buf _action)
  (and (window-parameter (selected-window) 'dired-side)
       (buffer-file-name (get-buffer buf))))

(add-to-list 'display-buffer-alist
             '(dired-side--display-buffer-condition
               (display-buffer-use-some-window)
               (inhibit-same-window . t)))



;;;###autoload
(defun dired-side-buffer ()
  "Return the sidebar buffer for the current project, or nil."
  (get-buffer (dired-side--buffer-name (funcall dired-side-root-function))))

;;;###autoload
(defun dired-side-get-window ()
  "Return the sidebar window, or nil if not visible."
  (when-let* ((buf (dired-side-buffer)))
    (get-buffer-window buf)))

;;;###autoload
(defun dired-side-toggle ()
  "Toggle the Dired sidebar."
  (interactive)
  (if-let* ((win (dired-side-get-window)))
      (delete-window win)
    (dired-side--open)))

;;;###autoload
(defun dired-side-jump ()
  "Jump to the sidebar window.  Open it if hidden."
  (interactive)
  (select-window (or (dired-side-get-window) (dired-side--open))))

;;;###autoload
(defun dired-side-follow-file ()
  "Show sidebar, reveal current file, and select sidebar window.
When `dired-side-follow-file-function' is set, call it with the
file path to reveal the file in the sidebar."
  (interactive)
  (let ((file (buffer-file-name))
        (win (dired-side--open)))
    (when (and file dired-side-follow-file-function)
      ;; Selecting the window makes its buffer current and keeps window point
      ;; in step with point, so revealing and recentring need no extra sync.
      (with-selected-window win
        (funcall dired-side-follow-file-function file)
        (recenter)))
    (select-window win)))

(provide 'dired-side)
;;; dired-side.el ends here

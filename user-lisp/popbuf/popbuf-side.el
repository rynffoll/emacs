;;; popbuf-side.el --- Toggle any buffer in a side window -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, windows
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

;; A panel in a side window, the side window counterpart of `popbuf-frame'.
;; `popbuf-side' toggles it: hidden, it shows the buffer it showed last, else
;; `popbuf-side-default-buffer'.  With a buffer spec (a buffer, a buffer name,
;; or a function returning one), it toggles that buffer:
;;
;;   (setq popbuf-side-default-buffer #'ghostel-project)
;;   (popbuf-side)             ; toggle the panel
;;   (popbuf-side #'ghostel)   ; toggle some other buffer in it
;;
;; `popbuf-side-display-buffer' sends buffers to the panel from
;; `display-buffer-alist':
;;
;;   (add-to-list 'display-buffer-alist
;;                '((derived-mode . help-mode) (popbuf-side-display-buffer)))
;;
;; The buffers shown in the panel under one key, e.g. one project (see
;; `popbuf-key-function'), are tabs of its tab line, when there are two
;; or more; `popbuf-side-pin' toggles the current buffer as one.  Hiding
;; deletes the window; its buffers live on.

;;; Code:

(require 'popbuf)

(defgroup popbuf-side nil
  "Toggle any buffer in a side window."
  :group 'popbuf)

(defcustom popbuf-side-default-buffer nil
  "Default buffer spec shown by `popbuf-side' when called with no argument.
A buffer, a buffer name, or a function returning one.  A function is
called inside `save-window-excursion'."
  :type '(choice (const :tag "None" nil) function string))

(defcustom popbuf-side-display-alist
  '((side . bottom)
    (slot . -1)
    (window-height . 0.4))
  "Action alist for `display-buffer-in-side-window', applied on every showing.
The default slot is -1, not 0: transient menus use the bottom slot 0."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom popbuf-side-mode-line-pin "⬓"
  "Mode line marker of a buffer pinned by `popbuf-side-pin'.
See `popbuf-side-mode-line-format'."
  :type 'string)

(defface popbuf-side-mode-line '((t :inherit shadow))
  "Face of `popbuf-side-mode-line-pin' on the mode line.")


(defvar popbuf-side--buffers nil
  "The buffers the panel has shown, in that order.")

(defvar-local popbuf-side--key nil
  "Key of the context this buffer was last shown in the panel from.")
(put 'popbuf-side--key 'permanent-local t)

(defvar-local popbuf-side--pinned nil
  "Non-nil in a buffer pinned by `popbuf-side-pin'.")
(put 'popbuf-side--pinned 'permanent-local t)

(defun popbuf-side--frame ()
  "Return the frame of the panel: the selected one, or its parent frame."
  (or (frame-parent) (selected-frame)))

(defun popbuf-side--window ()
  "Return the panel's window, or nil.
Its `popbuf-side' window parameter is the key of its tabs."
  (seq-find (lambda (window) (window-parameter window 'popbuf-side))
            (window-list (popbuf-side--frame) 'nomini)))

(defun popbuf-side--key-buffers (key)
  "Return the live buffers the panel has shown under KEY, in that order."
  (setq popbuf-side--buffers (seq-filter #'buffer-live-p popbuf-side--buffers))
  (seq-filter (lambda (buffer)
                (equal key (buffer-local-value 'popbuf-side--key buffer)))
              popbuf-side--buffers))

(defun popbuf-side--tabs ()
  "Return the panel's tabs.  For `tab-line-tabs-function'."
  (when-let* ((window (popbuf-side--window)))
    (popbuf-side--key-buffers (window-parameter window 'popbuf-side))))

(defun popbuf-side--update-tab-line ()
  "Show the panel's tab line if it has two tabs or more."
  (when-let* ((window (popbuf-side--window)))
    (popbuf--update-tab-line window)))

(defun popbuf-side--kill-buffer-update ()
  "Drop the buffer being killed from the panel's tabs.
On `kill-buffer-hook'."
  (when popbuf-side--key
    (setq popbuf-side--key nil)
    (popbuf-side--update-tab-line)))

(defun popbuf-side--show (buffer &optional parameters)
  "Show BUFFER in the panel, under the current buffer's key.
Return the panel's window.  PARAMETERS override `popbuf-side-display-alist'."
  (let ((key (popbuf--key)))
    (with-current-buffer buffer
      (setq popbuf-side--key key))
    (unless (memq buffer popbuf-side--buffers)
      (setq popbuf-side--buffers (append popbuf-side--buffers (list buffer))))
    (popbuf--install)
    (add-hook 'kill-buffer-hook #'popbuf-side--kill-buffer-update)
    (prog1 (with-selected-frame (popbuf-side--frame)
             (display-buffer-in-side-window
              buffer
              `(,@parameters
                ,@popbuf-side-display-alist
                (window-parameters (popbuf-side . ,key)
                                   (popbuf-tabs . popbuf-side--tabs)
                                   (no-delete-other-windows . t)))))
      (popbuf-side--update-tab-line))))


;;;###autoload
(defun popbuf-side-display-buffer (buffer _alist)
  "Display BUFFER in the panel and return its window.
A `display-buffer' action function."
  (popbuf-side--show buffer))

;;;###autoload
(defun popbuf-side (&optional spec parameters)
  "Toggle the panel, or with SPEC, the panel showing SPEC.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'.  Without SPEC, hide the panel,
or show the last shown of its tabs under the current key, else
`popbuf-side-default-buffer'.  Return the buffer if shown, nil if hidden.

PARAMETERS override `popbuf-side-display-alist' for this call."
  (interactive)
  (let ((window (popbuf-side--window))
        (buffer (and spec (popbuf--resolve spec))))
    (cond
     ((and window (or (not spec) (eq buffer (window-buffer window))))
      (popbuf-side-hide)
      nil)
     (t
      (unless spec
        (setq buffer
              ;; All shown, so all have a display time.
              (or (car (sort (popbuf-side--key-buffers (popbuf--key))
                             :key (lambda (buffer)
                                    (float-time (buffer-local-value
                                                 'buffer-display-time buffer)))
                             :reverse t))
                  (popbuf--resolve popbuf-side-default-buffer))))
      (unless (buffer-live-p buffer)
        (user-error "popbuf-side: spec yielded no buffer"))
      (select-window (popbuf-side--show buffer parameters))
      buffer))))

;;;###autoload
(defun popbuf-side-pin ()
  "Pin the current buffer as a panel tab under its key.
Called in a pinned buffer, unpin it and drop it from the tabs; if the
panel shows it, show a neighbor tab, or hide the panel without one."
  (interactive)
  (let ((buffer (current-buffer)))
    (if popbuf-side--pinned
        (let* ((window (popbuf-side--window))
               (tabs (popbuf-side--tabs))
               (pos (seq-position tabs buffer))
               (rest (remq buffer tabs)))
          (setq popbuf-side--pinned nil
                popbuf-side--key nil)
          (when (and pos window (eq buffer (window-buffer window)))
            (if rest
                (set-window-buffer window
                                   (nth (min pos (1- (length rest))) rest))
              (popbuf-side-hide)))
          (popbuf-side--update-tab-line)
          (message "popbuf-side: unpinned buffer `%s'" (buffer-name buffer)))
      (setq popbuf-side--pinned t
            popbuf-side--key (popbuf--key))
      (unless (memq buffer popbuf-side--buffers)
        (setq popbuf-side--buffers (append popbuf-side--buffers (list buffer))))
      (popbuf-side--update-tab-line)
      (message "popbuf-side: pinned buffer `%s'" (buffer-name buffer)))))

;;;###autoload
(defun popbuf-side-hide ()
  "Hide the panel if it is shown."
  (interactive)
  (when-let* ((window (popbuf-side--window)))
    (delete-window window)))

(defvar popbuf-side-mode-line-format '(:eval (popbuf-side-mode-line-format))
  "Mode line construct marking a buffer pinned by `popbuf-side-pin'.
Put it into `mode-line-format'.")
(put 'popbuf-side-mode-line-format 'risky-local-variable t)

(defun popbuf-side-mode-line-format ()
  "Return `popbuf-side-mode-line-pin' in a pinned buffer, else nil."
  (when popbuf-side--pinned
    (concat " "
            (propertize popbuf-side-mode-line-pin
                        'face 'popbuf-side-mode-line
                        'help-echo "popbuf-side: pinned"))))

(provide 'popbuf-side)
;;; popbuf-side.el ends here

;;; popside.el --- Toggle any buffer in a side window -*- lexical-binding: t; -*-

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

;; A panel in a side window, the side window counterpart of `popframe'.
;; `popside' toggles it: hidden, it shows the buffer it showed last, else
;; `popside-default-buffer'.  With a buffer spec (a buffer, a buffer name,
;; or a function returning one), it toggles that buffer:
;;
;;   (setq popside-default-buffer #'ghostel-project)
;;   (popside)             ; toggle the panel
;;   (popside #'ghostel)   ; toggle some other buffer in it
;;
;; `popside-display-buffer' sends buffers to the panel from
;; `display-buffer-alist':
;;
;;   (add-to-list 'display-buffer-alist
;;                '((derived-mode . help-mode) (popside-display-buffer)))
;;
;; The buffers shown in the panel under one key, e.g. one project (see
;; `popside-key-function'), are tabs of its tab line, when there are two
;; or more; `popside-pin' toggles the current buffer as one.  In the
;; panel, `next-buffer' and `previous-buffer' switch tabs.  Hiding
;; deletes the window; its buffers live on.

;;; Code:

(defgroup popside nil
  "Toggle any buffer in a side window."
  :group 'convenience)

(defcustom popside-default-buffer nil
  "Default buffer spec shown by `popside' when called with no argument.
A buffer, a buffer name, or a function returning one.  A function is
called inside `save-window-excursion'."
  :type '(choice (const :tag "None" nil) function string))

(defcustom popside-key-function #'ignore
  "Function returning the current buffer's panel key, or nil.
The panel's tabs are the buffers it has shown under one key; nil stands
for a single global key.  See `popside-project-key' for per-project
tabs."
  :type 'function)

(defcustom popside-display-alist
  '((side . bottom)
    (slot . -1)
    (window-height . 0.4))
  "Action alist for `display-buffer-in-side-window', applied on every showing.
The default slot is -1, not 0: transient menus use the bottom slot 0."
  :type '(alist :key-type symbol :value-type sexp))

(defvar-keymap popside-tabs-map
  :doc "Keys of the panel's window, as `tab-line-mode''s."
  "<remap> <next-buffer>" '(menu-item "" popside-switch-to-next-tab
                                      :filter popside--tab-command)
  "<remap> <previous-buffer>" '(menu-item "" popside-switch-to-prev-tab
                                          :filter popside--tab-command)
  ;; Remapped by name only: no dependency on Evil.
  "<remap> <evil-next-buffer>" '(menu-item "" popside-switch-to-next-tab
                                           :filter popside--tab-command)
  "<remap> <evil-prev-buffer>" '(menu-item "" popside-switch-to-prev-tab
                                           :filter popside--tab-command))


(defvar popside--buffers nil
  "The buffers the panel has shown, in that order.")

(defvar-local popside--key nil
  "Key of the context this buffer was last shown in the panel from.")
(put 'popside--key 'permanent-local t)

(defvar-local popside--pin nil
  "Non-nil in a buffer pinned by `popside-pin'.")
(put 'popside--pin 'permanent-local t)

(defun popside--current-key ()
  "Return the current buffer's panel key; see `popside-key-function'."
  (or (funcall popside-key-function) t))

(defun popside--frame ()
  "Return the frame of the panel: the selected one, or its parent frame."
  (or (frame-parent) (selected-frame)))

(defun popside--window ()
  "Return the panel's window, or nil.
Its `popside' window parameter is the key of its tabs."
  (seq-find (lambda (window) (window-parameter window 'popside))
            (window-list (popside--frame) 'nomini)))

(defun popside--key-buffers (key)
  "Return the live buffers the panel has shown under KEY, in that order."
  (setq popside--buffers (seq-filter #'buffer-live-p popside--buffers))
  (seq-filter (lambda (buffer)
                (equal key (buffer-local-value 'popside--key buffer)))
              popside--buffers))

(defun popside--tabs ()
  "Return the panel's tabs.  For `tab-line-tabs-function'."
  (when-let* ((window (popside--window)))
    (popside--key-buffers (window-parameter window 'popside))))

(defun popside--selected-tabs ()
  "Return the panel's tabs if its window is selected, else nil."
  (and (window-parameter nil 'popside)
       (popside--tabs)))

(defun popside--tab-command (command)
  "Return COMMAND in the panel's window with tabs, else nil.
A `menu-item' filter: elsewhere the key keeps its usual binding."
  (and (popside--selected-tabs) command))

(defconst popside--tab-line-format
  '(:eval (let ((tab-line-tabs-function #'popside--tabs))
            (tab-line-format)))
  "The panel window's `tab-line-format' parameter.")

(defun popside--update-tab-line ()
  "Show the panel's tab line if it has two tabs or more."
  (when-let* ((window (popside--window)))
    (set-window-parameter window 'tab-line-format
                          (if (length> (popside--tabs) 1)
                              popside--tab-line-format
                            ;; Not nil: that falls back to the buffer's.
                            'none))))

(defun popside--kill-buffer-update ()
  "Drop the buffer being killed from the panel's tabs.
On `kill-buffer-hook'."
  (when popside--key
    (setq popside--key nil)
    (popside--update-tab-line)))

(defun popside--resolve (spec)
  "Resolve SPEC to a live buffer, or nil.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'."
  (when-let* ((buffer (if (functionp spec)
                          (save-window-excursion (funcall spec))
                        spec)))
    (get-buffer buffer)))

(defun popside--show (buffer &optional parameters)
  "Show BUFFER in the panel, under the current buffer's key.
Return the panel's window.  PARAMETERS override `popside-display-alist'."
  (let ((key (popside--current-key)))
    (with-current-buffer buffer
      (setq popside--key key))
    (unless (memq buffer popside--buffers)
      (setq popside--buffers (append popside--buffers (list buffer))))
    ;; Before minor mode maps, which may bind the same keys.
    (add-to-list 'emulation-mode-map-alists
                 `((popside--buffers . ,popside-tabs-map)))
    (add-hook 'kill-buffer-hook #'popside--kill-buffer-update)
    (prog1 (with-selected-frame (popside--frame)
             (display-buffer-in-side-window
              buffer
              `(,@parameters
                ,@popside-display-alist
                (window-parameters (popside . ,key)
                                   (no-delete-other-windows . t)))))
      (popside--update-tab-line))))


;;;###autoload
(defun popside-display-buffer (buffer _alist)
  "Display BUFFER in the panel and return its window.
A `display-buffer' action function."
  (popside--show buffer))

;;;###autoload
(defun popside (&optional spec parameters)
  "Toggle the panel, or with SPEC, the panel showing SPEC.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'.  Without SPEC, hide the panel,
or show the last shown of its tabs under the current key, else
`popside-default-buffer'.  Return the buffer if shown, nil if hidden.

PARAMETERS override `popside-display-alist' for this call."
  (interactive)
  (let ((window (popside--window))
        (buffer (and spec (popside--resolve spec))))
    (cond
     ((and window (or (not spec) (eq buffer (window-buffer window))))
      (popside-hide)
      nil)
     (t
      (unless spec
        (setq buffer
              ;; All shown, so all have a display time.
              (or (car (sort (popside--key-buffers (popside--current-key))
                             :key (lambda (buffer)
                                    (float-time (buffer-local-value
                                                 'buffer-display-time buffer)))
                             :reverse t))
                  (popside--resolve popside-default-buffer))))
      (unless (buffer-live-p buffer)
        (user-error "popside: spec yielded no buffer"))
      (select-window (popside--show buffer parameters))
      buffer))))

;;;###autoload
(defun popside-pinned-p ()
  "Return non-nil if the current buffer is pinned by `popside-pin'."
  popside--pin)

;;;###autoload
(defun popside-pin ()
  "Pin the current buffer as a panel tab under its key.
Called in a pinned buffer, unpin it and drop it from the tabs; if the
panel shows it, show a neighbor tab, or hide the panel without one."
  (interactive)
  (let ((buffer (current-buffer)))
    (if popside--pin
        (let* ((window (popside--window))
               (tabs (popside--tabs))
               (pos (seq-position tabs buffer))
               (rest (remq buffer tabs)))
          (setq popside--pin nil
                popside--key nil)
          (when (and pos window (eq buffer (window-buffer window)))
            (if rest
                (set-window-buffer window
                                   (nth (min pos (1- (length rest))) rest))
              (popside-hide)))
          (popside--update-tab-line)
          (message "popside: unpinned buffer `%s'" (buffer-name buffer)))
      (setq popside--pin t
            popside--key (popside--current-key))
      (unless (memq buffer popside--buffers)
        (setq popside--buffers (append popside--buffers (list buffer))))
      (popside--update-tab-line)
      (message "popside: pinned buffer `%s'" (buffer-name buffer)))))

;;;###autoload
(defun popside-switch-to-next-tab (&optional arg)
  "Switch to the ARGth next panel tab, cycling."
  (interactive "p")
  (let* ((tabs (or (popside--selected-tabs)
                   (user-error "No popside tabs here")))
         (pos (or (seq-position tabs (window-buffer)) -1)))
    (switch-to-buffer (nth (mod (+ pos (or arg 1)) (length tabs)) tabs)
                      nil t)))

;;;###autoload
(defun popside-switch-to-prev-tab (&optional arg)
  "Switch to the ARGth previous panel tab, cycling."
  (interactive "p")
  (popside-switch-to-next-tab (- (or arg 1))))

;;;###autoload
(defun popside-hide ()
  "Hide the panel if it is shown."
  (interactive)
  (when-let* ((window (popside--window)))
    (delete-window window)))

(provide 'popside)
;;; popside.el ends here

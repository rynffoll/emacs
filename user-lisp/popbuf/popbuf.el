;;; popbuf.el --- Toggle any buffer in a popup, with tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, frames, windows
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

;; The core shared by the popbuf displays: `popbuf-frame', a centered
;; child frame, and `popbuf-side', a side window panel.  It resolves
;; buffer specs and the key that groups a display's tabs, and switches
;; tabs.  A display's window has a `popbuf-tabs' parameter: a function
;; returning its tabs.  With two tabs or more, the window shows them in
;; a tab line; there, `next-buffer' and `previous-buffer' switch tabs.

;;; Code:

(require 'tab-line)

(defgroup popbuf nil
  "Toggle any buffer in a popup, with tabs."
  :group 'convenience)

(defcustom popbuf-key-function #'ignore
  "Function returning the current buffer's key, or nil.
Buffers under one key share tabs; nil stands for a single global key.
See `popbuf-project-key' for per-project tabs."
  :type 'function)

(defvar-keymap popbuf-tabs-map
  :doc "Keys of a popbuf window with tabs, as `tab-line-mode''s."
  "<remap> <next-buffer>" '(menu-item "" popbuf-switch-to-next-tab
                                      :filter popbuf--tab-command)
  "<remap> <previous-buffer>" '(menu-item "" popbuf-switch-to-prev-tab
                                          :filter popbuf--tab-command)
  ;; Remapped by name only: no dependency on Evil.
  "<remap> <evil-next-buffer>" '(menu-item "" popbuf-switch-to-next-tab
                                           :filter popbuf--tab-command)
  "<remap> <evil-prev-buffer>" '(menu-item "" popbuf-switch-to-prev-tab
                                           :filter popbuf--tab-command))

(defvar-keymap popbuf-tab-line-tab-map
  :doc "Keymap of a popbuf tab."
  "<tab-line> <down-mouse-1>" #'popbuf-tab-line-select-tab
  ;; Else the up-click falls through to the global `tab-line' map.
  "<tab-line> <mouse-1>" #'ignore)


(defun popbuf--key ()
  "Return the current buffer's key; see `popbuf-key-function'."
  (or (funcall popbuf-key-function) t))

(defun popbuf--resolve (spec)
  "Resolve SPEC to a live buffer, or nil.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'."
  (when-let* ((buffer (if (functionp spec)
                          (save-window-excursion (funcall spec))
                        spec)))
    (get-buffer buffer)))

(defun popbuf--tabs (&optional window)
  "Return the tabs of WINDOW, a popbuf window, or nil.
WINDOW defaults to the selected one.  For `tab-line-tabs-function'."
  (when-let* ((function (window-parameter window 'popbuf-tabs)))
    (funcall function)))

(defun popbuf--tab-command (command)
  "Return COMMAND in a popbuf window with tabs, else nil.
A `menu-item' filter: elsewhere the key keeps its usual binding."
  (and (popbuf--tabs) command))

(defvar popbuf--installed nil
  "Non-nil once `popbuf--install' has run.")

(defun popbuf--install ()
  "Make `popbuf-tabs-map' active."
  (unless popbuf--installed
    ;; Before minor mode maps, which may bind the same keys.
    (add-to-list 'emulation-mode-map-alists
                 `((popbuf--installed . ,popbuf-tabs-map)))
    (setq popbuf--installed t)))

(defun popbuf--switch-to-tab (window buffer)
  "Show BUFFER, a tab, in WINDOW, keeping WINDOW's dedication."
  (let ((dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window dedicated)))

(defun popbuf--tab-name-format (tab tabs)
  "Format TAB, clicks on it selected by `popbuf-tab-line-select-tab'.
For `tab-line-tab-name-format-function'."
  (propertize (tab-line-tab-name-format-default tab tabs)
              'keymap popbuf-tab-line-tab-map))

(defconst popbuf--tab-line-format
  '(:eval (let ((tab-line-tabs-function #'popbuf--tabs)
                (tab-line-tab-name-format-function #'popbuf--tab-name-format))
            (tab-line-format)))
  "A popbuf window's `tab-line-format' parameter.")

(defun popbuf--update-tab-line (window)
  "Show the tabs of WINDOW, a popbuf window, in a tab line if two or more."
  ;; A window parameter: the tab line is only the popbuf window's.
  (set-window-parameter window 'tab-line-format
                        (if (length> (popbuf--tabs window) 1)
                            popbuf--tab-line-format
                          ;; Not nil: that falls back to the buffer's.
                          'none)))


;;;###autoload
(defun popbuf-switch-to-next-tab (&optional arg)
  "Switch to the ARGth next tab of the selected popbuf window, cycling."
  (interactive "p")
  (let* ((tabs (or (popbuf--tabs) (user-error "No popbuf tabs here")))
         (pos (or (seq-position tabs (window-buffer)) -1)))
    (popbuf--switch-to-tab (selected-window)
                           (nth (mod (+ pos (or arg 1)) (length tabs)) tabs))))

;;;###autoload
(defun popbuf-switch-to-prev-tab (&optional arg)
  "Switch to the ARGth previous tab of the selected popbuf window, cycling."
  (interactive "p")
  (popbuf-switch-to-next-tab (- (or arg 1))))

;;;###autoload
(defun popbuf-tab-line-select-tab (event)
  "Show the buffer of the popbuf tab clicked in EVENT.
Not `tab-line-select-tab': a popbuf window may be strongly dedicated."
  (interactive "e")
  (pcase-let* ((posn (event-start event))
               (`(,string . ,pos) (posn-string posn)))
    (popbuf--switch-to-tab (posn-window posn)
                           (get-text-property pos 'tab string))))

(provide 'popbuf)
;;; popbuf.el ends here

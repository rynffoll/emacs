;;; per-tab-group-theme.el --- Per Tab Group Theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ruslan Kamashev

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
;; Automatically switch theme per tab-bar "group".

;;; Code:

(require 'tab-bar)


(defvar per-tab-group-theme-mode) ;; forward decl to silence byte-compiler



(defgroup per-tab-group-theme nil
  "Per Tab Group Theme."
  :group 'tab-bar)

(defcustom per-tab-group-theme-default-theme 'modus-operandi
  "Theme to use when no specific theme is set for a tab group."
  :type 'symbol
  :group 'per-tab-group-theme)

(defcustom per-tab-group-theme-alist nil
  "Alist mapping tab group names (strings) to theme symbols.
Example: ((\"Work\" . modus-vivendi) (\"Play\" . catppuccin-latte))."
  :type '(alist :key-type string :value-type symbol)
  :group 'per-tab-group-theme)



(defun per-tab-group-theme--current-group ()
  "Return current tab group string or \"default\"."
  (or (alist-get 'group (tab-bar--current-tab))
      "default"))

(defun per-tab-group-theme--target-theme ()
  "Return theme symbol for current group, or the default theme."
  (or (alist-get (per-tab-group-theme--current-group)
                 per-tab-group-theme-alist
                 nil nil #'string=)
      per-tab-group-theme-default-theme))

(defun per-tab-group-theme--apply (&optional _frame _tab)
  "Apply appropriate theme for current tab group."
  (let* ((target (per-tab-group-theme--target-theme))
         (already (car custom-enabled-themes)))
    (unless (eq target already)
      (condition-case err
          (progn
            ;; N.B. disable first to avoid mixed faces from multiple themes.
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme target :no-confirm))
        (error
         ;; Restore previous if something exploded
         (when already
           (ignore-errors (load-theme already :no-confirm)))
         (message "[per-tab-group-theme] Failed to load %S: %s"
                  target (error-message-string err)))))))



;;;###autoload
(defun per-tab-group-theme-set (group)
  "Set theme for tab GROUP using consult-theme for selection.
GROUP is a string (tab group name)."
  (interactive (list (per-tab-group-theme--current-group)))
  (let ((selected-theme (if (fboundp 'consult-theme)
                            (progn
                              (call-interactively #'consult-theme)
                              (car custom-enabled-themes))
                          (intern (completing-read
                                   "Theme: " (mapcar #'symbol-name (custom-available-themes))
                                   nil t)))))
    (when selected-theme
      (setf (alist-get group per-tab-group-theme-alist nil nil #'string=) selected-theme))))

;;;###autoload
(defun per-tab-group-theme-clear (group)
  "Clear theme mapping for tab GROUP."
  (interactive (list (per-tab-group-theme--current-group)))
  (setq per-tab-group-theme-alist
        (assoc-delete-all group per-tab-group-theme-alist #'string=))
  (when per-tab-group-theme-mode
    (per-tab-group-theme--apply)))

;;;###autoload
(defun per-tab-group-theme-clear-all ()
  "Clear all tab group theme mappings."
  (interactive)
  (setq per-tab-group-theme-alist nil)
  (when per-tab-group-theme-mode
    (per-tab-group-theme--apply)))



(defun per-tab-group-theme--enable-hooks ()
  "Enable hooks used by `per-tab-group-theme-mode'."
  (add-hook 'tab-bar-tab-post-select-functions #'per-tab-group-theme--apply)
  (add-hook 'tab-bar-tab-post-open-functions   #'per-tab-group-theme--apply)
  (add-hook 'tab-bar-tab-post-change-group-functions #'per-tab-group-theme--apply)
  (add-hook 'after-make-frame-functions        #'per-tab-group-theme--apply))

(defun per-tab-group-theme--disable-hooks ()
  "Disable hooks used by `per-tab-group-theme-mode'."
  (remove-hook 'tab-bar-tab-post-select-functions #'per-tab-group-theme--apply)
  (remove-hook 'tab-bar-tab-post-open-functions   #'per-tab-group-theme--apply)
  (remove-hook 'tab-bar-tab-post-change-group-functions #'per-tab-group-theme--apply)
  (remove-hook 'after-make-frame-functions        #'per-tab-group-theme--apply))


;;;###autoload
(define-minor-mode per-tab-group-theme-mode
  "Toggle per-tab-group theme switching."
  :global t
  :group 'per-tab-group-theme
  (if per-tab-group-theme-mode
      (progn
        (per-tab-group-theme--enable-hooks)
        (per-tab-group-theme--apply))
    (per-tab-group-theme--disable-hooks)))

(provide 'per-tab-group-theme)
;;; per-tab-group-theme.el ends here

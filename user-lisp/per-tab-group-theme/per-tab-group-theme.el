;;; per-tab-group-theme.el --- Per Tab Group Theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ruslan Kamashev
;; Copyright (C) 2025  Ruslan Kamashev and Claude Code

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
;; Automatically switch theme per tab-bar "group" by hooking into standard
;; theme loading mechanisms. When you change theme via load-theme in a tab
;; group, it saves that mapping. When switching to a tab group, it restores
;; the associated theme or falls back to the default theme.

;;; Code:

(require 'tab-bar)



(defgroup per-tab-group-theme nil
  "Per Tab Group Theme."
  :group 'tab-bar)

(defcustom per-tab-group-theme-alist nil
  "Alist mapping tab group names (strings) to theme symbols.
Example: ((\"Work\" . modus-vivendi) (\"Play\" . catppuccin-latte))."
  :type '(alist :key-type string :value-type symbol)
  :group 'per-tab-group-theme)



(defvar per-tab-group-theme--ignore-theme-change nil
  "Flag to ignore theme changes during internal switching.")

(defvar per-tab-group-theme--default-theme nil
  "Default theme to use when no group-specific theme is set.")

(defun per-tab-group-theme--current-group ()
  "Return current tab group string or nil if no group."
  (alist-get 'group (tab-bar--current-tab)))

(defun per-tab-group-theme--get-default-theme ()
  "Get default theme."
  (or per-tab-group-theme--default-theme
      (car custom-enabled-themes)))

(defun per-tab-group-theme--get-group-theme (group)
  "Get theme for GROUP or default if no mapping exists."
  (if group
      (or (alist-get group per-tab-group-theme-alist nil nil #'string=)
          (per-tab-group-theme--get-default-theme))
    (per-tab-group-theme--get-default-theme)))

(defun per-tab-group-theme--set-group-theme (group theme)
  "Set THEME for GROUP, or set default theme if GROUP is nil."
  (if group
      (setf (alist-get group per-tab-group-theme-alist nil nil #'string=) theme)
    (setq per-tab-group-theme--default-theme theme)))

(defun per-tab-group-theme--on-enable-theme (theme)
  "Hook to save THEME mapping for current group when theme is loaded."
  (unless per-tab-group-theme--ignore-theme-change
    (when per-tab-group-theme-mode
      (per-tab-group-theme--set-group-theme
       (per-tab-group-theme--current-group) theme))))

(defun per-tab-group-theme--apply-for-group (&optional _frame _tab)
  "Apply appropriate theme for current tab group."
  (when per-tab-group-theme-mode
    (let* ((current-group (per-tab-group-theme--current-group))
           (target-theme (per-tab-group-theme--get-group-theme current-group))
           (current-theme (car custom-enabled-themes)))
      (when (and target-theme (not (eq target-theme current-theme)))
        (let ((per-tab-group-theme--ignore-theme-change t))
          (condition-case err
              (progn
                (mapc #'disable-theme custom-enabled-themes)
                (load-theme target-theme :no-confirm))
            (error
             (when current-theme
               (ignore-errors (load-theme current-theme :no-confirm)))
             (message "[per-tab-group-theme] Failed to load %S: %s"
                      target-theme (error-message-string err)))))))))

;;;###autoload
(defun per-tab-group-theme-reset (group)
  "Reset theme mapping for GROUP and apply default theme.
When called interactively, uses current group."
  (interactive (list (per-tab-group-theme--current-group)))
  (when group
    (setq per-tab-group-theme-alist
          (assq-delete-all group per-tab-group-theme-alist)))
  (per-tab-group-theme--apply-for-group))

;;;###autoload
(defun per-tab-group-theme-reset-all ()
  "Reset all tab group theme mappings and apply default theme."
  (interactive)
  (setq per-tab-group-theme-alist nil)
  (per-tab-group-theme--apply-for-group))

;;;###autoload
(defun per-tab-group-theme-show-mappings ()
  "Show current theme mappings."
  (interactive)
  (let ((current-group (per-tab-group-theme--current-group)))
    (message "Default theme: %s\nGroups:\n%s"
             (or per-tab-group-theme--default-theme "none")
             (if per-tab-group-theme-alist
                 (let ((max-width (apply #'max (mapcar (lambda (pair) (length (car pair)))
                                                       per-tab-group-theme-alist))))
                   (mapconcat (lambda (pair)
                                (let ((format-string (format "  %%-%ds → %%s" max-width))
                                      (group (car pair))
                                      (theme (cdr pair)))
                                  (format "%s%s"
                                          (if (and current-group (string= group current-group)) "•" " ")
                                          (format format-string group theme))))
                              per-tab-group-theme-alist "\n"))
               "  none"))))

(defun per-tab-group-theme--activate ()
  "Activate hooks and advice used by `per-tab-group-theme-mode'."
  (add-hook 'tab-bar-tab-post-select-functions #'per-tab-group-theme--apply-for-group)
  (add-hook 'tab-bar-tab-post-open-functions #'per-tab-group-theme--apply-for-group)
  (add-hook 'tab-bar-tab-post-change-group-functions #'per-tab-group-theme--apply-for-group)
  (add-hook 'after-make-frame-functions #'per-tab-group-theme--apply-for-group)

  (add-hook 'enable-theme-functions #'per-tab-group-theme--on-enable-theme))

(defun per-tab-group-theme--deactivate ()
  "Deactivate hooks and advice used by `per-tab-group-theme-mode'."
  (remove-hook 'tab-bar-tab-post-select-functions #'per-tab-group-theme--apply-for-group)
  (remove-hook 'tab-bar-tab-post-open-functions #'per-tab-group-theme--apply-for-group)
  (remove-hook 'tab-bar-tab-post-change-group-functions #'per-tab-group-theme--apply-for-group)
  (remove-hook 'after-make-frame-functions #'per-tab-group-theme--apply-for-group)

  (remove-hook 'enable-theme-functions #'per-tab-group-theme--on-enable-theme))

;;;###autoload
(define-minor-mode per-tab-group-theme-mode
  "Toggle per-tab-group theme switching.

When enabled, this mode:
1. Automatically saves theme when you use `load-theme' in a tab group
2. Restores appropriate theme when switching between tab groups
3. Uses the default theme (first in `custom-enabled-themes') for ungrouped tabs

Usage:
- Use `load-theme' normally - themes are saved per tab group automatically
- Use `per-tab-group-theme-reset' to reset current group to default theme
- Use `per-tab-group-theme-reset-all' to reset all groups to default theme
- Use `per-tab-group-theme-show-mappings' to view mappings (* marks active group)"
  :global t
  :group 'per-tab-group-theme
  (if per-tab-group-theme-mode
      (progn
        (per-tab-group-theme--activate)
        (unless per-tab-group-theme--default-theme
          (setq per-tab-group-theme--default-theme (car custom-enabled-themes)))
        (per-tab-group-theme--apply-for-group))
    (per-tab-group-theme--deactivate)))

(provide 'per-tab-group-theme)
;;; per-tab-group-theme.el ends here

;;; tab-bar-theme.el --- Tab Bar Theme  -*- lexical-binding: t; -*-

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

;; Theme for `tab-bar-mode'

;;; Code:

(require 'tab-bar)


(defgroup tab-bar-theme
  nil
  "Tab Bar Theme."
  :group 'tab-bar)

(defcustom tab-bar-theme-height 5
  "Height of tab bar."
  :type 'integer
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-font-height 0.9
  "Font height of tab bar."
  :type 'float
  :group 'tab-bar-theme)


;;;###autoload
(defun tab-bar-theme--apply (&optional _theme)
  "Apply faces to current theme."
  (let* ((bg-inactive (face-attribute 'mode-line-inactive :background))
         (fg-inactive (face-attribute 'mode-line-inactive :foreground))
         (bg-active   (face-attribute 'default :background))
         (fg-active   (face-attribute 'default :foreground))
         (height      tab-bar-theme-height)
         (font-height tab-bar-theme-font-height))
    (custom-set-faces
     `(tab-bar
       ((t ( :inherit unspecified
             :background ,bg-inactive
             :foreground ,fg-inactive
             :height ,font-height
             :box ,(tab-bar-theme--box-style height bg-inactive)))))
     `(tab-bar-tab
       ((t ( :inherit tab-bar
             :background ,bg-active
             :foreground ,fg-active
             :box ,(tab-bar-theme--box-style height bg-active)))))
     `(tab-bar-tab-inactive
       ((t ( :inherit tab-bar-tab
             :background ,bg-inactive
             :foreground ,fg-inactive
             :box ,(tab-bar-theme--box-style height bg-inactive)))))
     `(tab-bar-tab-ungrouped
       ((t ( :inherit tab-bar-tab-inactive
             :background ,bg-inactive
             :foreground ,fg-inactive
             :box ,(tab-bar-theme--box-style height bg-inactive)))))
     `(tab-bar-tab-group-inactive
       ((t ( :inherit tab-bar-tab-inactive
             :background ,bg-inactive
             :foreground ,fg-inactive
             :weight bold
             :box ,(tab-bar-theme--box-style height bg-inactive)))))
     `(tab-bar-tab-group-current
       ((t ( :inherit tab-bar-tab
             :background ,bg-inactive
             :foreground ,fg-active
             :weight bold
             :box ,(tab-bar-theme--box-style height bg-inactive))))))))

(defun tab-bar-theme--box-style (height color)
  "Return box style for tab bar."
  (when (> height 0)
    `(:line-width ,height :style nil :color ,color)))

;;;###autoload
(define-minor-mode tab-bar-theme-mode
  "Toggle `tab-bar-theme-mode'."
  :global t :group 'tab-bar-theme
  (cond
   (tab-bar-theme-mode
    (tab-bar-theme--apply)
    (add-hook 'enable-theme-functions #'tab-bar-theme--apply))
   (t
    (remove-hook 'enable-theme-functions #'tab-bar-theme--apply))))

(provide 'tab-bar-theme)
;;; tab-bar-theme.el ends here

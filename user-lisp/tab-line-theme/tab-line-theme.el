;;; tab-line-theme.el --- Tab Line Theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

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

;; Theme for `tab-line-mode'

;;; Code:

(require 'tab-line)


(defgroup tab-line-theme
  nil
  "Tab Line Theme."
  :group 'tab-line)

(defcustom tab-line-theme-height nil
  "Height of tab line face.
When nil, height is not set."
  :type '(choice (const :tag "Unset" nil)
                 (number :tag "Height"))
  :group 'tab-line-theme)

(defcustom tab-line-theme-line-width 3
  "Line width of tab line box."
  :type 'integer
  :group 'tab-line-theme)

(defcustom tab-line-theme-tab-name-padding " "
  "String used to pad each tab name on both sides."
  :type 'string
  :group 'tab-line-theme)


(defun tab-line-theme--tab-name-format-padding (fn tab tabs)
  "Add padding around tab name via FN."
  (let ((orig tab-line-tab-name-function))
    (cl-letf (((symbol-value 'tab-line-tab-name-function)
               (lambda (buf &optional bufs)
                 (concat tab-line-theme-tab-name-padding
                         (funcall orig buf bufs)
                         tab-line-theme-tab-name-padding))))
      (funcall fn tab tabs))))

(defun tab-line-theme--box-style (line-width color)
  "Return box style for LINE-WIDTH and COLOR."
  (when (and (display-graphic-p)
             (> line-width 0))
    `(:line-width ,line-width :style nil :color ,color)))

(defun tab-line-theme--setup-base-faces ()
  "Apply base `tab-line' faces."
  (let* ((bg-inactive (face-attribute 'mode-line-inactive :background))
         (fg-inactive (face-attribute 'mode-line-inactive :foreground))
         (bg-active   (face-attribute 'default :background))
         (fg-active   (face-attribute 'default :foreground))
         (line-width  tab-line-theme-line-width))
    (custom-set-faces
     `(tab-line
       ((t ( :inherit unspecified
             ,@(when tab-line-theme-height (list :height tab-line-theme-height))
             :background ,bg-inactive
             :foreground ,fg-inactive
             :box ,(tab-line-theme--box-style line-width bg-inactive)))))
     `(tab-line-tab
       ((t ( :inherit tab-line
             :background ,bg-inactive
             :foreground ,fg-inactive
             :box ,(tab-line-theme--box-style line-width bg-inactive)))))
     `(tab-line-tab-current
       ((t ( :inherit tab-line-tab
             :background ,bg-active
             :foreground ,fg-active
             :box ,(tab-line-theme--box-style line-width bg-active)))))
     `(tab-line-tab-inactive
       ((t ( :inherit tab-line-tab
             :background ,bg-inactive
             :foreground ,fg-inactive
             :box ,(tab-line-theme--box-style line-width bg-inactive)))))
     `(tab-line-tab-group
       ((t ( :inherit tab-line-tab
             :background ,bg-inactive
             :foreground ,fg-inactive
             :weight bold
             :box nil))))
     `(tab-line-highlight
       ((t ( :inherit tab-line-tab
             :box t)))))))

(defun tab-line-theme--apply (&optional _theme)
  "Apply `tab-line-theme' settings."
  (tab-line-theme--setup-base-faces))


;;;###autoload
(define-minor-mode tab-line-theme-mode
  "Toggle `tab-line-theme-mode'."
  :global t :group 'tab-line-theme
  (cond
   (tab-line-theme-mode
    (tab-line-theme--apply)
    (add-hook 'enable-theme-functions #'tab-line-theme--apply)
    (advice-add #'tab-line-tab-name-format-default :around
                #'tab-line-theme--tab-name-format-padding))
   (t
    (remove-hook 'enable-theme-functions #'tab-line-theme--apply)
    (advice-remove #'tab-line-tab-name-format-default
                   #'tab-line-theme--tab-name-format-padding))))

(provide 'tab-line-theme)
;;; tab-line-theme.el ends here

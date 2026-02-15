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

(defface tab-group-active-1
  '((t (:inherit (outline-1 tab-bar-tab-group-current))))
  "Active face for tab group 1."
  :group 'tab-bar-theme)

(defface tab-group-active-2
  '((t (:inherit (outline-2 tab-bar-tab-group-current))))
  "Active face for tab group 2."
  :group 'tab-bar-theme)

(defface tab-group-active-3
  '((t (:inherit (outline-3 tab-bar-tab-group-current))))
  "Active face for tab group 3."
  :group 'tab-bar-theme)

(defface tab-group-active-4
  '((t (:inherit (outline-4 tab-bar-tab-group-current))))
  "Active face for tab group 4."
  :group 'tab-bar-theme)

(defface tab-group-active-5
  '((t (:inherit (outline-5 tab-bar-tab-group-current))))
  "Active face for tab group 5."
  :group 'tab-bar-theme)

(defface tab-group-active-6
  '((t (:inherit (outline-6 tab-bar-tab-group-current))))
  "Active face for tab group 6."
  :group 'tab-bar-theme)

(defface tab-group-active-7
  '((t (:inherit (outline-7 tab-bar-tab-group-current))))
  "Active face for tab group 7."
  :group 'tab-bar-theme)

(defface tab-group-active-8
  '((t (:inherit (outline-8 tab-bar-tab-group-current))))
  "Active face for tab group 8."
  :group 'tab-bar-theme)

(defface tab-group-inactive-1
  '((t (:inherit (outline-1 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 1."
  :group 'tab-bar-theme)

(defface tab-group-inactive-2
  '((t (:inherit (outline-2 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 2."
  :group 'tab-bar-theme)

(defface tab-group-inactive-3
  '((t (:inherit (outline-3 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 3."
  :group 'tab-bar-theme)

(defface tab-group-inactive-4
  '((t (:inherit (outline-4 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 4."
  :group 'tab-bar-theme)

(defface tab-group-inactive-5
  '((t (:inherit (outline-5 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 5."
  :group 'tab-bar-theme)

(defface tab-group-inactive-6
  '((t (:inherit (outline-6 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 6."
  :group 'tab-bar-theme)

(defface tab-group-inactive-7
  '((t (:inherit (outline-7 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 7."
  :group 'tab-bar-theme)

(defface tab-group-inactive-8
  '((t (:inherit (outline-8 tab-bar-tab-group-inactive))))
  "Inactive face for tab group 8."
  :group 'tab-bar-theme)


(defcustom tab-bar-theme-line-width 3
  "Line width of tab bar box."
  :type 'integer
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-colors-enabled t
  "If non-nil, render tab groups with rotating colored faces."
  :type 'boolean
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-active-faces
  '(tab-group-active-1 tab-group-active-2 tab-group-active-3 tab-group-active-4
    tab-group-active-5 tab-group-active-6 tab-group-active-7 tab-group-active-8)
  "Available active faces for `tab-group' coloring."
  :type '(repeat face)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-inactive-faces
  '(tab-group-inactive-1 tab-group-inactive-2 tab-group-inactive-3 tab-group-inactive-4
    tab-group-inactive-5 tab-group-inactive-6 tab-group-inactive-7 tab-group-inactive-8)
  "Available inactive faces for `tab-group' coloring."
  :type '(repeat face)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-current-indicator "● "
  "Indicator shown for current `tab-group' when hints are hidden."
  :group 'tab-bar-theme
  :type 'string)

(defcustom tab-bar-theme-tab-name-padding " "
  "String used to pad each tab name on both sides."
  :group 'tab-bar-theme
  :type 'string)


(defun tab-bar-theme--tab-name-format-spaces (name &optional _tab _i)
  "Pad tab label NAME with `tab-bar-theme-tab-name-padding'."
  (concat tab-bar-theme-tab-name-padding
          name
          tab-bar-theme-tab-name-padding))

(defun tab-bar-theme--setup-tab-name-format-functions ()
  "Set up `tab-bar-tab-name-format-functions' for `tab-bar-theme-mode'."
  (let* ((format-fn 'tab-bar-theme--tab-name-format-spaces)
         (functions (delq format-fn (copy-sequence tab-bar-tab-name-format-functions))))
    (setq tab-bar-tab-name-format-functions
          (let ((result nil)
                (inserted nil))
            (dolist (element functions)
              ;; insert padding before `tab-bar-tab-name-format-face'
              (when (and (not inserted)
                         (eq element 'tab-bar-tab-name-format-face))
                (push format-fn result)
                (setq inserted t))
              (push element result))
            (setq result (nreverse result))
            (unless inserted
              (message "%s not found in %s; adding at end instead"
                       'tab-bar-tab-name-format-face 'tab-bar-tab-name-format-functions)
              (setq result (append result (list format-fn))))
            result))))

(defun tab-bar-theme--setup-auto-width-faces ()
  "Sync inactive `tab-group' faces with `tab-bar-auto-width-faces'."
  (dolist (face tab-bar-theme-tab-group-inactive-faces)
    (setq tab-bar-auto-width-faces (delq face tab-bar-auto-width-faces)))
  (when tab-bar-theme-tab-group-colors-enabled
    (dolist (face tab-bar-theme-tab-group-inactive-faces)
      (add-to-list 'tab-bar-auto-width-faces face))))

(defun tab-bar-theme--setup-tab-group-format-function ()
  "Set `tab-bar-tab-group-format-function' for `tab-bar-theme'."
  (setq tab-bar-tab-group-format-function #'tab-bar-theme-tab-group-format-color))

(defun tab-bar-theme--face-at-index (faces i)
  "Return face from FACES for tab index I."
  (when faces
    (nth (mod (1- i) (length faces)) faces)))

(defun tab-bar-theme-tab-group-format-color (tab i &optional current-p)
  "Format TAB group name for tab index I and CURRENT-P state."
  (if (or (not tab-bar-theme-tab-group-colors-enabled)
          (null tab-bar-theme-tab-group-active-faces)
          (null tab-bar-theme-tab-group-inactive-faces))
      (tab-bar-tab-group-format-default tab i current-p)
    (let* ((hint (if (and tab-bar-tab-hints (not current-p))
                     (format "%d " i)
                   tab-bar-theme-tab-group-current-indicator))
           (name (funcall tab-bar-tab-group-function tab))
           (name (tab-bar-theme--tab-name-format-spaces (concat hint name)))
           (face (if current-p
                     (tab-bar-theme--face-at-index tab-bar-theme-tab-group-active-faces i)
                   (tab-bar-theme--face-at-index tab-bar-theme-tab-group-inactive-faces i))))
      (propertize name 'face face))))

(defun tab-bar-theme--box-style (line-width color)
  "Return box style for LINE-WIDTH and COLOR."
  (when (and (display-graphic-p)
             (> line-width 0))
    `(:line-width ,line-width :style nil :color ,color)))

(defun tab-bar-theme--setup-base-faces ()
  "Apply base `tab-bar' faces."
  (let* ((bg-inactive (face-attribute 'mode-line-inactive :background))
         (fg-inactive (face-attribute 'mode-line-inactive :foreground))
         (bg-active   (face-attribute 'default :background))
         (fg-active   (face-attribute 'default :foreground))
         (line-width  tab-bar-theme-line-width))
    (custom-set-faces
           `(tab-bar
             ((t ( :inherit unspecified
                   :background ,bg-inactive
                   :foreground ,fg-inactive
                   :box ,(tab-bar-theme--box-style line-width bg-inactive)))))
           `(tab-bar-tab
             ((t ( :inherit tab-bar
                   :background ,bg-active
                   :foreground ,fg-active
                   :box ,(tab-bar-theme--box-style line-width bg-active)))))
           `(tab-bar-tab-inactive
             ((t ( :inherit tab-bar-tab
                   :background ,bg-inactive
                   :foreground ,fg-inactive
                   :box ,(tab-bar-theme--box-style line-width bg-inactive)))))
           `(tab-bar-tab-ungrouped
             ((t ( :inherit tab-bar-tab-inactive
                   :background ,bg-inactive
                   :foreground ,fg-inactive
                   :box ,(tab-bar-theme--box-style line-width bg-inactive)))))
           `(tab-bar-tab-group-inactive
             ((t ( :inherit tab-bar-tab-inactive
                   :background ,bg-inactive
                   :foreground ,fg-inactive
                   :weight bold
                   :box ,(tab-bar-theme--box-style line-width bg-inactive)))))
           `(tab-bar-tab-group-current
             ((t ( :inherit tab-bar-tab
                   :background ,bg-inactive
                   :foreground ,fg-active
                   :weight bold
                   :box ,(tab-bar-theme--box-style line-width bg-inactive))))))))

(defun tab-bar-theme--apply (&optional _theme)
  "Apply `tab-bar-theme' settings."
  (tab-bar-theme--setup-tab-name-format-functions)
  (tab-bar-theme--setup-tab-group-format-function)
  (tab-bar-theme--setup-auto-width-faces)
  (tab-bar-theme--setup-base-faces))


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

;;; tab-line-theme.el --- Tab Line Theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: faces
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


(defun tab-line-theme--pad-tab-name (name)
  "Pad NAME with `tab-line-theme-tab-name-padding' on both sides."
  (concat tab-line-theme-tab-name-padding
          name
          tab-line-theme-tab-name-padding))

(defun tab-line-theme--box-style (line-width color)
  "Return box style for LINE-WIDTH and COLOR."
  (when (and (display-graphic-p)
             (> line-width 0))
    ;; `unspecified' is not a valid box color; omit :color instead (the
    ;; box then uses the face's foreground).
    `(:line-width ,line-width :style nil
      ,@(when (stringp color) (list :color color)))))

(defvar tab-line-theme--overridden-faces nil
  "Faces this mode has given an override spec, to drop again.
Recorded as they are set rather than listed by hand, so a face added to
the setup cannot be forgotten in the teardown.")

(defun tab-line-theme--set-face (face spec)
  "Give FACE the override SPEC, and record it for the teardown."
  (face-spec-set face spec)
  (push face tab-line-theme--overridden-faces))

(defun tab-line-theme--drop-faces ()
  "Drop every override spec this mode set, and forget them."
  (dolist (face tab-line-theme--overridden-faces)
    (face-spec-set face nil 'face-override-spec))
  (setq tab-line-theme--overridden-faces nil))

(defun tab-line-theme--setup-base-faces ()
  "Apply base `tab-line' faces."
  (let* ((bg-inactive (face-attribute 'mode-line-inactive :background nil 'default))
         (fg-inactive (face-attribute 'mode-line-inactive :foreground nil 'default))
         (bg-active   (face-attribute 'default :background))
         (fg-active   (face-attribute 'default :foreground))
         (box-inactive (tab-line-theme--box-style
                        tab-line-theme-line-width bg-inactive))
         (box-active (tab-line-theme--box-style
                      tab-line-theme-line-width bg-active))
         (common `( :inherit unspecified
                    ,@(when tab-line-theme-height
                        (list :height tab-line-theme-height))))
         (dim `(,@common :background ,bg-inactive
                         :foreground ,fg-inactive)))
    ;; Override specs rather than `custom-set-faces': these values are
    ;; computed from the current theme, so they have no business in a
    ;; custom file, and an override is what `tab-line-theme--drop-faces'
    ;; can drop again.
    ;;
    ;; Every value is named on every face, and no face here inherits
    ;; another of them.  doric-themes has `tab-line-tab' inherit
    ;; `tab-line-tab-current', and `face-spec-set' recalculates one face
    ;; at a time: the moment `tab-line-tab' has lost its override and
    ;; `tab-line-tab-current' still has one saying `:inherit
    ;; tab-line-tab', that is an inheritance cycle.  It signals, which
    ;; leaves the teardown half done and takes `load-theme' down with
    ;; it.
    (dolist (face '(tab-line tab-line-tab tab-line-tab-inactive))
      (tab-line-theme--set-face face `((t (,@dim :box ,box-inactive)))))
    (pcase-dolist (`(,face ,spec)
                   `((tab-line-tab-group ((t (,@dim :weight bold :box nil))))
                     (tab-line-highlight ((t (,@dim :box t))))
                     (tab-line-tab-current
                      ((t (,@common :background ,bg-active
                                    :foreground ,fg-active
                                    :box ,box-active))))))
      (tab-line-theme--set-face face spec))))

(defun tab-line-theme--apply (&optional _theme)
  "Apply `tab-line-theme' settings."
  ;; Drop what a previous pass set first: a face dropped from the setup
  ;; between two theme loads would otherwise keep its override.
  (tab-line-theme--drop-faces)
  (tab-line-theme--setup-base-faces))


;;;###autoload
(define-minor-mode tab-line-theme-mode
  "Toggle `tab-line-theme-mode'."
  :global t :group 'tab-line-theme
  (cond
   (tab-line-theme-mode
    (tab-line-theme--apply)
    (add-hook 'enable-theme-functions #'tab-line-theme--apply)
    ;; Pad the tab *name*, not the formatter: padding then sits inside the
    ;; propertized name, so the tab face's background and box cover it, and
    ;; whatever `tab-line-tab-name-format-function' is in use keeps working
    ;; (no advice stacked on `tab-line-tab-name-format-default', which
    ;; tab-line-nerd-icons also advises).
    (add-function :filter-return (var tab-line-tab-name-function)
                  #'tab-line-theme--pad-tab-name))
   (t
    (remove-hook 'enable-theme-functions #'tab-line-theme--apply)
    (remove-function (var tab-line-tab-name-function)
                     #'tab-line-theme--pad-tab-name)
    (tab-line-theme--drop-faces)))
  ;; A row already drawn lives in its window's `tab-line-cache', under a
  ;; key that says nothing about the name function or the faces, so
  ;; without this it keeps the old look until the window's tabs change
  ;; on their own.
  (tab-line-force-update t))

(provide 'tab-line-theme)
;;; tab-line-theme.el ends here

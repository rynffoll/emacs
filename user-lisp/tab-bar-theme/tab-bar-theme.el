;;; tab-bar-theme.el --- Tab Bar Theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ruslan Kamashev

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

;; Theme for `tab-bar-mode'

;;; Code:

(require 'tab-bar)
(require 'seq)
(require 'color)


(defgroup tab-bar-theme
  nil
  "Tab Bar Theme."
  :group 'tab-bar)

(defface tab-bar-theme-active-1
  '((t (:inherit outline-1)))
  "Active color 1."
  :group 'tab-bar-theme)

(defface tab-bar-theme-active-2
  '((t (:inherit outline-2)))
  "Active color 2."
  :group 'tab-bar-theme)

(defface tab-bar-theme-active-3
  '((t (:inherit outline-3)))
  "Active color 3."
  :group 'tab-bar-theme)

(defface tab-bar-theme-active-4
  '((t (:inherit outline-4)))
  "Active color 4."
  :group 'tab-bar-theme)

(defface tab-bar-theme-active-5
  '((t (:inherit outline-5)))
  "Active color 5."
  :group 'tab-bar-theme)

(defface tab-bar-theme-active-6
  '((t (:inherit outline-6)))
  "Active color 6."
  :group 'tab-bar-theme)

(defface tab-bar-theme-active-7
  '((t (:inherit outline-7)))
  "Active color 7."
  :group 'tab-bar-theme)

(defface tab-bar-theme-active-8
  '((t (:inherit outline-8)))
  "Active color 8."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-1
  '((t (:inherit outline-1)))
  "Inactive color 1."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-2
  '((t (:inherit outline-2)))
  "Inactive color 2."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-3
  '((t (:inherit outline-3)))
  "Inactive color 3."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-4
  '((t (:inherit outline-4)))
  "Inactive color 4."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-5
  '((t (:inherit outline-5)))
  "Inactive color 5."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-6
  '((t (:inherit outline-6)))
  "Inactive color 6."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-7
  '((t (:inherit outline-7)))
  "Inactive color 7."
  :group 'tab-bar-theme)

(defface tab-bar-theme-inactive-8
  '((t (:inherit outline-8)))
  "Inactive color 8."
  :group 'tab-bar-theme)


(defcustom tab-bar-theme-height nil
  "Height of tab bar face.
When nil, height is not set."
  :type '(choice (const :tag "Unset" nil)
                 (number :tag "Height"))
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-line-width 3
  "Line width of tab bar box."
  :type 'integer
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-inactive-background 30
  "Background for an inactive tab: nil, a shift percent, or a color.

At its default background, an inactive tab's own shade is identical
to the tab-bar strip itself (visible behind the tab-bar menu icon and
in the empty tail after the last tab), so an inactive tab —
particularly the last one, right against that empty tail — reads as a
continuation of the strip rather than a tab of its own.

An integer nudges that shared background toward the active tab's by
that percent, via `tab-bar-theme--shift-color'; a pale theme needs a
much bigger shift than a saturated one to read as a different color
at all, so the default is tuned by eye and worth adjusting per theme.
A color string is used as is.  nil matches the strip, as before this
option existed."
  :type '(choice (const :tag "Match the strip" nil)
                 (integer :tag "Shift percent toward the active tab")
                 color)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-colors 'groups
  "What to render with the rotating colored faces.

`groups'  each tab group's label, colored by the group's position
`tabs'    each tab itself, colored by the tab's own position — for a
          workflow of one tab per project and no groups
t         both
nil       neither

Both draw from the same `tab-bar-theme-active-faces' and
`tab-bar-theme-inactive-faces'."
  :type '(choice (const :tag "Tab groups" groups)
                 (const :tag "Tabs" tabs)
                 (const :tag "Both" t)
                 (const :tag "Neither" nil))
  :group 'tab-bar-theme)

(defun tab-bar-theme--tab-colors-p (what)
  "Return non-nil when WHAT, `tabs' or `groups', is to be colored."
  (or (eq tab-bar-theme-tab-colors t)
      (eq tab-bar-theme-tab-colors what)))

(defcustom tab-bar-theme-active-faces
  '( tab-bar-theme-active-1 tab-bar-theme-active-2 tab-bar-theme-active-3 tab-bar-theme-active-4
     tab-bar-theme-active-5 tab-bar-theme-active-6 tab-bar-theme-active-7 tab-bar-theme-active-8)
  "Available active faces for the rotating coloring."
  :type '(repeat face)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-inactive-faces
  '( tab-bar-theme-inactive-1 tab-bar-theme-inactive-2 tab-bar-theme-inactive-3 tab-bar-theme-inactive-4
     tab-bar-theme-inactive-5 tab-bar-theme-inactive-6 tab-bar-theme-inactive-7 tab-bar-theme-inactive-8)
  "Available inactive faces for the rotating coloring."
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

(defcustom tab-bar-theme-tab-extra-face-function nil
  "Function returning one more face to give a tab, or nil for none.
Called with a single TAB, in the manner of `tab-bar-tab-face-function'.
The face is merged ahead of the rotating color and the base tab face,
so it has the last word on whatever attributes it sets.

What earns a tab a face of its own is none of a theme's business, so
it is asked rather than decided here.  To embolden the tabs bound to a
project, for instance:

    (setq tab-bar-theme-tab-extra-face-function
          (lambda (tab) (when (otpp-get-tab-root-dir tab) \\='bold)))"
  :type '(choice function (const nil))
  :group 'tab-bar-theme)


(defun tab-bar-theme--tab-name-format-spaces (name &optional _tab _i)
  "Pad tab label NAME with `tab-bar-theme-tab-name-padding'."
  (concat tab-bar-theme-tab-name-padding
          name
          tab-bar-theme-tab-name-padding))

(defun tab-bar-theme--setup-tab-name-format-functions ()
  "Set up `tab-bar-tab-name-format-functions' for `tab-bar-theme-mode'.
Pad before `tab-bar-tab-name-format-face', so the face's background and
box cover the padding too.  Padding is the only step that belongs
here at all — it rewrites the label; the coloring is a face, and so
lives in `tab-bar-theme--tab-face' instead."
  (let* ((format-fn 'tab-bar-theme--tab-name-format-spaces)
         (fns (remq format-fn tab-bar-tab-name-format-functions))
         (i (seq-position fns 'tab-bar-tab-name-format-face)))
    (unless i
      (message "%s not found in %s; adding at end instead"
               'tab-bar-tab-name-format-face 'tab-bar-tab-name-format-functions))
    (setq tab-bar-tab-name-format-functions
          (if i
              (append (take i fns) (list format-fn) (nthcdr i fns))
            (append fns (list format-fn))))))

(defun tab-bar-theme--setup-auto-width-faces ()
  "Sync inactive `tab-group' faces with `tab-bar-auto-width-faces'."
  (setq tab-bar-auto-width-faces
        (seq-difference tab-bar-auto-width-faces
                        tab-bar-theme-inactive-faces))
  (when (tab-bar-theme--tab-colors-p 'groups)
    (setq tab-bar-auto-width-faces
          (seq-union tab-bar-auto-width-faces
                     tab-bar-theme-inactive-faces))))

(defun tab-bar-theme--face-at-index (faces i)
  "Return face from FACES for tab index I."
  (when faces
    (nth (mod (1- i) (length faces)) faces)))

(defun tab-bar-theme--layer (over base)
  "Return a face spec applying every face in OVER ahead of BASE.
Each entry in OVER wins its own attributes; BASE supplies whatever
they leave unset."
  (append over (list base)))

(defun tab-bar-theme-tab-group-format-color (tab i &optional current-p)
  "Format TAB group name for tab index I and CURRENT-P state.
The rotation face only carries a foreground (see the `active'/
`inactive' faces' docstrings); the weight and background that make a
group label look like one still have to come from the core group
face, layered in behind it here rather than baked into the rotation
face itself, which has no opinion of its own on \"group\"."
  (if (or (not (tab-bar-theme--tab-colors-p 'groups))
          (null tab-bar-theme-active-faces)
          (null tab-bar-theme-inactive-faces))
      (tab-bar-tab-group-format-default tab i current-p)
    (let* ((hint (if (and tab-bar-tab-hints (not current-p))
                     (format "%d " i)
                   tab-bar-theme-tab-group-current-indicator))
           (name (funcall tab-bar-tab-group-function tab))
           (name (tab-bar-theme--tab-name-format-spaces (concat hint name)))
           (rotation-face (if current-p
                              (tab-bar-theme--face-at-index tab-bar-theme-active-faces i)
                            (tab-bar-theme--face-at-index tab-bar-theme-inactive-faces i)))
           (base-face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))
      (propertize name 'face (tab-bar-theme--layer (list rotation-face) base-face)))))

(defun tab-bar-theme--tab-face (tab)
  "Return the face for TAB, layered over the stock tab face.
Mirrors `tab-bar-theme-tab-group-format-color', but keys off the tab
itself rather than its group, for a groupless one-tab-per-project
workflow, and adds the extra face on top.

Installed as `tab-bar-tab-face-function' rather than as another entry
in `tab-bar-tab-name-format-functions': `tab-bar' appends the returned
face, so our styling lands ahead of the base tab face yet behind any
face something else already gave part of the label — an attention
marker, say, whose own color has to survive.  Only the foreground is
taken from the rotating faces; the base face keeps supplying the
active/inactive background and box.  That base comes from
`tab-bar-tab-group-face-default', the one default correct in both of
the slots this is installed in."
  (let ((over ()))
    (when-let* ((tab-bar-theme-tab-extra-face-function)
                (face (funcall tab-bar-theme-tab-extra-face-function tab)))
      (push face over))
    (when-let* (((tab-bar-theme--tab-colors-p 'tabs))
                (index (tab-bar--tab-index tab))
                (faces (if (eq (car tab) 'current-tab)
                           tab-bar-theme-active-faces
                         tab-bar-theme-inactive-faces))
                (fg (face-attribute (tab-bar-theme--face-at-index faces (1+ index))
                                    :foreground nil t)))
      (push (list :foreground fg) over))
    (tab-bar-theme--layer over (tab-bar-tab-group-face-default tab))))

(defun tab-bar-theme--shift-color (color percent)
  "Shift COLOR by PERCENT, toward content when negative, away when positive.
\"Toward content\" means toward what an active element's background
looks like on the current frame; \"away\" means further into chrome.
Lightening is toward content on a light background but away from it
on a dark one — and vice versa for darkening — so a fixed sign keeps
its meaning across both, rather than one of them flooring an
already-dark or already-light color into no contrast at all."
  (let ((dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (if (eq (< percent 0) dark)
        (color-darken-name color (abs percent))
      (color-lighten-name color (abs percent)))))

(defun tab-bar-theme--box-style (line-width color)
  "Return box style for LINE-WIDTH and COLOR."
  (when (and (display-graphic-p)
             (> line-width 0))
    ;; `unspecified' is not a valid box color and would poison the face
    ;; spec saved by `custom-set-faces'; omit :color instead (the box
    ;; then uses the face's foreground).
    `(:line-width ,line-width :style nil
      ,@(when (stringp color) (list :color color)))))

(defun tab-bar-theme--setup-base-faces ()
  "Apply base `tab-bar' faces."
  (let* ((bg-inactive (face-attribute 'mode-line-inactive :background nil 'default))
         (fg-inactive (face-attribute 'mode-line-inactive :foreground nil 'default))
         (bg-active   (face-attribute 'default :background))
         (fg-active   (face-attribute 'default :foreground))
         (bg-tab-inactive (pcase tab-bar-theme-tab-inactive-background
                            ((and (pred integerp) shift)
                             (tab-bar-theme--shift-color bg-inactive (- shift)))
                            ((and (pred stringp) color) color)
                            (_ bg-inactive)))
         (line-width  tab-bar-theme-line-width)
         (box-inactive (tab-bar-theme--box-style line-width bg-inactive))
         (box-active   (tab-bar-theme--box-style line-width bg-active))
         (box-tab-inactive (tab-bar-theme--box-style line-width bg-tab-inactive)))
    (custom-set-faces
     `(tab-bar
       ((t ( :inherit unspecified
             ,@(when tab-bar-theme-height (list :height tab-bar-theme-height))
             :background ,bg-inactive
             :foreground ,fg-inactive
             :box ,box-inactive))))
     `(tab-bar-tab
       ((t ( :inherit tab-bar
             :background ,bg-active
             :foreground ,fg-active
             :box ,box-active))))
     `(tab-bar-tab-inactive
       ((t ( :inherit tab-bar-tab
             :background ,bg-tab-inactive
             :foreground ,fg-inactive
             :box ,box-tab-inactive))))
     `(tab-bar-tab-ungrouped
       ((t ( :inherit tab-bar-tab-inactive
             :background ,bg-tab-inactive
             :foreground ,fg-inactive
             :box ,box-tab-inactive))))
     `(tab-bar-tab-group-inactive
       ((t ( :inherit tab-bar-tab-inactive
             :background ,bg-tab-inactive
             :foreground ,fg-inactive
             :weight bold
             :box ,box-tab-inactive))))
     `(tab-bar-tab-group-current
       ((t ( :inherit tab-bar-tab
             :background ,bg-inactive
             :foreground ,fg-active
             :weight bold
             :box ,box-inactive)))))))

(defun tab-bar-theme--apply (&optional _theme)
  "Apply `tab-bar-theme' settings."
  (tab-bar-theme--setup-tab-name-format-functions)
  (setq tab-bar-tab-face-function #'tab-bar-theme--tab-face)
  ;; `tab-bar-format-tabs-groups' renders every tab with
  ;; `tab-bar-tab-group-face-function' shadowing the above, so both
  ;; slots have to name the same function to style tabs either way —
  ;; the usage `tab-bar-tab-group-face-function' documents itself.
  (setq tab-bar-tab-group-face-function #'tab-bar-theme--tab-face)
  (setq tab-bar-tab-group-format-function #'tab-bar-theme-tab-group-format-color)
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

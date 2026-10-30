;;; tab-bar-theme.el --- Faces and colors for the tab bar -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
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

;; Faces and colors for the tab bar, derived from the current theme.
;; `tab-bar-theme-mode' sets the tab bar faces on every theme change,
;; colors tab groups or tabs from a palette, pads tab names and builds
;; group labels.

;;; Code:

(require 'tab-bar)
(require 'color)


(defgroup tab-bar-theme nil
  "Faces and colors for the tab bar."
  :group 'tab-bar
  :prefix "tab-bar-theme-")

;; Rotation colors inherit from font-lock faces, not `outline-N':
;; font-lock is preloaded, while outline.el is loaded on demand, so
;; `outline-N' may have no foreground yet.
;;
;; These faces have a color of their own in nearly every theme; the
;; other font-lock faces inherit from them or use the default
;; foreground.  The order keeps neighbours far apart, since the row is
;; read left to right and wraps.  Most themes have fewer distinguishable
;; font-lock colors than slots, so the last slots reduce collisions
;; rather than add colors.
;;
;; One face per slot: the non-current variant is the same color dimmed,
;; derived in `tab-bar-theme--face-pair'.

(defface tab-bar-theme-color-1
  '((t (:inherit font-lock-string-face)))
  "Rotation color 1."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-2
  '((t (:inherit font-lock-negation-char-face)))
  "Rotation color 2."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-3
  '((t (:inherit font-lock-constant-face)))
  "Rotation color 3."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-4
  '((t (:inherit font-lock-function-name-face)))
  "Rotation color 4."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-5
  '((t (:inherit font-lock-warning-face)))
  "Rotation color 5."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-6
  '((t (:inherit font-lock-variable-name-face)))
  "Rotation color 6."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-7
  '((t (:inherit font-lock-builtin-face)))
  "Rotation color 7."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-8
  '((t (:inherit font-lock-type-face)))
  "Rotation color 8."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-9
  '((t (:inherit font-lock-escape-face)))
  "Rotation color 9."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-10
  '((t (:inherit font-lock-doc-markup-face)))
  "Rotation color 10."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-11
  '((t (:inherit font-lock-property-name-face)))
  "Rotation color 11."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-12
  '((t (:inherit font-lock-comment-face)))
  "Rotation color 12."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-13
  '((t (:inherit font-lock-preprocessor-face)))
  "Rotation color 13."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-14
  '((t (:inherit font-lock-variable-use-face)))
  "Rotation color 14."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-15
  '((t (:inherit font-lock-doc-face)))
  "Rotation color 15."
  :group 'tab-bar-theme)

(defface tab-bar-theme-color-16
  '((t (:inherit font-lock-keyword-face)))
  "Rotation color 16."
  :group 'tab-bar-theme)

(defun tab-bar-theme--set-and-apply (symbol value)
  "Set SYMBOL to VALUE; reapply the theme if `tab-bar-theme-mode' is on.
For the `:set' of options read when the theme is applied."
  (set-default symbol value)
  (when (bound-and-true-p tab-bar-theme-mode)
    (tab-bar-theme--apply)))

(defcustom tab-bar-theme-height nil
  "Height of the tab bar faces, or nil to leave it unset."
  :type '(choice (const :tag "Unset" nil)
                 (number :tag "Height"))
  :set #'tab-bar-theme--set-and-apply
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-inactive-background-shift 0.3
  "Shift of the non-current tab background off the tab bar background.
Unshifted, non-current tabs share the background of the tab bar strip,
so the last one merges with the empty space after it.

A number shifts that background toward the current tab's by that
fraction of its lightness, via `tab-bar-theme--shift-color'.  Pale
themes need a larger shift than saturated ones.  A string is a color
used as is.  nil keeps the strip background."
  :type '(choice (const :tag "Match the strip" nil)
                 (float :tag "Shift fraction toward the active tab")
                 color)
  :set #'tab-bar-theme--set-and-apply
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-background-shift nil
  "Shift of the current group label background off the current tab's.
Unshifted, the label and the current tab read as one block.

A number shifts the label background by that fraction of its lightness,
via `tab-bar-theme--shift-color': negative is toward the tab bar,
positive toward content.  A string is a color used as is.  nil keeps the
current tab background."
  :type '(choice (const :tag "Match the active tab" nil)
                 (float :tag "Shift fraction off the active tab")
                 color)
  :set #'tab-bar-theme--set-and-apply
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-colors 'groups
  "What to color with `tab-bar-theme-color-faces'.
`groups' colors each tab group label, `tabs' each tab (for one tab per
project without groups), t both, and nil neither."
  :type '(choice (const :tag "Tab groups" groups)
                 (const :tag "Tabs" tabs)
                 (const :tag "Both" t)
                 (const :tag "Neither" nil))
  :group 'tab-bar-theme)

(defun tab-bar-theme--tab-colors-p (what)
  "Return non-nil when WHAT, `tabs' or `groups', is to be colored."
  (or (eq tab-bar-theme-tab-colors t)
      (eq tab-bar-theme-tab-colors what)))

(defcustom tab-bar-theme-color-faces
  '( tab-bar-theme-color-1  tab-bar-theme-color-2
     tab-bar-theme-color-3  tab-bar-theme-color-4
     tab-bar-theme-color-5  tab-bar-theme-color-6
     tab-bar-theme-color-7  tab-bar-theme-color-8
     tab-bar-theme-color-9  tab-bar-theme-color-10
     tab-bar-theme-color-11 tab-bar-theme-color-12
     tab-bar-theme-color-13 tab-bar-theme-color-14
     tab-bar-theme-color-15 tab-bar-theme-color-16)
  "Faces a tab can be colored with, one per slot.
Only the foreground is used.  A non-current tab uses the same color
dimmed per `tab-bar-theme-tab-inactive-blend'.  A tab gets a face by
hand via \\[tab-bar-theme-set-tab-color], from
`tab-bar-theme-tab-color-face-function', by
`tab-bar-theme-tab-color-key-function' or by its position in the row."
  :type '(repeat face)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-color-face-function nil
  "Function returning the face to color a tab with, or nil.
Called with one argument, the tab.  If it returns nil, the tab takes its
group's color, if any.  If this is nil, tabs are colored by
`tab-bar-theme-tab-color-key-function' or by position.  Takes effect
only when `tab-bar-theme-tab-colors' includes tabs.

A face set by \\[tab-bar-theme-set-tab-color] takes precedence.  To
color tabs by a key, such as a directory, use
`tab-bar-theme-tab-color-key-function' instead."
  :type '(choice (const :tag "None — color by position in the row" nil)
                 function)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-color-key-function nil
  "Function returning the key to color a tab by, or nil.
Called with one argument, TAB.  `tab-bar-theme-color-from-key' maps the
key, e.g. a project directory, to a face.  Used if
`tab-bar-theme-tab-color-face-function' is nil, instead of coloring by
position."
  :type '(choice (const :tag "None" nil) function)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-inactive-blend 0.55
  "How much of its color a non-current tab keeps.
A number is the fraction of the way from the tab background toward the
full color: the tab color for the label, the `tab-bar-tab-inactive'
foreground for the indicator bar.  The bar applies the fraction twice,
once for the non-current tab and once for being a bar rather than a
label.  The current tab's bar uses `tab-bar-theme-indicator-blend'.

A string or a face is used as is for both; nil disables dimming.  See
`tab-bar-theme--resolve-color'."
  :type '(choice (const :tag "No dimming" nil)
                 (float :tag "Fraction of the way from the tab's background")
                 color face)
  :set #'tab-bar-theme--set-and-apply
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-name-padding " "
  "String used to pad each tab name on both sides."
  :type 'string
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-separator " "
  "Space between the elements of `tab-bar-format'.
A string is used as is.  A number is a width in pixels, finer than whole
characters.  Terminal frames always get one space, since they cannot
draw part of a cell; that fallback and pixel widths are what this adds
over a propertized string in `tab-bar-format'.

This is not the variable `tab-bar-separator', which precedes every tab.
All tabs come from one format element, so the space between tabs is that
variable or `tab-bar-theme-tab-name-padding'."
  :type '(choice (string :tag "String")
                 (integer :tag "Pixels"))
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-extra-face-function nil
  "Function returning an extra face for a tab, or nil.
Called with one argument, TAB, like `tab-bar-tab-face-function'.  The
face is merged ahead of the base tab face, so its attributes win, except
the foreground, which the tab color keeps.  For example, to embolden
tabs bound to a project:

    (setq tab-bar-theme-tab-extra-face-function
          (lambda (tab) (when (my-tab-project tab) \\='bold)))"
  :type '(choice (const :tag "None" nil) function)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-format-functions
  '(tab-bar-theme-tab-group-format-padding
    tab-bar-theme-tab-group-format-face)
  "Functions that build a tab group's label.
Each is called with four arguments, NAME, TAB, I and CURRENT-P, and
returns the new NAME, like `tab-bar-tab-name-format-functions' for a
tab.  NAME starts as the hint and group name, without a face.
`tab-bar-theme-mode' sets `tab-bar-tab-group-format-function' to
`tab-bar-theme-tab-group-format', which runs these.

Other packages provide formatters too, e.g.
`tab-bar-status-tab-group-format' and `tab-bar-indicator-tab-group-format'.
Put `tab-bar-theme-tab-group-format-face' last, so its face goes under
all parts."
  :type 'hook
  :options '(tab-bar-theme-tab-group-format-padding
             tab-bar-theme-tab-group-format-fold
             tab-bar-theme-tab-group-format-face)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-indicator-blend 0.8
  "Color of the indicator bar on the current tab.
A number is the fraction of the way from the tab background toward the
tab color; a string or a face is used as is; nil is the tab color
itself.  Mixing from the tab's own colors suits both light and dark
themes, unlike a fixed color.  See `tab-bar-theme--resolve-color'."
  :type '(choice (const :tag "The color itself" nil)
                 (float :tag "Fraction of the way from the tab's background")
                 color face)
  :set #'tab-bar-theme--set-and-apply
  :group 'tab-bar-theme)

;;;###autoload
(defun tab-bar-theme-separator ()
  "Return `tab-bar-theme-separator' as a string for `tab-bar-format'.
Like the function `tab-bar-separator', it shares its name with a
variable."
  (pcase tab-bar-theme-separator
    ((and (pred numberp) px)
     ;; A width spec in the format can leave the whole row unpainted on
     ;; a terminal (reported by modern-tab, Emacs 30.2 under tmux).
     (if (display-graphic-p)
         (propertize " " 'display `(space :width (,px)))
       " "))
    ((and (pred stringp) str) str)
    (_ " ")))

;; Defined by the mode at the end of this file.
(defvar tab-bar-theme-mode)

;;;###autoload
(defun tab-bar-theme-tab-name-format-padding (name &optional _tab _i)
  "Pad tab label NAME with `tab-bar-theme-tab-name-padding'.
A no-op when `tab-bar-theme-mode' is off, so it can stay in
`tab-bar-tab-name-format-functions' unconditionally.  Put it before
`tab-bar-tab-name-format-face', so the tab face covers the padding, and
before `tab-bar-indicator-tab-name-format', so the bar is at the tab's
left edge."
  (if tab-bar-theme-mode
      (concat tab-bar-theme-tab-name-padding
              name
              tab-bar-theme-tab-name-padding)
    name))

(defun tab-bar-theme--at-slot (sequence i)
  "Return the element of SEQUENCE for 1-based tab number I, wrapping around."
  (let ((n (length sequence)))
    (when (> n 0)
      (elt sequence (mod (1- i) n)))))

(defun tab-bar-theme--layer (over base)
  "Return a face list applying the faces in OVER ahead of BASE.
Each face in OVER wins its own attributes; BASE supplies the rest."
  (append over (list base)))

(defun tab-bar-theme--tab-group-face (&optional current-p)
  "Return the base face of a group label in CURRENT-P state.
Used by `tab-bar-theme-tab-group-format-face'."
  (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive))

;;;###autoload
(defun tab-bar-theme-tab-group-format (tab i &optional current-p)
  "Return the label of TAB's group for tab index I and CURRENT-P state.
Start from `tab-bar-tab-group-format-default' without its face, so core
decides the hint, and run `tab-bar-theme-tab-group-format-functions'."
  (let ((name (substring-no-properties
               (tab-bar-tab-group-format-default tab i current-p))))
    (run-hook-wrapped 'tab-bar-theme-tab-group-format-functions
                      (lambda (fun)
                        (setq name (funcall fun name tab i current-p))
                        nil))
    name))

(defun tab-bar-theme-tab-group-format-padding (name _tab _i &optional _current-p)
  "Pad group label NAME with `tab-bar-theme-tab-name-padding'."
  (tab-bar-theme-tab-name-format-padding name))

(defun tab-bar-theme-tab-group-format-fold (name _tab _i &optional current-p)
  "Prefix group label NAME with an open or folded marker.
A group is open if CURRENT-P or `tab-bar-show-inactive-group-tabs'."
  (concat (propertize (concat
                       ;; Graphic frames only: there the preceding
                       ;; indicator is a few pixels wide.
                       (and (display-graphic-p) tab-bar-theme-tab-name-padding)
                       (if (or current-p tab-bar-show-inactive-group-tabs) "⌵" "›"))
                      'face 'shadow)
          name))

(defun tab-bar-theme-tab-group-format-face (name tab i &optional current-p)
  "Add the color and face of TAB's group under group label NAME.
Appended, so faces already in NAME, e.g. a mark's, take precedence."
  (let ((name (copy-sequence name))
        ;; (:foreground COLOR), or nil for an uncolored group.
        (spec (and (tab-bar-theme--tab-colors-p 'groups)
                   tab-bar-theme-color-faces
                   (cdr (tab-bar-theme--tab-pair tab current-p i 'groups)))))
    (when spec
      (add-face-text-property 0 (length name) spec t name))
    (add-face-text-property 0 (length name)
                            (tab-bar-theme--tab-group-face current-p) t name)
    name))

(defun tab-bar-theme--color-from-tab (tab)
  "Return the face set on TAB by \\[tab-bar-theme-set-tab-color], or nil.
Stored as a tab parameter, so it lives as long as the tab."
  (alist-get 'tab-bar-theme-color tab))

(defvar tab-bar-theme--key-face-cache (make-hash-table :test #'equal)
  "Cache of `tab-bar-theme-color-from-key' results.
Keyed on (KEY . LENGTH), LENGTH being that of
`tab-bar-theme-color-faces'; nothing depends on the theme, so the cache
survives theme loads.")

(defun tab-bar-theme-color-from-key (key)
  "Return the face for KEY, or nil if KEY is nil.
KEY is a string naming the colored thing for its lifetime, e.g. a
project directory or a tab group name.  The face is chosen by hash, so a
key keeps its color across sessions, at the cost of occasional
collisions while other slots are free.  `secure-hash' is used because
`sxhash-equal' values are not stable across Emacs versions."
  (when (and key tab-bar-theme-color-faces)
    (with-memoization (gethash (cons key (length tab-bar-theme-color-faces))
                               tab-bar-theme--key-face-cache)
      ;; `--at-slot' wraps, so no `mod' here.
      (tab-bar-theme--at-slot
       tab-bar-theme-color-faces
       (1+ (string-to-number (substring (secure-hash 'md5 key) 0 8) 16))))))

(defvar tab-bar-theme--last-color nil
  "Last result of `tab-bar-theme--tab-color-face', as (TAB I WHAT FACE).
Each tab is queried twice in a row, for its label and for its bar.")

(defun tab-bar-theme--tab-color-face (tab i what)
  "Return the face TAB is colored with, or nil.
I is its position in the row and WHAT the `tab-bar-theme-tab-colors'
axis being drawn.  For tabs, the precedence is: the face set by hand,
then `tab-bar-theme-tab-color-face-function', the key function or the
position, then the color of TAB's group.  Groups use only their name's
color."
  (let ((last tab-bar-theme--last-color))
    (if (and (eq tab (nth 0 last))
             (eql i (nth 1 last))
             (eq what (nth 2 last)))
        (nth 3 last)
      (let ((face (and (tab-bar-theme--tab-colors-p what)
                       (or (and (eq what 'tabs)
                                (or (tab-bar-theme--color-from-tab tab)
                                    (cond
                                     (tab-bar-theme-tab-color-face-function
                                      (funcall tab-bar-theme-tab-color-face-function tab))
                                     (tab-bar-theme-tab-color-key-function
                                      (tab-bar-theme-color-from-key
                                       (funcall tab-bar-theme-tab-color-key-function tab)))
                                     (t (tab-bar-theme--at-slot
                                         tab-bar-theme-color-faces (or i 1))))))
                           ;; Groups, and tabs nothing above names, take
                           ;; the group's color.  By name, not I: I is the
                           ;; index of the group's first tab and changes
                           ;; when a tab is opened to its left.
                           (tab-bar-theme-color-from-key
                            (funcall tab-bar-tab-group-function tab))))))
        (setq tab-bar-theme--last-color (list tab i what face))
        face))))

(defun tab-bar-theme--tab-face (tab)
  "Return the face for TAB, layered over the stock tab face.
Like `tab-bar-theme-tab-group-format-face', but keyed on the tab itself,
plus the face from `tab-bar-theme-tab-extra-face-function'.

Used as `tab-bar-tab-face-function' rather than in
`tab-bar-tab-name-format-functions' because `tab-bar' appends this
face: it goes ahead of the base tab face but behind faces already on
the label, e.g. a mark's.  Only the foreground comes from
the tab color; the base face supplies the background.  The base is
`tab-bar-tab-group-face-default', which is correct both here and as
`tab-bar-tab-group-face-function'."
  (let ((over ()))
    (when-let* ((tab-bar-theme-tab-extra-face-function)
                (face (funcall tab-bar-theme-tab-extra-face-function tab)))
      (push face over))
    ;; Pushed second, so the tab's own color wins the foreground.
    (when-let* ((face (tab-bar-theme--tab-color-spec tab)))
      (push face over))
    (tab-bar-theme--layer over (tab-bar-tab-group-face-default tab))))

(defun tab-bar-theme--tab-color-spec (tab)
  "Return the label spec of TAB's own color, or nil.
Take the slot from `tab-bar-theme--last-color' when it holds TAB, to
avoid `tab-bar--tab-index', which walks the whole tab list.  It usually
does: the indicator formatter runs just before the face formatter in
`tab-bar-tab-name-format-functions'."
  (let* ((last tab-bar-theme--last-color)
         (current (eq (car tab) 'current-tab))
         (i (if (and (eq tab (nth 0 last)) (eq 'tabs (nth 2 last)))
                (nth 1 last)
              (when-let* ((index (tab-bar--tab-index tab)))
                (1+ index)))))
    (when i
      (cdr (tab-bar-theme--tab-pair tab current i 'tabs)))))

;;;###autoload
(defun tab-bar-theme-set-tab-color (face)
  "Color the current tab with FACE for the tab's lifetime.
FACE takes precedence over `tab-bar-theme-tab-color-face-function'.
Empty input removes the color."
  (interactive
   (list (let ((name (completing-read "Face for this tab (empty for none): "
                                      tab-bar-theme-color-faces nil t)))
           (unless (string-empty-p name) (intern name)))))
  ;; The tab itself, not the copy `tab-bar--current-tab' returns.
  (setf (alist-get 'tab-bar-theme-color
                   (cdr (assq 'current-tab (funcall tab-bar-tabs-function)))
                   nil :remove)
        face)
  (setq tab-bar-theme--last-color nil)
  (force-mode-line-update t))

(defvar tab-bar-theme--pair-cache (make-hash-table :test #'eq)
  "Cache of (CURRENT-PAIR . OTHER-PAIR) per face seen on a tab.
Each pair is (COLOR . FACE): the indicator bar color and the label face
spec.  The nil key holds the pair of an uncolored tab.")

(defun tab-bar-theme--inactive-spec ()
  "Return the blend for a non-current indicator bar.
A numeric `tab-bar-theme-tab-inactive-blend' is applied twice."
  (if (numberp tab-bar-theme-tab-inactive-blend)
      (* tab-bar-theme-tab-inactive-blend
         tab-bar-theme-tab-inactive-blend)
    tab-bar-theme-tab-inactive-blend))

(defvar tab-bar-theme--quiet-cache (make-hash-table :test #'eq)
  "Cache of `tab-bar-theme--quiet-color' per face.
Cleared along with the pairs by `tab-bar-theme--drop-pairs'.")

(defun tab-bar-theme--quiet-color (face)
  "Return the muted indicator color for FACE.
Mixed from FACE's own background and foreground, so the contrast fits
the surface it is drawn on.  Not `tab-bar-theme--build-pair' with no
color: without a blend that falls back to the tab color rather than the
label foreground, and it reads only the two tab faces."
  (with-memoization (gethash face tab-bar-theme--quiet-cache)
    (let ((bg (face-attribute face :background nil t))
          (fg (face-attribute face :foreground nil t))
          (spec (tab-bar-theme--inactive-spec)))
      (if spec (tab-bar-theme--resolve-color spec bg fg) fg))))

(defun tab-bar-theme--build-pair (current raw)
  "Return the (COLOR . FACE) pair of a tab in state CURRENT with color RAW.
COLOR is the indicator bar color, see
`tab-bar-theme-tab-indicator-color'; FACE is the label face spec."
  (let* ((face (if current 'tab-bar-tab 'tab-bar-tab-inactive))
         (spec (if current
                   tab-bar-theme-indicator-blend
                 (tab-bar-theme--inactive-spec)))
         (bg (face-attribute face :background nil t))
         (fg (face-attribute face :foreground nil t))
         ;; A non-current tab takes RAW dimmed against its background.
         ;; A nil blend makes `--resolve-color' return RAW unchanged.
         (rot (if current
                  raw
                (and raw (tab-bar-theme--resolve-color
                          tab-bar-theme-tab-inactive-blend bg raw)))))
    (cons
     ;; The current tab's bar mixes toward the tab color.  Non-current
     ;; bars all mix toward the label foreground, one shared tone: the
     ;; label already tells tabs apart.
     (if spec
         (tab-bar-theme--resolve-color spec bg (if current (or rot fg) fg))
       (or rot fg))
     (and (color-defined-p rot) (list :foreground rot)))))

(defun tab-bar-theme--face-color (face)
  "Return FACE's foreground, or nil where it names no color."
  (when-let* ((face)
              (color (face-foreground face nil t))
              ((color-defined-p color)))
    color))

(defun tab-bar-theme--drop-pairs ()
  "Forget every pair derived from the theme's colors."
  (clrhash tab-bar-theme--pair-cache)
  (clrhash tab-bar-theme--quiet-cache)
  (setq tab-bar-theme--last-color nil))

(defun tab-bar-theme--face-pair (face)
  "Return the (CURRENT-PAIR . OTHER-PAIR) for tabs colored with FACE.
A nil FACE gives the pair of an uncolored tab.

Built on first use of each face and cached outside redisplay: faces of
tabs opened later are unknown up front, and computing pairs per tab on
each redisplay is costly."
  (with-memoization (gethash face tab-bar-theme--pair-cache)
    (let ((raw (tab-bar-theme--face-color face)))
      (cons (tab-bar-theme--build-pair t raw)
            (tab-bar-theme--build-pair nil raw)))))

(defun tab-bar-theme--tab-pair (tab current i what)
  "Return the (COLOR . FACE) pair for TAB, in state CURRENT at slot I.
WHAT is the `tab-bar-theme-tab-colors' axis being drawn.  An uncolored
TAB gets the muted bar color and no label face, leaving the label to the
theme."
  (let ((pair (tab-bar-theme--face-pair
               (tab-bar-theme--tab-color-face tab i what))))
    (if current (car pair) (cdr pair))))

;;;###autoload
(defun tab-bar-theme-tab-indicator-color (tab i)
  "Return the indicator bar color for TAB at slot I, or nil.
Mixed like the tab's label, so the two match.  A nil TAB, as passed by
`tab-bar-indicator-separator', gets the muted tone of an uncolored tab
without reading tab parameters or calling tab color functions.

Meant for `tab-bar-indicator-color-function':

    (setq tab-bar-indicator-color-function #\\='tab-bar-theme-tab-indicator-color)"
  (if (not tab)
      (tab-bar-theme--quiet-color 'tab-bar-tab-inactive)
    (car (tab-bar-theme--tab-pair tab (eq (car tab) 'current-tab) i 'tabs))))

;;;###autoload
(defun tab-bar-theme-tab-group-indicator-color (_tab _i &optional current-p)
  "Return the color of a group's indicator in CURRENT-P state.
A muted tone of the group face; the group's own color is on its label.
For `tab-bar-indicator-group-color-function'."
  (tab-bar-theme--quiet-color (tab-bar-theme--tab-group-face current-p)))

(defun tab-bar-theme--shift-color (color fraction)
  "Shift the lightness of COLOR by FRACTION, keeping its hue.
Positive moves toward the default background, darker on a dark frame
and lighter on a light one; negative moves away from it.  The sign thus
means the same on light and dark themes."
  ;; Not a color, see `tab-bar-theme--blend-color': return it unshifted.
  (if (not (color-name-to-rgb color))
      color
    (let ((dark (eq (frame-parameter nil 'background-mode) 'dark))
          ;; `color-darken-name' and `color-lighten-name' take a percent.
          (percent (* 100 (abs fraction))))
      (if (eq (> fraction 0) dark)
          (color-darken-name color percent)
        (color-lighten-name color percent)))))

(defun tab-bar-theme--blend-color (from to fraction)
  "Return the color FRACTION of the way from FROM to TO, or nil.
Unlike `tab-bar-theme--shift-color', it takes on TO's hue: for mixing a
label or bar color toward a foreground, where a lightness shift of a
warm background turns muddy."
  ;; Nil if either end is not a color this frame knows; see
  ;; `color-defined-p'.
  (when-let* ((a (color-name-to-rgb from))
              (b (color-name-to-rgb to)))
    (apply #'color-rgb-to-hex
           (append (color-blend b a fraction) '(2)))))

(defun tab-bar-theme--face-fg-or-bg (face)
  "Return FACE's foreground, or its background if the foreground is default.
Faces like `highlight' or `match' carry their color in the background."
  (let ((fg (face-foreground face nil 'default)))
    (if (equal fg (face-foreground 'default nil 'default))
        (face-background face nil 'default)
      fg)))

(defun tab-bar-theme--resolve-color (spec bg full)
  "Return the color SPEC denotes against the background BG, or FULL.
A number is the fraction of the way from BG toward FULL; a string is a
color used as is; a face gives its color per
`tab-bar-theme--face-fg-or-bg'.  Return FULL for anything else, or when
there is nothing to mix."
  ;; `stringp' before `facep': `facep' accepts a face name as a string,
  ;; and `face-attribute' then signals, e.g. for "shadow", which would
  ;; break `load-theme' via `tab-bar-theme--apply'.
  (or (pcase spec
        ((pred numberp) (tab-bar-theme--blend-color bg full spec))
        ((pred stringp) spec)
        ((pred facep) (tab-bar-theme--face-fg-or-bg spec)))
      full))

(defvar tab-bar-theme--overridden-faces nil
  "Faces this mode has given an override spec, for teardown.
Recorded when set, so teardown cannot miss a face.")

(defun tab-bar-theme--set-face (face spec)
  "Give FACE the override SPEC, and record it for teardown."
  (face-spec-set face spec)
  (push face tab-bar-theme--overridden-faces))

(defun tab-bar-theme--drop-faces ()
  "Drop every override spec this mode set.
Also called before each apply, so a face removed from the setup between
theme loads does not keep its override."
  (dolist (face tab-bar-theme--overridden-faces)
    (face-spec-set face nil 'face-override-spec))
  (setq tab-bar-theme--overridden-faces nil))

(defun tab-bar-theme--shifted (color spec)
  "Return COLOR shifted per SPEC, as in the background shift options.
A number shifts COLOR's lightness by that fraction, a string is a color
used as is, and nil returns COLOR."
  (pcase spec
    ((and (pred numberp) shift) (tab-bar-theme--shift-color color shift))
    ((and (pred stringp) literal) literal)
    (_ color)))

(defun tab-bar-theme--setup-base-faces ()
  "Apply base `tab-bar' faces."
  (let* ((bg-inactive (face-attribute 'mode-line-inactive :background nil 'default))
         (fg-inactive (face-attribute 'mode-line-inactive :foreground nil 'default))
         (bg-active   (face-attribute 'default :background))
         (fg-active   (face-attribute 'default :foreground))
         ;; Computed, not the theme's `tab-bar-tab-inactive': that is
         ;; designed against the theme's `tab-bar', while the strip here
         ;; is `mode-line-inactive', and the mix made non-current tabs
         ;; the loudest part of the row.
         (bg-tab-inactive
          (tab-bar-theme--shifted bg-inactive
                                  tab-bar-theme-tab-inactive-background-shift))
         (bg-group-current
          (tab-bar-theme--shifted bg-active
                                  tab-bar-theme-tab-group-background-shift))
         ;; No :box: the indicator bar height provides vertical padding.
         (common `( :inherit unspecified :box nil
                    ,@(when tab-bar-theme-height
                        (list :height tab-bar-theme-height))))
         (dim `(,@common :background ,bg-tab-inactive
                         :foreground ,fg-inactive)))
    ;; Override specs, not `custom-set-faces': the values derive from
    ;; the current theme, do not belong in a custom file, and
    ;; `tab-bar-theme--cleanup' can drop them.
    ;;
    ;; Every attribute is set on every face and none inherits another,
    ;; so the theme's inheritance between these faces cannot form a
    ;; cycle with the overrides.
    (dolist (face '(tab-bar-tab-inactive tab-bar-tab-ungrouped))
      (tab-bar-theme--set-face face `((t (,@dim)))))
    (pcase-dolist (`(,face ,spec)
                   `((tab-bar
                      ((t (,@common :background ,bg-inactive
                                    :foreground ,fg-inactive))))
                     (tab-bar-tab
                      ((t (,@common :background ,bg-active
                                    :foreground ,fg-active))))
                     (tab-bar-tab-group-inactive ((t (,@dim :weight bold))))
                     (tab-bar-tab-group-current
                      ((t (,@common :background ,bg-group-current
                                    :foreground ,fg-active
                                    :weight bold))))))
      (tab-bar-theme--set-face face spec))))

(defun tab-bar-theme--apply (&optional _theme)
  "Apply `tab-bar-theme' settings."
  (setq tab-bar-tab-face-function #'tab-bar-theme--tab-face)
  ;; `tab-bar-format-tabs-groups' uses `tab-bar-tab-group-face-function'
  ;; instead, so both must name the same function, as that variable's
  ;; docstring suggests.
  (setq tab-bar-tab-group-face-function #'tab-bar-theme--tab-face)
  (setq tab-bar-tab-group-format-function #'tab-bar-theme-tab-group-format)
  (tab-bar-theme--drop-faces)
  (tab-bar-theme--setup-base-faces)
  ;; After the base faces, which the pairs are mixed from.
  (tab-bar-theme--drop-pairs))


(defconst tab-bar-theme--variables
  '( tab-bar-tab-face-function
     tab-bar-tab-group-face-function
     tab-bar-tab-group-format-function)
  "Variables `tab-bar-theme--apply' overwrites.
Not `tab-bar-tab-name-format-functions', which this package does not
own; see `tab-bar-theme-tab-name-format-padding'.")

(defvar tab-bar-theme--saved nil
  "Alist of what `tab-bar-theme--variables' held before the mode.")

(defun tab-bar-theme--save-variables ()
  "Record the values of `tab-bar-theme--variables', to restore later."
  ;; Only once: `define-minor-mode' runs its body on every enable call,
  ;; so a second call would record the mode's own values.
  (unless tab-bar-theme--saved
    (setq tab-bar-theme--saved
          (mapcar (lambda (sym) (cons sym (symbol-value sym)))
                  tab-bar-theme--variables))))

(defun tab-bar-theme--restore-variables ()
  "Restore what `tab-bar-theme--save-variables' recorded.
Return non-nil if anything was recorded, i.e. the mode was on."
  (when tab-bar-theme--saved
    (dolist (cell tab-bar-theme--saved)
      (set (car cell) (cdr cell)))
    (setq tab-bar-theme--saved nil)
    t))

(defun tab-bar-theme--cleanup ()
  "Undo `tab-bar-theme--apply'."
  (when (tab-bar-theme--restore-variables)
    (tab-bar-theme--drop-faces)
    ;; The cached colors derive from the dropped faces, and other
    ;; modes' formatters may still ask for them.
    (tab-bar-theme--drop-pairs)))



;;;###autoload
(define-minor-mode tab-bar-theme-mode
  "Toggle tab bar faces and colors derived from the current theme.
When enabled, set the tab bar faces on every theme change and take over
`tab-bar-tab-face-function', `tab-bar-tab-group-face-function' and
`tab-bar-tab-group-format-function'; restore them when disabled."
  :global t :group 'tab-bar-theme
  (cond
   (tab-bar-theme-mode
    (tab-bar-theme--save-variables)
    (tab-bar-theme--apply)
    (add-hook 'enable-theme-functions #'tab-bar-theme--apply))
   (t
    (remove-hook 'enable-theme-functions #'tab-bar-theme--apply)
    (tab-bar-theme--cleanup)))
  (force-mode-line-update t))

(provide 'tab-bar-theme)
;;; tab-bar-theme.el ends here

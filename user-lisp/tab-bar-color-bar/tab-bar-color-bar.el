;;; tab-bar-color-bar.el --- Colored bar for tab-bar tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, tools
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

;; Draw a colored bar before a tab-bar tab's label.  The current tab's
;; bar mixes the color `tab-bar-color-bar-color-function' returns for
;; that tab into the tab background, per `tab-bar-color-bar-blend'.
;; Other tabs, tab groups and the closing bar share one muted tone, per
;; `tab-bar-color-bar-inactive-blend'.  What the color means is up to
;; the caller; this package only renders and places the bar.
;;
;; Add `tab-bar-color-bar-tab-name-format' to
;; `tab-bar-tab-name-format-functions', after padding and right before
;; `tab-bar-tab-name-format-face'.  Bars are drawn wherever the
;; formatters are listed.

;;; Code:

(require 'tab-bar)
(require 'color)


(defgroup tab-bar-color-bar nil
  "Colored bar for tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-color-bar-")

(defcustom tab-bar-color-bar-color-function nil
  "Function returning the color of a tab, or nil for none.
Called with TAB and I, as `tab-bar-tab-name-format-functions' passes
them.  The current tab's bar shows the color, see
`tab-bar-color-bar-blend'.  A nil function draws no bars."
  :type '(choice (const :tag "None" nil) function)
  :group 'tab-bar-color-bar)

(defcustom tab-bar-color-bar-blend 0.8
  "Color of the current tab's bar.
A number is the fraction of the way from the tab background toward the
tab color, or toward the `tab-bar-tab' foreground if the tab has none; a
string is a color used as is; nil is the tab color itself."
  :type '(choice (const :tag "The color itself" nil)
                 (float :tag "Fraction of the way from the tab's background")
                 color)
  :group 'tab-bar-color-bar)

(defcustom tab-bar-color-bar-inactive-blend 0.3
  "Color of the bars of other tabs, tab groups and the closing bar.
A number is the fraction of the way from the face's background toward
its foreground; a string is a color used as is; nil is the foreground
itself.  All of them share one tone: the label tells tabs apart."
  :type '(choice (const :tag "The foreground itself" nil)
                 (float :tag "Fraction of the way from the background")
                 color)
  :group 'tab-bar-color-bar)

(defcustom tab-bar-color-bar-width 3
  "Width of a tab's bar; zero draws none.
A float is a fraction of the frame's character width, so it scales with
the font; an integer is a width in pixels.  All bars have the same
width, so selecting a tab does not shift the row."
  :type '(choice (natnum :tag "Pixels")
                 (float :tag "Fraction of a character's width"))
  :group 'tab-bar-color-bar)

(defcustom tab-bar-color-bar-height nil
  "Height in pixels of a tab's bar, or nil for the default.
The bar is the tallest element of the row, so this sets the tab bar
height.  The default is 1.2 times the frame's character height.
Without bars the row is as tall as its text."
  :type '(choice (const :tag "Default" nil) natnum)
  :group 'tab-bar-color-bar)

(defcustom tab-bar-color-bar-symbol "▌"
  "String drawn as the current tab's bar on frames without images.
It is drawn in the bar's color.  A block element fills part of the cell
\(U+258F through U+2588 range from 1/8 to the whole cell); a
box-drawing character draws a line.  If the terminal cannot encode it,
\"|\" is used instead.  See also `tab-bar-color-bar-inactive-symbol'."
  :type 'string
  :group 'tab-bar-color-bar)

(defcustom tab-bar-color-bar-inactive-symbol "▏"
  "String drawn as a non-current tab's bar on frames without images.
A terminal cannot leave part of a cell unpainted, so a narrower block
element stands for the narrower bar; the cell width is the same.
Falls back to \"|\" like `tab-bar-color-bar-symbol'."
  :type 'string
  :group 'tab-bar-color-bar)


(defun tab-bar-color-bar--drawn-p ()
  "Return non-nil if bars are drawn at all."
  (and (numberp tab-bar-color-bar-width) (> tab-bar-color-bar-width 0)))

(defun tab-bar-color-bar--image-p ()
  "Return non-nil if bars are drawn as images."
  (and (display-graphic-p) (image-type-available-p 'pbm)))

(defun tab-bar-color-bar--height ()
  "Return the bar height in pixels, which is also the row height."
  (or tab-bar-color-bar-height
      ;; A tenth of a line above and below the label.
      (round (* (frame-char-height) 1.2))))

(defun tab-bar-color-bar--width ()
  "Return the bar width in pixels.
A float width yields at least one pixel, so small fonts never round it
to zero."
  (let ((width tab-bar-color-bar-width))
    (if (integerp width)
        width
      (max 1 (round (* width (frame-char-width)))))))

(defun tab-bar-color-bar--symbol (current-p)
  "Return the terminal bar string for CURRENT-P, or \"|\".
Fall back to \"|\" when `char-displayable-p' reports that the terminal
coding system cannot encode it.  A glyph missing from the terminal
font is undetectable and shows as a box."
  (let ((symbol (if current-p
                    tab-bar-color-bar-symbol
                  tab-bar-color-bar-inactive-symbol)))
    (if (seq-every-p #'char-displayable-p symbol) symbol "|")))

(defvar tab-bar-color-bar--color-cache (make-hash-table :test #'equal)
  "Bar colors keyed by (SPEC BACKGROUND TARGET).
All three are literal values, so theme changes need no flush.")

(defun tab-bar-color-bar--mix (spec face &optional target)
  "Return the bar color SPEC gives against FACE's background.
TARGET is the color to mix toward, by default FACE's foreground.  A
number is the fraction of the way from the background toward TARGET; a
string is used as is; nil, or colors that do not resolve, give TARGET."
  (let ((bg (face-attribute face :background nil t))
        (target (or target (face-attribute face :foreground nil t))))
    (with-memoization (gethash (list spec bg target)
                               tab-bar-color-bar--color-cache)
      (or (pcase spec
            ((pred stringp) spec)
            ((pred numberp)
             (when-let* ((from (color-name-to-rgb bg))
                         (to (color-name-to-rgb target)))
               (apply #'color-rgb-to-hex
                      (append (color-blend to from spec) '(2))))))
          target))))

(defun tab-bar-color-bar--inactive-color (face)
  "Return the muted bar color drawn on FACE."
  (tab-bar-color-bar--mix tab-bar-color-bar-inactive-blend face))

(defvar tab-bar-color-bar--cache (make-hash-table :test #'equal)
  "Bar strings keyed by (COLOR WIDTH HEIGHT CURRENT-P FACE).
COLOR is a literal color, so theme changes need no flush; WIDTH and
HEIGHT are resolved pixels, which differ between frames.")

;; Workaround: `tab-bar-auto-width' pads a tab with copies of its first
;; character's properties.  Copies of a bar's `display' are `eq', draw
;; as one element, never widen the tab, and hang redisplay.  This lead
;; has no `display', so the copies are plain spaces.  Drawn as a 1-pixel
;; `thin-space'; terminals need no lead.
(defconst tab-bar-color-bar--lead (string #x200b)
  "Zero-width space leading a graphic bar.")

(defun tab-bar-color-bar--append-face (bar face)
  "Append FACE to BAR's faces and return BAR.
Return BAR unchanged if FACE is nil."
  ;; Append, do not set: on a terminal the bar's color lives in its
  ;; `face' property, which a set face would override.
  (when face
    (add-face-text-property 0 (length bar) face t bar))
  bar)


;;;###autoload
(defun tab-bar-color-bar-strut ()
  "Return an empty string as tall as the tab bar row, aligned like a bar.
This is a strut: an invisible element of zero width that gives the row
it is in a minimum height.  Put it in another row, e.g. a header line,
to give that row the tab bar's height; for use in
`tab-bar-tree-header-format'.  Return \"\" if bars are not drawn as
images."
  (if-let* (((tab-bar-color-bar--drawn-p))
            ((tab-bar-color-bar--image-p))
            (font (font-info (face-font 'tab-bar))))
      (let ((height (tab-bar-color-bar--height))
            (ascent (aref font 8))
            (descent (aref font 9)))
        ;; Center on the text's middle, like the image's `:ascent center'.
        ;; A `space' spec takes :ascent as a percentage of its height.
        (propertize " " 'display
                    `(space :width (0) :height (,height)
                            :ascent ,(round (+ 50 (/ (* 50.0 (- ascent descent))
                                                     height))))))
    ""))

;;;###autoload
(defun tab-bar-color-bar-build (color &optional current-p face)
  "Return a bar string in COLOR, or \"\" if the width is zero.
CURRENT-P draws the current tab's bar.  An undefined COLOR gives a
blank of the same width.  FACE is appended behind the bar.
The returned string may be shared; copy it before adding properties."
  ;; Every bar reserves the same width, so selecting a tab or a tab
  ;; losing its color does not shift the row.  FACE fills the unpainted
  ;; part; a later formatter that faces the whole label, like
  ;; `tab-bar-tab-name-format-face', makes it unnecessary.
  (if (not (tab-bar-color-bar--drawn-p))
      ""
    (let ((width (tab-bar-color-bar--width))
          (height (tab-bar-color-bar--height)))
      (if (not (color-defined-p color))
          ;; Not cached: COLOR may be defined later, e.g. by a theme.
          (tab-bar-color-bar--append-face
           (if (tab-bar-color-bar--image-p)
               (concat tab-bar-color-bar--lead
                       (propertize " " 'display `(space :width (,width))))
             (copy-sequence " "))
           face)
        (with-memoization (gethash (list color width height current-p face)
                                   tab-bar-color-bar--cache)
          (if (tab-bar-color-bar--image-p)
              ;; The painted part is an image exactly that wide.  The rest
              ;; is a transparent `space'; PBM 0 bits would be opaque white.
              (let ((painted (if current-p width 1)))
                (tab-bar-color-bar--append-face
                 (concat
                  tab-bar-color-bar--lead
                  (propertize " " 'display
                              (create-image
                               (format "P1\n%d %d\n%s\n" painted height
                                       (make-string (* painted height) ?1))
                               'pbm t :foreground color :ascent 'center))
                  (and (< painted width)
                       (propertize " " 'display
                                   `(space :width (,(- width painted))))))
                 face))
            (tab-bar-color-bar--append-face
             (propertize (tab-bar-color-bar--symbol current-p)
                         'face (list :foreground color))
             face)))))))

;;;###autoload
(defun tab-bar-color-bar-tab-name-format (name tab i)
  "Prefix tab label NAME with the bar for TAB and slot I.
Return NAME unchanged if `tab-bar-color-bar-color-function' is nil.
Place it after padding, so the bar sits at the tab's edge, and right
before `tab-bar-tab-name-format-face', so that face covers it."
  (if tab-bar-color-bar-color-function
      (let ((current (eq (car tab) 'current-tab)))
        (concat (tab-bar-color-bar-build
                 (if current
                     (tab-bar-color-bar--mix
                      tab-bar-color-bar-blend 'tab-bar-tab
                      (funcall tab-bar-color-bar-color-function tab i))
                   (tab-bar-color-bar--inactive-color 'tab-bar-tab-inactive))
                 current)
                name))
    name))

;;;###autoload
(defun tab-bar-color-bar-tab-group-format (name _tab _i &optional current-p)
  "Prefix group label NAME with a bar in the muted tone of the group face.
The face is `tab-bar-tab-group-current' if CURRENT-P, else
`tab-bar-tab-group-inactive'; the bar is drawn as for a non-current tab.
For group label formatters, e.g. `tab-bar-theme-tab-group-format-functions'."
  (concat (tab-bar-color-bar-build
           (tab-bar-color-bar--inactive-color
            (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))
          name))

;;;###autoload
(defun tab-bar-color-bar-separator ()
  "Return a bar closing the tab list, or \"\" if none.
Drawn in the muted tone of non-current tabs, if
`tab-bar-color-bar-color-function' is non-nil.  For `tab-bar-format',
after `tab-bar-format-tabs-groups'."
  (if tab-bar-color-bar-color-function
      (tab-bar-color-bar-build
       (tab-bar-color-bar--inactive-color 'tab-bar-tab-inactive))
    ""))

(provide 'tab-bar-color-bar)
;;; tab-bar-color-bar.el ends here

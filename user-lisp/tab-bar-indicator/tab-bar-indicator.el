;;; tab-bar-indicator.el --- Colored bar for tab-bar tabs -*- lexical-binding: t; -*-

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

;; Draw a colored indicator before a tab-bar tab's label, in the color
;; `tab-bar-indicator-color-function' returns for that tab.  What the
;; color means is up to the caller; this package only renders and
;; places the indicator.
;;
;; Add `tab-bar-indicator-tab-name-format' to
;; `tab-bar-tab-name-format-functions', after padding and right before
;; `tab-bar-tab-name-format-face'.  Indicators are drawn wherever the
;; formatters are listed.

;;; Code:

(require 'tab-bar)

(defgroup tab-bar-indicator nil
  "Colored bar for tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-indicator-")

(defcustom tab-bar-indicator-color-function nil
  "Function returning the color of a tab's indicator, or nil for none.
Called with TAB and I, as `tab-bar-tab-name-format-functions' passes
them.  A nil function draws no indicator; a nil value draws a blank of
the same width.

`tab-bar-indicator-separator' calls it with both arguments nil, so it
must not assume TAB is non-nil."
  :type '(choice (const :tag "None" nil) function)
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-group-color-function nil
  "Function returning the color of a tab group's indicator, or nil.
Called with TAB, I and CURRENT-P, like group label formatters.  A nil
function draws no indicator; a nil value draws a blank of the same
width."
  :type '(choice (const :tag "None" nil) function)
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-width 3
  "Width of a tab's indicator; zero draws none.
A float is a fraction of the frame's character width, so it scales with
the font; an integer is a width in pixels.  All indicators have the
same width, so selecting a tab does not shift the row."
  :type '(choice (natnum :tag "Pixels")
                 (float :tag "Fraction of a character's width"))
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-height nil
  "Height in pixels of a tab's indicator, or nil for the default.
The indicator is the tallest element of the row, so this sets the tab
bar height.  The default is 1.2 times the frame's character height.
Without indicators the row is as tall as its text."
  :type '(choice (const :tag "Default" nil) natnum)
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-symbol "▌"
  "String drawn as the current tab's indicator on frames without images.
It is drawn in the indicator's color.  A block element fills part of
the cell (U+258F through U+2588 range from 1/8 to the whole cell); a
box-drawing character draws a line.  If the terminal cannot encode it,
\"|\" is used instead.  See also `tab-bar-indicator-inactive-symbol'."
  :type 'string
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-inactive-symbol "▏"
  "String drawn as a non-current tab's indicator on frames without images.
A terminal cannot leave part of a cell unpainted, so a narrower block
element stands for the narrower indicator; the cell width is the same.
Falls back to \"|\" like `tab-bar-indicator-symbol'."
  :type 'string
  :group 'tab-bar-indicator)

(defun tab-bar-indicator--drawn-p ()
  "Return non-nil if indicators are drawn at all."
  (and (numberp tab-bar-indicator-width) (> tab-bar-indicator-width 0)))

(defun tab-bar-indicator--image-p ()
  "Return non-nil if indicators are drawn as images."
  (and (display-graphic-p) (image-type-available-p 'pbm)))

(defun tab-bar-indicator--height ()
  "Return the indicator height in pixels, which is also the row height."
  (or tab-bar-indicator-height
      ;; A tenth of a line above and below the label.
      (round (* (frame-char-height) 1.2))))

;;;###autoload
(defun tab-bar-indicator-strut ()
  "Return an empty string as tall as the tab bar row, aligned like an indicator.
Return \"\" if indicators are not drawn as images."
  (if-let* (((tab-bar-indicator--drawn-p))
            ((tab-bar-indicator--image-p))
            (font (font-info (face-font 'tab-bar))))
      (let ((height (tab-bar-indicator--height))
            (ascent (aref font 8))
            (descent (aref font 9)))
        ;; Center on the text's middle, like the image's `:ascent center'.
        ;; A `space' spec takes :ascent as a percentage of its height.
        (propertize " " 'display
                    `(space :width (0) :height (,height)
                            :ascent ,(round (+ 50 (/ (* 50.0 (- ascent descent))
                                                     height))))))
    ""))

(defun tab-bar-indicator--width ()
  "Return the indicator width in pixels.
A float width yields at least one pixel, so small fonts never round it
to zero."
  (let ((width tab-bar-indicator-width))
    (if (integerp width)
        width
      (max 1 (round (* width (frame-char-width)))))))

(defun tab-bar-indicator--symbol (current-p)
  "Return the terminal indicator string for CURRENT-P, or \"|\".
Fall back to \"|\" when `char-displayable-p' reports that the terminal
coding system cannot encode it.  A glyph missing from the terminal
font is undetectable and shows as a box."
  (let ((symbol (if current-p
                    tab-bar-indicator-symbol
                  tab-bar-indicator-inactive-symbol)))
    (if (seq-every-p #'char-displayable-p symbol) symbol "|")))

(defvar tab-bar-indicator--cache (make-hash-table :test #'equal)
  "Indicator strings keyed by (COLOR WIDTH HEIGHT CURRENT-P FACE).
COLOR is a literal color, so theme changes need no flush; WIDTH and
HEIGHT are resolved pixels, which differ between frames.")

;; Workaround: `tab-bar-auto-width' pads a tab with copies of its first
;; character's properties.  Copies of an indicator's `display' are `eq',
;; draw as one element, never widen the tab, and hang redisplay.  This
;; lead has no `display', so the copies are plain spaces.  Drawn as a
;; 1-pixel `thin-space'; terminals need no lead.
(defconst tab-bar-indicator--lead (string #x200b)
  "Zero-width space leading a graphic indicator.")

;;;###autoload
(defun tab-bar-indicator-build (color &optional current-p face)
  "Return an indicator string in COLOR, or \"\" if the width is zero.
CURRENT-P draws the current tab's indicator.  An undefined COLOR gives a
blank of the same width.  FACE is appended behind the indicator.
The returned string may be shared; copy it before adding properties."
  ;; Every indicator reserves the same width, so selecting a tab or a tab
  ;; losing its color does not shift the row.  FACE fills the unpainted
  ;; part; a later formatter that faces the whole label, like
  ;; `tab-bar-tab-name-format-face', makes it unnecessary.
  (if (not (tab-bar-indicator--drawn-p))
      ""
    (let ((width (tab-bar-indicator--width))
          (height (tab-bar-indicator--height)))
      (if (not (color-defined-p color))
          ;; Not cached: COLOR may be defined later, e.g. by a theme.
          (tab-bar-indicator--append-face
           (if (tab-bar-indicator--image-p)
               (concat tab-bar-indicator--lead
                       (propertize " " 'display `(space :width (,width))))
             (copy-sequence " "))
           face)
        (with-memoization (gethash (list color width height current-p face)
                                   tab-bar-indicator--cache)
          (if (tab-bar-indicator--image-p)
              ;; The painted part is an image exactly that wide.  The rest
              ;; is a transparent `space'; PBM 0 bits would be opaque white.
              (let ((painted (if current-p width 1)))
                (tab-bar-indicator--append-face
                 (concat
                  tab-bar-indicator--lead
                  (propertize " " 'display
                              (create-image
                               (format "P1\n%d %d\n%s\n" painted height
                                       (make-string (* painted height) ?1))
                               'pbm t :foreground color :ascent 'center))
                  (and (< painted width)
                       (propertize " " 'display
                                   `(space :width (,(- width painted))))))
                 face))
            (tab-bar-indicator--append-face
             (propertize (tab-bar-indicator--symbol current-p)
                         'face (list :foreground color))
             face)))))))

(defun tab-bar-indicator--append-face (indicator face)
  "Append FACE to INDICATOR's faces and return INDICATOR.
Return INDICATOR unchanged if FACE is nil."
  ;; Append, do not set: on a terminal the indicator's color lives in
  ;; its `face' property, which a set face would override.
  (when face
    (add-face-text-property 0 (length indicator) face t indicator))
  indicator)

;;;###autoload
(defun tab-bar-indicator-tab-name-format (name tab i)
  "Prefix tab label NAME with the indicator for TAB and slot I.
Return NAME unchanged if `tab-bar-indicator-color-function' is nil.
Place it after padding, so the indicator sits at the tab's edge, and
right before `tab-bar-tab-name-format-face', so that face covers it."
  (if tab-bar-indicator-color-function
      (concat (tab-bar-indicator-build
               (funcall tab-bar-indicator-color-function tab i)
               (eq (car tab) 'current-tab))
              name)
    name))

;;;###autoload
(defun tab-bar-indicator-tab-group-format (name tab i &optional current-p)
  "Prefix group label NAME with an indicator.
The color comes from `tab-bar-indicator-group-color-function' called
with TAB, I and CURRENT-P; the indicator is drawn as for a non-current
tab.  Return NAME unchanged if that function is nil.  For group label
formatters, e.g. `tab-bar-theme-tab-group-format-functions'."
  (if tab-bar-indicator-group-color-function
      (concat (tab-bar-indicator-build
               (funcall tab-bar-indicator-group-color-function
                        tab i current-p))
              name)
    name))

;;;###autoload
(defun tab-bar-indicator-separator ()
  "Return an indicator closing the tab list, or \"\" if none.
Its color is what `tab-bar-indicator-color-function' returns for nil
TAB and I, independent of any tab.  For `tab-bar-format', after
`tab-bar-format-tabs-groups'."
  (if tab-bar-indicator-color-function
      (tab-bar-indicator-build
       (funcall tab-bar-indicator-color-function nil nil))
    ""))

(provide 'tab-bar-indicator)
;;; tab-bar-indicator.el ends here

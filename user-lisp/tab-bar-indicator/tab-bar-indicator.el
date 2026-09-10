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

;; Draws a colored indicator ahead of a tab-bar tab's label, in whatever
;; color `tab-bar-indicator-color-function' names for that tab.  What a
;; tab's color means — a project, a group, a place in some rotation — is
;; entirely the caller's business; this package only turns a color into
;; an indicator and knows how to place one.
;;
;; Add `tab-bar-indicator-tab-name-format' to
;; `tab-bar-tab-name-format-functions' yourself, between the hints and
;; the face formatters — see its docstring for why there.

;;; Code:

(require 'tab-bar)

(defgroup tab-bar-indicator nil
  "Colored bar for tab-bar tabs."
  :group 'tab-bar
  :prefix "tab-bar-indicator-")

(defcustom tab-bar-indicator-color-function nil
  "Function returning the color a tab's indicator is drawn in, or nil for none.
Called with two arguments, TAB and I — the tab and its slot, the way
`tab-bar-tab-name-format-functions' hands them to a formatter.  An
answer of nil, or no function at all, draws no indicator on that tab.

Both arguments are nil when the question is about no tab in particular,
which is how `tab-bar-indicator-separator' asks: answer without reading
TAB, rather than assuming there is one."
  :type '(choice (const :tag "None" nil) function)
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-width 3
  "Width of a tab's indicator; zero draws none.
A float is that fraction of a character's width, so it holds its
proportion when the font size changes; an integer is that many pixels
outright.  Every indicator is the same size, so selecting a tab never moves
the row; what tells tabs apart is the color."
  :type '(choice (natnum :tag "Pixels")
                 (float :tag "Fraction of a character's width"))
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-height nil
  "Height in pixels of a tab's indicator.
It is the tallest thing in the row, so this sets the height of the tab
bar itself; nil leaves room around the label.  With none drawn at all
the row is only as tall as its text."
  :type '(choice (const :tag "Default" nil) natnum)
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-symbol "▌"
  "Symbol standing in for the indicator where a frame draws no images.
It is drawn in the indicator's color, so a block element fills that much
of the cell and reads as one — U+258F through U+2588 go from an eighth
of it to the whole — while a box-drawing character draws a line
instead.  A terminal that cannot encode the symbol falls back on a
plain \"|\".

This is the indicator of the current tab; `tab-bar-indicator-inactive-symbol'
is the one every other tab gets."
  :type 'string
  :group 'tab-bar-indicator)

(defcustom tab-bar-indicator-inactive-symbol "▏"
  "Symbol standing in for the indicator of a tab that is not current.
A terminal has no fraction of a cell to leave unpainted the way
`tab-bar-indicator-width' leaves pixels, so a narrower block element
stands in for the narrower indicator: the cell is the same width
either way, and less of it is filled.  Shaped like `tab-bar-indicator-symbol' in
every other way, including the fallback for a terminal that cannot
encode it."
  :type 'string
  :group 'tab-bar-indicator)

(defun tab-bar-indicator--drawn-p ()
  "Return non-nil where an indicator is drawn at all."
  (and (numberp tab-bar-indicator-width) (> tab-bar-indicator-width 0)))

(defun tab-bar-indicator--height ()
  "Return the height in pixels of an indicator, and so of the row."
  (or tab-bar-indicator-height
      ;; A fifth of a line of room around the label, which is what a box
      ;; around a tab used to add.
      (round (* (frame-char-height) 1.2))))

(defun tab-bar-indicator--width ()
  "Return the width in pixels of an indicator.
A fraction is at least one pixel: a knob meant to draw something must
not round itself away to nothing on a small font."
  (let ((width tab-bar-indicator-width))
    (if (integerp width)
        width
      (max 1 (round (* width (frame-char-width)))))))

(defun tab-bar-indicator--symbol (current-p)
  "Return the symbol standing in for an indicator in CURRENT-P state.
Falling back on a plain bar where the terminal cannot encode it:
`char-displayable-p' answers for the coding system, and a terminal
whose font simply has no glyph still shows a box, since Emacs can ask
a font what it holds and a terminal nothing."
  (let ((symbol (if current-p
                    tab-bar-indicator-symbol
                  tab-bar-indicator-inactive-symbol)))
    (if (memq nil (mapcar #'char-displayable-p symbol)) "|" symbol)))

(defvar tab-bar-indicator--cache (make-hash-table :test #'equal)
  "An indicator string for each (COLOR WIDTH HEIGHT CURRENT-P FACE) so far.
Keyed on a literal color rather than on anything theme-derived, so
nothing here needs dropping on a theme change; on the resolved pixel
size, which two frames can differ in; and on the rest because each is
part of the string.  A color no frame can render is held here too, as
an indicator with nothing painted in it.")

;;;###autoload
(defun tab-bar-indicator-build (color &optional current-p face)
  "Return an indicator in COLOR, reserving its width even where none is drawn.
Every indicator reserves the same width whatever CURRENT-P or COLOR, so
neither selecting a tab nor a tab losing its color ever moves the row:
a nil CURRENT-P paints only the first pixel of that width and leaves
the rest transparent, so the current tab's alone reads as filled; an
unreadable COLOR paints none of it, transparent throughout, rather than
shrinking the indicator away to nothing.  A frame without images gets
`tab-bar-indicator-symbol' or `tab-bar-indicator-inactive-symbol'
instead — one cell either way, filled less of, or a plain space where
COLOR cannot be drawn in it at all.

FACE goes behind the indicator, so the width left unpainted shows that
face's background rather than the tab bar the indicator sits on.  A
caller inside `tab-bar-tab-name-format-functions' has no use for it,
`tab-bar-tab-name-format-face' covering the whole label already; one
formatting a group's own label, which core leaves outside that
pipeline, does.

The returned string is shared: copy it before giving it properties of
your own."
  (if (not (tab-bar-indicator--drawn-p))
      ""
    (let ((width (tab-bar-indicator--width))
          (height (tab-bar-indicator--height)))
      (with-memoization (gethash (list color width height current-p face)
                                 tab-bar-indicator--cache)
        (cond
         ;; No real color to draw: still reserve the width, painted with
         ;; nothing.  Decided once, here, ahead of the backends below,
         ;; so a third one added later inherits the same reserved width
         ;; instead of needing its own reminder to check COLOR at all.
         ((not (color-defined-p color))
          (tab-bar-indicator--append-face
           (if (and (display-graphic-p) (image-type-available-p 'pbm))
               (propertize " " 'display `(space :width (,width)))
             (copy-sequence " "))
           face))
         ((and (display-graphic-p) (image-type-available-p 'pbm))
          ;; The painted part is its own image, exactly as wide as it is
          ;; painted; the rest, when CURRENT-P leaves any, is a `space'
          ;; — unlike a PBM's unpainted "0" bits, plain white by the
          ;; format itself, no transparency to speak of — which shows
          ;; whatever is really behind it.
          (let* ((painted (if current-p width 1))
                 (row (make-string painted ?1)))
            (tab-bar-indicator--append-face
             (concat
              (propertize " " 'display
                          (create-image
                           (format "P1\n%d %d\n%s\n" painted height
                                   (apply #'concat (make-list height row)))
                           'pbm t :foreground color :ascent 'center))
              (and (< painted width)
                   (propertize " " 'display
                               `(space :width (,(- width painted))))))
             face)))
         (t
          (tab-bar-indicator--append-face
           (propertize (tab-bar-indicator--symbol current-p)
                       'face (list :foreground color))
           face)))))))

(defun tab-bar-indicator--append-face (indicator face)
  "Add FACE behind INDICATOR and return it, or return it as is for a nil FACE."
  ;; Appended, never set: the indicator carries its own color, in
  ;; `display' on a graphic frame but in `face' on a terminal, where a
  ;; face set over it would paint it out.
  (when face
    (add-face-text-property 0 (length indicator) face t indicator))
  indicator)

(defvar tab-bar-indicator--saved-auto-width nil
  "What the option `tab-bar-auto-width' held, to restore on disable.
A list, because nil is a value the option itself can hold: nil here is
nothing saved yet, a one-element list is a saved nil.")

(defun tab-bar-indicator--auto-width-watcher (_symbol newval operation &rest _)
  "Refuse a non-nil option `tab-bar-auto-width' while an indicator is drawn.
NEWVAL and OPERATION are as `add-variable-watcher' passes them; a
`let' is allowed through, since erroring inside somebody else's
binding would break code that has nothing to do with this."
  ;; `tab-bar-auto-width' pads a tab by copying the text properties of
  ;; the label's first character onto a space — and with an indicator
  ;; drawn, that character is an image.  Every padding space then
  ;; carries the same, `eq' image spec, which the display engine
  ;; coalesces into one occurrence, so the width never grows and the
  ;; loop never ends: the frame hangs, and not interruptibly.
  (when (and newval
             (eq operation 'set)
             (bound-and-true-p tab-bar-indicator-mode)
             (tab-bar-indicator--drawn-p))
    (error "Tab-bar-auto-width hangs redisplay while tab-bar-indicator \
draws an indicator; set tab-bar-indicator-width to 0 first")))

;;;###autoload
(define-minor-mode tab-bar-indicator-mode
  "Toggle a colored indicator on tab-bar tabs.
Its color comes from `tab-bar-indicator-color-function'.  This mode
does not place the indicator itself — add
`tab-bar-indicator-tab-name-format' to
`tab-bar-tab-name-format-functions' where it belongs; the mode owns
turning that formatter on and off, and guarding the option
`tab-bar-auto-width' while it draws."
  :group 'tab-bar-indicator :global t
  (if tab-bar-indicator-mode
      (progn
        ;; A mode enabled twice must not save the value it set itself.
        (unless tab-bar-indicator--saved-auto-width
          (setq tab-bar-indicator--saved-auto-width (list tab-bar-auto-width)))
        ;; Removed first: `define-minor-mode' runs this body on every call.
        (remove-variable-watcher 'tab-bar-auto-width
                                 #'tab-bar-indicator--auto-width-watcher)
        (add-variable-watcher 'tab-bar-auto-width
                              #'tab-bar-indicator--auto-width-watcher)
        (when (tab-bar-indicator--drawn-p)
          (setq tab-bar-auto-width nil)))
    ;; Before restoring, whose job is to put back a value the watcher
    ;; would refuse: Emacs ships this option as t.
    (remove-variable-watcher 'tab-bar-auto-width
                             #'tab-bar-indicator--auto-width-watcher)
    (when tab-bar-indicator--saved-auto-width
      (setq tab-bar-auto-width (car tab-bar-indicator--saved-auto-width))
      (setq tab-bar-indicator--saved-auto-width nil)))
  (force-mode-line-update t))

(defun tab-bar-indicator--enabled-p ()
  "Return non-nil where this package has an indicator to draw at all."
  (and tab-bar-indicator-mode tab-bar-indicator-color-function))

;;;###autoload
(defun tab-bar-indicator-tab-name-format (name tab i)
  "Prefix tab label NAME with the indicator TAB and slot I are colored for.
A no-op with `tab-bar-indicator-mode' off or with no color function to
ask, so callers may list this in `tab-bar-tab-name-format-functions'
unconditionally: it goes right before `tab-bar-tab-name-format-face',
so that face covers the indicator too, and after any padding, so the
indicator sits flush against the tab's own edge rather than inside it."
  (if (tab-bar-indicator--enabled-p)
      (concat (tab-bar-indicator-build
               (funcall tab-bar-indicator-color-function tab i)
               (eq (car tab) 'current-tab))
              name)
    name))

;;;###autoload
(defun tab-bar-indicator-separator ()
  "Return an indicator closing off the tab list, or the empty string for none.
Drawn in whatever `tab-bar-indicator-color-function' answers for no tab
in particular — it is called with nil for both arguments — so it
borrows no one tab's own color.  To name in `tab-bar-format' after
`tab-bar-format-tabs-groups'."
  (if (tab-bar-indicator--enabled-p)
      (tab-bar-indicator-build
       (funcall tab-bar-indicator-color-function nil nil))
    ""))

(provide 'tab-bar-indicator)
;;; tab-bar-indicator.el ends here

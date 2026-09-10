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
(require 'color)


(defgroup tab-bar-theme
  nil
  "Tab Bar Theme."
  :group 'tab-bar)

;; The rotation borrows its colors from font-lock rather than from
;; `outline-N': font-lock is preloaded, so the colors are there from the
;; first frame, where outline.el is loaded on demand and a session that
;; has not needed it leaves every slot without a foreground.
;;
;; Which sixteen, and in which order, was measured rather than chosen,
;; over the 149 modus/ef/doric/doom themes installed here: these are the
;; font-lock faces that carry a color of their own in nearly every theme
;; (the rest either inherit one of these or are left at the default
;; foreground, where a slot would come out looking uncolored), and this
;; order keeps the greatest distance between neighbours — no less than
;; 25 ΔE2000 — which matters because the rotation wraps and the row is
;; read left to right.
;;
;; Sixteen is the ceiling: no theme measured offers more than sixteen
;; distinguishable font-lock colors, and the median offers ten.  So the
;; slots past the tenth buy fewer collisions, not more colors a reader
;; can tell apart: two projects on similar colors still beat two on one.
;;
;; One face per slot, not two: what a tab that is not current gets is
;; the same color dimmed, and `tab-bar-theme--face-pair' derives that
;; where it already derives the bar.

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

(defcustom tab-bar-theme-height nil
  "Height of tab bar face.
When nil, height is not set."
  :type '(choice (const :tag "Unset" nil)
                 (number :tag "Height"))
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-inactive-background-shift 0.3
  "How far an inactive tab's background is shifted off the tab bar's.

At its default background, an inactive tab's own shade is identical
to the tab-bar strip itself (visible behind the tab-bar menu icon and
in the empty tail after the last tab), so an inactive tab —
particularly the last one, right against that empty tail — reads as a
continuation of the strip rather than a tab of its own.

A number nudges that shared background toward the active tab's by that
fraction of its own lightness, via `tab-bar-theme--shift-color'; a pale
theme needs a much bigger shift than a saturated one to read as a
different color at all, so the default is tuned by eye and worth
adjusting per theme.
A color string is used as is.  nil matches the strip, as before this
option existed."
  :type '(choice (const :tag "Match the strip" nil)
                 (float :tag "Shift fraction toward the active tab")
                 color)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-background-shift nil
  "How far the current group's label sits off the active tab's background.
The two share a background by default, so a group and the tab of it you
are on read as one block — which also leaves a group whose name matches
that tab's looking like the same thing twice.

A number shifts the label's background off the tab's by that fraction
of its own lightness, via `tab-bar-theme--shift-color': negative is
toward the tab bar, positive is toward content.  A color string is used
as is.  nil matches the active tab, as before this option existed."
  :type '(choice (const :tag "Match the active tab" nil)
                 (float :tag "Shift fraction off the active tab")
                 color)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-colors 'groups
  "What to color out of `tab-bar-theme-color-faces'.

`groups'  each tab group's label
`tabs'    each tab itself — for a workflow of one tab per project and
          no groups
t         both
nil       neither

Both draw from `tab-bar-theme-color-faces'."
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
  "The colors a tab can be given, one per slot.
Only their foreground is read.  A tab that is not current takes the
same color dimmed, per `tab-bar-theme-tab-inactive-blend'.  A tab takes one
by its place in the row, by what `tab-bar-theme-tab-color-face-function' answers
for it, or by hand out of this list — see
\\[tab-bar-theme-set-tab-color]."
  :type '(repeat face)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-color-face-function nil
  "Function returning the face a tab is colored with, or nil for none.
Called with one argument, the tab.  A tab it answers nil for falls
back to its own group's color, and is left uncolored only past that
too.  Nil here colors every tab by its place in the row.  Either way
`tab-bar-theme-tab-colors' has to admit tabs, whose default is groups
alone.

A face put on one tab by \\[tab-bar-theme-set-tab-color] outranks this
function, so a policy need not keep room for exceptions.  Two helpers
turn a key into the face it always answers with:

  `tab-bar-theme-color-from-key'
        keyed on any string that outlives the tab
  `tab-bar-theme-color-from-worktrees-family'
        keyed on the repository a directory was cut from, so a checkout
        and every worktree of it share one color

Which key a tab stands for is the caller's to know:

  (setq tab-bar-theme-tab-color-face-function
        (lambda (tab)
          (when-let* ((dir (my-tab-directory tab)))
            (tab-bar-theme-color-from-worktrees-family dir))))"
  :type '(choice (const :tag "None — color by position in the row" nil)
                 function)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-inactive-blend 0.55
  "How much of its color a tab that is not current keeps.
A number is that fraction of the way from the tab's background toward
the color the thing would have had: its label toward the color that
names it, its bar toward the color of the label.

The bar takes the fraction twice — it is dimmed for sitting on a tab
that is not current, and again for being a rail rather than a name,
which is one reason more than the label has.  The bar of the current
tab is dimmed neither way; there it is the mark that names the tab, and
`tab-bar-theme-indicator-blend' says how far.

A string or a face is taken as it is for both; nil dims nothing.  See
`tab-bar-theme--resolve-color'."
  :type '(choice (const :tag "No dimming" nil)
                 (float :tag "Fraction of the way from the tab's background")
                 color face)
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-name-padding " "
  "String used to pad each tab name on both sides."
  :group 'tab-bar-theme
  :type 'string)

(defcustom tab-bar-theme-separator " "
  "What spaces the elements of `tab-bar-format' apart.
A string is used as it is; a number is that many pixels, where a string
can only be whole characters, some seven pixels at a time.  A terminal
gets one space whatever the value, because it draws no part of a cell —
which is the other half of what this buys over naming a propertized
string in the format directly.

Not `tab-bar-separator', which prefixes every tab.  All the tabs come
out of one element of the format, so what sets them apart is that
variable, or `tab-bar-theme-tab-name-padding' inside their labels."
  :type '(choice (string :tag "String")
                 (integer :tag "Pixels"))
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-extra-face-function nil
  "Function returning one more face to give a tab, or nil for none.
Called with a single TAB, in the manner of `tab-bar-tab-face-function'.
The face is merged ahead of the base tab face, so it has the last word
on whatever attributes it sets — except the foreground, which the tab's
own color keeps.

What earns a tab a face of its own is none of a theme's business, so
it is asked rather than decided here.  To embolden the tabs bound to a
project, for instance:

    (setq tab-bar-theme-tab-extra-face-function
          (lambda (tab) (when (my-tab-project tab) \\='bold)))"
  :type '(choice function (const nil))
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-tab-group-format-function
  #'tab-bar-theme-tab-group-format-color
  "Function formatting a tab group's own name.
Shaped like `tab-bar-tab-group-format-function', which
`tab-bar-theme-mode' sets to this variable's value, not to
`tab-bar-theme-tab-group-format-color' directly — so a config that
wants to draw more than that, a bar on top of it say, keeps its own
function through every theme change instead of losing it back to the
default:

    (defun +tab-bar-group-format (tab i &optional current-p)
      (let ((face (tab-bar-theme-tab-group-face current-p)))
        (concat (tab-bar-indicator-build
                 (tab-bar-theme-tab-group-indicator-color current-p) nil face)
                (tab-bar-theme-tab-group-format-color tab i current-p))))
    (setq tab-bar-theme-tab-group-format-function
          #\\='+tab-bar-group-format)"
  :type 'function
  :group 'tab-bar-theme)

(defcustom tab-bar-theme-indicator-blend 0.8
  "Color of the indicator bar on the current tab.
A number is that fraction of the way from the tab's own background
toward the color that names it; a string or a face is taken as it is;
nil is that color undimmed.  Mixing out of the tab's own colors keeps
the bar in key whatever the theme, where one fixed color is loud on a
light theme and lost on a dark one.  See `tab-bar-theme--resolve-color'."
  :type '(choice (const :tag "The color itself" nil)
                 (float :tag "Fraction of the way from the tab's background")
                 color face)
  :group 'tab-bar-theme)

(defun tab-bar-theme-separator ()
  "Return `tab-bar-theme-separator' as a string, to name in `tab-bar-format'.
Named there the way `tab-bar-separator' is, being a variable and a
function of one name too."
  (pcase tab-bar-theme-separator
    ((and (pred numberp) px)
     ;; A terminal draws no part of a cell, and modern-tab reports that
     ;; a width spec in the format can leave the whole row unpainted
     ;; there (measured on Emacs 30.2 under tmux).
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
A no-op with `tab-bar-theme-mode' off, so callers may list this in
`tab-bar-tab-name-format-functions' unconditionally: it belongs ahead
of `tab-bar-tab-name-format-face', so the tab's own face covers the
padding too, and ahead of `tab-bar-indicator-tab-name-format', so the
bar ends up at the tab's left edge rather than inside it."
  (if tab-bar-theme-mode
      (concat tab-bar-theme-tab-name-padding
              name
              tab-bar-theme-tab-name-padding)
    name))

(defun tab-bar-theme--at-slot (sequence i)
  "Return the element of SEQUENCE standing for tab number I.
The rotation wraps, so the ninth tab takes the first slot again."
  (let ((n (length sequence)))
    (when (> n 0)
      (elt sequence (mod (1- i) n)))))

(defun tab-bar-theme--layer (over base)
  "Return a face spec applying every face in OVER ahead of BASE.
Each entry in OVER wins its own attributes; BASE supplies whatever
they leave unset."
  (append over (list base)))

;;;###autoload
(defun tab-bar-theme-tab-group-face (&optional current-p)
  "Return the base face a group's label takes in CURRENT-P state.
Named apart from `tab-bar-theme-tab-group-format-color' so a config
drawing more than that label — a bar ahead of it, say — can put its own
additions on the same background."
  (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive))

;;;###autoload
(defun tab-bar-theme-tab-group-format-color (tab i &optional current-p)
  "Format TAB group name for tab index I and CURRENT-P state.
Built on `tab-bar-tab-group-format-default', so the hint number, the
name and the face behind them follow whatever rule core follows — and
keep following it: which groups show a hint at all is core's to say,
and it has changed.

The group's own color covers the whole label, padding and hint along
with the name, the way `tab-bar-tab-name-format-face' covers a tab's."
  (let ((label (tab-bar-theme-tab-name-format-padding
                (tab-bar-tab-group-format-default tab i current-p)))
        ;; The label spec of the pair, as it comes: the same
        ;; `(:foreground COLOR)' a colored tab's own label takes.
        (spec (and (tab-bar-theme--tab-colors-p 'groups)
                   tab-bar-theme-color-faces
                   (cdr (tab-bar-theme--tab-pair tab current-p i 'groups)))))
    ;; Onto what core's default put there rather than over it, the way
    ;; `tab-bar-tab-name-format-face' adds a tab's own face: the color
    ;; ahead of everything, the group face behind, which this reaches the
    ;; padding with and the default never saw.
    (when spec
      (add-face-text-property 0 (length label) spec nil label))
    (add-face-text-property 0 (length label)
                            (tab-bar-theme-tab-group-face current-p) t label)
    label))

(defun tab-bar-theme-color-from-tab (tab)
  "Return the face kept on TAB itself, or nil for none.
Written by \\[tab-bar-theme-set-tab-color] as a tab parameter, so it
lives exactly as long as the tab."
  (alist-get 'tab-bar-theme-color tab))

(defvar tab-bar-theme--family-cache (make-hash-table :test #'equal)
  "The repository each project root was cut from, kept per root.
It is read from a file, and this runs for every tab of every
redisplay.")

;;;###autoload
(defun tab-bar-theme-worktrees-family (root)
  "Return the directory of the repository ROOT was cut from, or ROOT itself.
A git worktree keeps a `.git' file naming the repository it came from,
so every worktree of one project answers with one directory.  Read from
that file rather than asked of git: `magit-list-worktrees' answers the
same and costs 120 ms, where this costs 0.2 ms once per root."
  (with-memoization (gethash root tab-bar-theme--family-cache)
    (let ((dotgit (expand-file-name ".git" root)))
      ;; A directory name either way, so that ROOT reached with and
      ;; without its trailing slash is one family and one color.
      (file-name-as-directory
       (or (and (file-regular-p dotgit)
                (with-temp-buffer
                  (ignore-errors (insert-file-contents dotgit))
                  (goto-char (point-min))
                  (when (re-search-forward
                         "gitdir: \\(.+?\\)/\\(?:\\.git/\\)?worktrees/" nil t)
                    (match-string 1))))
           root)))))

(defvar tab-bar-theme--key-face-cache (make-hash-table :test #'equal)
  "The face each key answers with, kept per key and palette.
Keyed on the length of `tab-bar-theme-color-faces' as well, which is
all the answer depends on besides the key — nothing here comes from the
theme, so this outlives a theme load.")

(defun tab-bar-theme-color-from-key (key)
  "Return the face KEY always answers with, or nil where there is no KEY.
KEY is any string that names the thing being colored for as long as it
lives: a project's directory, the name of a tab group.  A hash and not
an assignment, so the same key keeps its color across sessions and
however often it comes back — at the price of two keys sometimes
landing on one slot while others stand free.  `secure-hash' rather than
`sxhash-equal', whose value Emacs does not promise to keep across
versions."
  (when (and key tab-bar-theme-color-faces)
    (with-memoization (gethash (cons key (length tab-bar-theme-color-faces))
                               tab-bar-theme--key-face-cache)
      ;; `--at-slot' wraps, so the hash needs no `mod' of its own.
      (tab-bar-theme--at-slot
       tab-bar-theme-color-faces
       (1+ (string-to-number (substring (secure-hash 'md5 key) 0 8) 16))))))

(defun tab-bar-theme-color-from-worktrees-family (dir)
  "Return the face the repository DIR was cut from always answers with."
  (when dir
    (tab-bar-theme-color-from-key
     (tab-bar-theme-worktrees-family dir))))

(defvar tab-bar-theme--last-color nil
  "The last answer of `tab-bar-theme--tab-color-face', as (TAB I WHAT FACE).
Every tab is asked twice over in a row — once for its label's face and
once for its bar — so one entry answers the second for free.")

(defun tab-bar-theme--tab-color-face (tab i what)
  "Return the face TAB is colored with, or nil where none is.
I is its place in the row and WHAT the axis of
`tab-bar-theme-tab-colors' being drawn.  A face put on the tab by hand
outranks `tab-bar-theme-tab-color-face-function', which outranks TAB's
own group's color: a member tab neither one names still shows which
group it belongs to.  A group is not a tab, and goes straight to its
own name, by neither a hand-set color nor a function about tabs."
  (let ((last tab-bar-theme--last-color))
    (if (and (eq tab (nth 0 last))
             (eql i (nth 1 last))
             (eq what (nth 2 last)))
        (nth 3 last)
      (let ((face (and (tab-bar-theme--tab-colors-p what)
                       (or (and (eq what 'tabs)
                                (or (tab-bar-theme-color-from-tab tab)
                                    (if tab-bar-theme-tab-color-face-function
                                        (funcall tab-bar-theme-tab-color-face-function tab)
                                      (tab-bar-theme--at-slot
                                       tab-bar-theme-color-faces (or i 1)))))
                           ;; Both axes end at the group's own name: a
                           ;; group is not a tab, so neither a color set
                           ;; on one of its tabs nor a function about
                           ;; tabs says anything about it — and a tab
                           ;; nothing else names still shows which group
                           ;; it belongs to.  The name and not I, which
                           ;; is the number of the group's first tab: a
                           ;; color off that would move when a tab is
                           ;; opened to the left of it.
                           (tab-bar-theme-color-from-key
                            (funcall tab-bar-tab-group-function tab))))))
        (setq tab-bar-theme--last-color (list tab i what face))
        face))))

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
taken from the tab's color; the base face keeps supplying the
active/inactive background.  That base comes from
`tab-bar-tab-group-face-default', the one default correct in both of
the slots this is installed in."
  (let ((over ()))
    (when-let* ((tab-bar-theme-tab-extra-face-function)
                (face (funcall tab-bar-theme-tab-extra-face-function tab)))
      (push face over))
    ;; Pushed second, so the tab's own color wins the foreground.
    (when-let* ((face (tab-bar-theme--tab-color-spec tab)))
      (push face over))
    (tab-bar-theme--layer over (tab-bar-tab-group-face-default tab))))

(defun tab-bar-theme--tab-color-spec (tab)
  "Return the label spec of TAB's own color, or nil where it has none.
The slot in the row is asked of `tab-bar-theme--last-color' before
`tab-bar--tab-index', which walks the frame's whole tab list to answer
the same thing: the indicator formatter runs one step ahead of the face
formatter in `tab-bar-tab-name-format-functions', so by here the slot
holds this very tab."
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
  "Keep FACE on the current tab, for as long as the tab lives.
It outranks `tab-bar-theme-tab-color-face-function'.  An empty answer takes the
color off again."
  (interactive
   (list (let ((name (completing-read "Face for this tab (empty for none): "
                                      tab-bar-theme-color-faces)))
           (unless (string-empty-p name) (intern name)))))
  ;; `tab-bar--current-tab-find' and not `tab-bar--current-tab': the
  ;; latter makes a fresh structure, and a color written onto that one
  ;; would be dropped on the floor.
  (setf (alist-get 'tab-bar-theme-color (cdr (tab-bar--current-tab-find))
                   nil :remove)
        face)
  (setq tab-bar-theme--last-color nil)
  (force-mode-line-update t))

(defvar tab-bar-theme--pair-cache (make-hash-table :test #'eq)
  "A (COLOR . FACE) pair for each face seen on a tab so far.
The color a bar would be drawn in and the spec its label takes, keyed
on the face and holding (CURRENT-PAIR . OTHER-PAIR).  Nil is a key like
any other: it is the pair of a tab left uncolored.")

(defun tab-bar-theme--inactive-spec ()
  "Return the blend an indicator that is not current is dimmed by.
Twice dimmed: see `tab-bar-theme-tab-inactive-blend'."
  (if (numberp tab-bar-theme-tab-inactive-blend)
      (* tab-bar-theme-tab-inactive-blend
         tab-bar-theme-tab-inactive-blend)
    tab-bar-theme-tab-inactive-blend))

(defvar tab-bar-theme--quiet-cache (make-hash-table :test #'eq)
  "The quiet tone an indicator takes on each face asked for so far.
Dropped along with the pairs, which is where those faces are set up
again — see `tab-bar-theme--drop-pairs'.")

(defun tab-bar-theme--quiet-color (face)
  "Return the one quiet tone an indicator takes on FACE.
Not `tab-bar-theme--build-pair' with no color, which answers for a
tab: with no blend to apply that one falls back to the tab\='s own
color rather than to the label\='s foreground, and it reads the two
faces a tab can take rather than whichever face is asked about.
Mixed out of FACE itself: a tone mixed against one background and drawn
on another comes out with however much contrast the two happen to
share."
  (with-memoization (gethash face tab-bar-theme--quiet-cache)
    (let ((bg (face-attribute face :background nil t))
          (fg (face-attribute face :foreground nil t))
          (spec (tab-bar-theme--inactive-spec)))
      (if spec (tab-bar-theme--resolve-color spec bg fg) fg))))

(defun tab-bar-theme--build-pair (current raw)
  "Return the (COLOR . FACE) pair of a tab in state CURRENT with color RAW.
COLOR is what an indicator bar for this tab would be drawn in — see
`tab-bar-theme-tab-indicator-color' — and FACE is what its label takes."
  (let* ((face (if current 'tab-bar-tab 'tab-bar-tab-inactive))
         (spec (if current
                   tab-bar-theme-indicator-blend
                 (tab-bar-theme--inactive-spec)))
         (bg (face-attribute face :background nil t))
         (fg (face-attribute face :foreground nil t))
         ;; A tab that is not current takes the same color dimmed, mixed
         ;; out of the face its label takes.  Nil asks
         ;; `--resolve-color' for no mix, so it dims nothing.
         (rot (if current
                  raw
                (and raw (tab-bar-theme--resolve-color
                          tab-bar-theme-tab-inactive-blend bg raw)))))
    (cons
     ;; The current tab's bar is mixed toward its own color, the same one
     ;; the label takes; a tab that is not current mixes toward the
     ;; label's own foreground instead, every inactive one the same tone
     ;; — telling tabs apart is the label's job, and the width already
     ;; marks where one inactive tab ends and the next begins.
     (if spec
         (tab-bar-theme--resolve-color spec bg (if current (or rot fg) fg))
       (or rot fg))
     (and (color-defined-p rot) (list :foreground rot)))))

(defun tab-bar-theme--face-color (face)
  "Return FACE's foreground, or nil where it names no color."
  (when-let* ((face)
              (color (face-attribute face :foreground nil t))
              ((color-defined-p color)))
    color))

(defun tab-bar-theme--drop-pairs ()
  "Forget every pair derived from the theme's colors."
  (clrhash tab-bar-theme--pair-cache)
  (clrhash tab-bar-theme--quiet-cache)
  (setq tab-bar-theme--last-color nil))

(defun tab-bar-theme--face-pair (face)
  "Return the (CURRENT-PAIR . OTHER-PAIR) FACE colors a tab with.
Nil for FACE gives the pair of a tab left uncolored.

Built on first sight of a face rather than for every slot up front: a
tab opened after the theme was set up carries a face nobody could have
known about, and one built at that point is one built once.  Out of
redisplay either way — derived per tab per command, this was some 44
percent of what a row of tabs cost, and grew with the square of the
number of tabs."
  (with-memoization (gethash face tab-bar-theme--pair-cache)
    (let ((raw (tab-bar-theme--face-color face)))
      (cons (tab-bar-theme--build-pair t raw)
            (tab-bar-theme--build-pair nil raw)))))

(defun tab-bar-theme--tab-pair (tab current i what)
  "Return the (COLOR . FACE) pair for TAB, in state CURRENT at slot I.
WHAT is the axis of `tab-bar-theme-tab-colors' being drawn.  A tab that
`tab-bar-theme--tab-color-face' answers nil for takes the pair of no
color: the same quiet bar color, and its label left to the theme's own
face."
  (let ((pair (tab-bar-theme--face-pair
               (tab-bar-theme--tab-color-face tab i what))))
    (if current (car pair) (cdr pair))))

;;;###autoload
(defun tab-bar-theme-tab-indicator-color (tab i)
  "Return the color TAB's indicator bar is drawn in at slot I, or nil.
Mixed the same way as the tab's own label, so the bar and the label
agree.  A nil TAB means no tab in particular — `tab-bar-indicator-separator'
asks that way — and answers with the same quiet tone an uncolored tab
gets, rather than reading tab parameters or asking a tab policy
function about a tab that does not exist.

Meant to be set as `tab-bar-indicator-color-function':

    (setq tab-bar-indicator-color-function #\\='tab-bar-theme-tab-indicator-color)"
  (if (not tab)
      (tab-bar-theme--quiet-color 'tab-bar-tab-inactive)
    (car (tab-bar-theme--tab-pair tab (eq (car tab) 'current-tab) i 'tabs))))

;;;###autoload
(defun tab-bar-theme-tab-group-indicator-color (&optional current-p)
  "Return the color a group's indicator bar is drawn in in CURRENT-P state.
The one quiet tone an indicator takes, mixed out of the face the
group's own label takes — one as loud as the current tab's, drawn ahead
of that tab, says nothing its label has not said already; and the group
carries its own color on the label, where
`tab-bar-theme-tab-group-format-color' puts it.  See
`tab-bar-theme-tab-group-format-function' for where this is meant to be
used."
  (tab-bar-theme--quiet-color (tab-bar-theme-tab-group-face current-p)))

(defun tab-bar-theme--shift-color (color fraction)
  "Shift COLOR by FRACTION, toward content when positive, away when negative.
\"Toward content\" means toward what an active element's background
looks like on the current frame; \"away\" means further into chrome.
Lightening is toward content on a light background but away from it
on a dark one — and vice versa for darkening — so a fixed sign keeps
its meaning across both, rather than one of them flooring an
already-dark or already-light color into no contrast at all."
  ;; No color to shift: see `tab-bar-theme--blend-color'.  Unshifted, then.
  (if (not (color-name-to-rgb color))
      color
    (let ((dark (eq (frame-parameter nil 'background-mode) 'dark))
          ;; `color-darken-name' and `color-lighten-name' take a percent.
          (percent (* 100 (abs fraction))))
      (if (eq (> fraction 0) dark)
          (color-darken-name color percent)
        (color-lighten-name color percent)))))

(defun tab-bar-theme--blend-color (from to fraction)
  "Return the color FRACTION of the way from FROM to TO, or nil for none.
Mixing toward the foreground beats shifting the background's own
lightness: `color-darken-name' and `color-lighten-name' hold the hue,
so a warm background gives a muddy tint of itself."
  ;; Nil where either end is no color this frame knows; see
  ;; `color-defined-p'.
  (when-let* ((a (color-name-to-rgb from))
              (b (color-name-to-rgb to)))
    (apply #'color-rgb-to-hex
           (append (color-blend b a fraction) '(2)))))

(defun tab-bar-theme--face-fg-or-bg (face)
  "Return FACE's foreground, or its background where that is FACE's color.
A face like `highlight' or `match' means itself through its background
and answers the same foreground `default' does — using that foreground
would just be `default' in another name, so its background is the
color FACE actually means."
  (let ((fg (face-foreground face nil 'default)))
    (if (equal fg (face-foreground 'default nil 'default))
        (face-background face nil 'default)
      fg)))

(defun tab-bar-theme--resolve-color (spec bg full)
  "Return the color SPEC names against the surface BG, or FULL for none.
A number is that fraction of the way from BG toward FULL; a string is
a color as it is; a face gives its foreground, or its background for a
face that means itself through that instead — see
`tab-bar-theme--face-fg-or-bg'.  FULL answers for anything else, and
wherever the frame leaves nothing to mix."
  ;; `stringp' comes before `facep' because `facep' accepts the *name*
  ;; of a face as a string, and `face-attribute' then signals on it: a
  ;; color of "shadow" or "default" would take down `load-theme', which
  ;; is what runs `tab-bar-theme--apply'.
  (or (pcase spec
        ((pred numberp) (tab-bar-theme--blend-color bg full spec))
        ((pred stringp) spec)
        ((pred facep) (tab-bar-theme--face-fg-or-bg spec)))
      full))

(defvar tab-bar-theme--overridden-faces nil
  "Faces this mode has given an override spec, to drop again.
Recorded as they are set rather than listed by hand, so a face added to
the setup cannot be forgotten in the teardown.")

(defun tab-bar-theme--set-face (face spec)
  "Give FACE the override SPEC, and record it for the teardown."
  (face-spec-set face spec)
  (push face tab-bar-theme--overridden-faces))

(defun tab-bar-theme--drop-faces ()
  "Drop every override spec this mode set, and forget them.
Called before each apply as well as on teardown: a face dropped from an
option between two theme loads would otherwise keep its override for
the rest of the session."
  (dolist (face tab-bar-theme--overridden-faces)
    (face-spec-set face nil 'face-override-spec))
  (setq tab-bar-theme--overridden-faces nil))

(defun tab-bar-theme--shifted (color spec)
  "Return COLOR shifted per SPEC, the shape both shift options take.
A number is a fraction to shift COLOR's own lightness by, a string is a
color to use as it is, and nil is COLOR itself."
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
         ;; Computed, not taken from the theme's own
         ;; `tab-bar-tab-inactive': that background is drawn against the
         ;; theme's `tab-bar', where the strip here comes from
         ;; `mode-line-inactive' — and half of a theme's design read
         ;; against the other half of ours put the loudest block of the
         ;; row on the tabs nobody is looking at.
         (bg-tab-inactive
          (tab-bar-theme--shifted bg-inactive
                                  tab-bar-theme-tab-inactive-background-shift))
         (bg-group-current
          (tab-bar-theme--shifted bg-active
                                  tab-bar-theme-tab-group-background-shift))
         ;; The room a box used to make above and below a label now
         ;; comes from the height of the indicator bar, which a box
         ;; would only stack on top of — so it is off on every face.
         (common `( :inherit unspecified :box nil
                    ,@(when tab-bar-theme-height
                        (list :height tab-bar-theme-height))))
         (dim `(,@common :background ,bg-tab-inactive
                         :foreground ,fg-inactive)))
    ;; Override specs rather than `custom-set-faces': these values are
    ;; computed from the current theme, so they have no business in a
    ;; custom file, and an override is what `tab-bar-theme--cleanup'
    ;; can drop again.
    ;;
    ;; Every value is named on every face, and no face here inherits
    ;; another of them — see `tab-line-theme--setup-base-faces' for the
    ;; inheritance cycle that rule avoids, and the theme that showed it.
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
  ;; `tab-bar-format-tabs-groups' renders every tab with
  ;; `tab-bar-tab-group-face-function' shadowing the above, so both
  ;; slots have to name the same function to style tabs either way —
  ;; the usage `tab-bar-tab-group-face-function' documents itself.
  (setq tab-bar-tab-group-face-function #'tab-bar-theme--tab-face)
  (setq tab-bar-tab-group-format-function tab-bar-theme-tab-group-format-function)
  (tab-bar-theme--drop-faces)
  (tab-bar-theme--setup-base-faces)
  ;; After the base faces: a pair's colors are mixed out of the face
  ;; its tab sits on, which is set there.
  (tab-bar-theme--drop-pairs))


(defconst tab-bar-theme--variables
  '( tab-bar-tab-face-function
     tab-bar-tab-group-face-function
     tab-bar-tab-group-format-function)
  "Variables `tab-bar-theme--apply' overwrites wholesale.
`tab-bar-tab-name-format-functions' is not among them: it is not
tab-bar-theme's to own at all, see `tab-bar-theme-tab-name-format-padding'.")

(defvar tab-bar-theme--saved nil
  "Alist of what `tab-bar-theme--variables' held before the mode.")

(defun tab-bar-theme--save-variables ()
  "Record what `tab-bar-theme--variables' hold, to be put back later."
  ;; `define-minor-mode' runs its body on every call and a nil argument
  ;; means enable, so a mode enabled twice would otherwise record the
  ;; values it set itself and never put the original ones back.
  (unless tab-bar-theme--saved
    (setq tab-bar-theme--saved
          (mapcar (lambda (sym) (cons sym (symbol-value sym)))
                  tab-bar-theme--variables))))

(defun tab-bar-theme--restore-variables ()
  "Put back what `tab-bar-theme--save-variables' recorded.
Non-nil where there was anything to put back: nothing recorded is a
mode that was never on."
  (when tab-bar-theme--saved
    (dolist (cell tab-bar-theme--saved)
      (set (car cell) (cdr cell)))
    (setq tab-bar-theme--saved nil)
    t))

(defun tab-bar-theme--cleanup ()
  "Undo `tab-bar-theme--apply'."
  (when (tab-bar-theme--restore-variables)
    (tab-bar-theme--drop-faces)
    ;; The colors were mixed out of the faces just dropped, and another
    ;; mode's formatter may still be asking for them.
    (tab-bar-theme--drop-pairs)))



;;;###autoload
(define-minor-mode tab-bar-theme-mode
  "Toggle `tab-bar-theme-mode'."
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

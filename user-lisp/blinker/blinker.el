;;; blinker.el --- Blink text on one timer -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: convenience, faces
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

;; Blink text, e.g. an indicator in a mode line or after a tab name,
;; for packages that show something in progress.  One timer serves all
;; of them, so everything blinks in step.  Blinking text is dimmed to
;; the color of `blinker-blend' and restored in turn.
;;
;; A package puts its text through `blinker-string' wherever it builds
;; it, and calls `blinker-start' with a function that redisplays the
;; text when the text appears:
;;
;;   (defvar my-process nil
;;     "Process shown as a blinking indicator while it runs.")
;;
;;   ;; In `mode-line-format': (:eval (my-indicator)).
;;   (defun my-indicator ()
;;     (and (process-live-p my-process)
;;          (blinker-string "●")))
;;
;;   (defun my-blink ()
;;     (when (process-live-p my-process)
;;       (force-mode-line-update)
;;       t))
;;
;;   ;; After starting the process:
;;   (blinker-start #'my-blink)
;;
;; Text of `blinker-string' inserted in a buffer blinks without a
;; redraw of the buffer: call `blinker-update-buffer' from the function.
;;
;; The timer runs only while a function returns non-nil and a frame has
;; focus.

;;; Code:

(require 'color)
(require 'seq)
(require 'text-property-search)


(defgroup blinker nil
  "Blinking text."
  :group 'display
  :prefix "blinker-")

(defcustom blinker-interval 0.5
  "Seconds between blinks.
Applied when blinking starts."
  :type 'number
  :group 'blinker)

(defcustom blinker-blend 0.15
  "Color of blinking text while dimmed.
A number is the fraction of the way from the background toward the
foreground; a string is a color used as is; nil hides the text."
  :type '(choice (const :tag "Hide" nil)
                 (float :tag "Fraction of the way from the background")
                 color)
  :group 'blinker)


(defvar blinker--functions nil
  "Functions that redisplay blinking text, see `blinker-start'.")

(defvar blinker--timer nil
  "Blink timer, or nil.")

(defvar blinker--dimmed nil
  "Non-nil while blinking text is dimmed.")

;;;###autoload
(defun blinker-dimmed-p ()
  "Return non-nil while blinking text is dimmed."
  blinker--dimmed)

(defun blinker--face ()
  "Return the face of dimmed text, or nil to hide it.
See `blinker-blend'.  Nil too if a color of the `default' face is
unknown."
  (pcase blinker-blend
    ((and (pred stringp) color) `(:foreground ,color))
    ((and (pred numberp) fraction)
     (when-let* ((foreground (color-name-to-rgb
                              (face-foreground 'default nil t)))
                 (background (color-name-to-rgb
                              (face-background 'default nil t))))
       `(:foreground
         ,(apply #'color-rgb-to-hex
                 (append (color-blend foreground background fraction)
                         '(2))))))))

(defun blinker--dim (start end &optional object)
  "Dim the text between START and END in OBJECT.
Hide it if `blinker--face' returns nil.  Return what `blinker--restore'
needs to undo it: the face added, or `hidden'."
  (if-let* ((face (blinker--face)))
      (progn (add-face-text-property start end face nil object)
             face)
    ;; A blank of the same width, so the text after it does not jump.
    (put-text-property
     start end 'display
     (make-string (string-width (if (stringp object)
                                    (substring object start end)
                                  (buffer-substring start end)))
                  ?\s)
     object)
    'hidden))

(defun blinker--restore (start end dimmed)
  "Undo `blinker--dim' between START and END of the current buffer.
DIMMED is what it returned."
  (if (eq dimmed 'hidden)
      (put-text-property start end 'display nil)
    (let ((face (get-text-property start 'face)))
      (put-text-property start end 'face
                         (and (not (eq face dimmed)) (remq dimmed face))))))

;;;###autoload
(defun blinker-string (string)
  "Return a copy of STRING that blinks: dimmed while `blinker-dimmed-p'.
Build the text anew on each redisplay, or insert it in a buffer and call
`blinker-update-buffer' on each blink."
  (let ((string (copy-sequence string))
        (end (length string)))
    ;; The mark is t, or while dimmed what `blinker--dim' returned.
    (put-text-property 0 end 'blinker
                       (or (not blinker--dimmed)
                           (blinker--dim 0 end string))
                       string)
    string))

;;;###autoload
(defun blinker-update-buffer (buffer)
  "Dim or restore the text of `blinker-string' in BUFFER.
This does not redraw the buffer.  For the function of `blinker-start'."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (with-silent-modifications
        (let (match)
          (while (setq match (text-property-search-forward 'blinker))
            (let ((start (prop-match-beginning match))
                  (end (prop-match-end match))
                  (mark (prop-match-value match)))
              (cond ((and blinker--dimmed (eq mark t))
                     (put-text-property start end 'blinker
                                        (blinker--dim start end)))
                    ((not (or blinker--dimmed (eq mark t)))
                     (blinker--restore start end mark)
                     (put-text-property start end 'blinker t))))))))))


(defun blinker--run ()
  "Call `blinker--functions' and drop those that return nil.
Return the rest."
  ;; Not `seq-filter': a function may call `blinker-start'.
  (dolist (function blinker--functions)
    (unless (funcall function)
      (setq blinker--functions (delq function blinker--functions))))
  blinker--functions)

(defun blinker--focused-p ()
  "Return non-nil if a frame has focus or may have it."
  (seq-some #'frame-focus-state (frame-list)))

(defun blinker--stop ()
  "Cancel the blink timer and restore blinking text."
  (when blinker--timer
    (cancel-timer blinker--timer)
    (setq blinker--timer nil))
  (when blinker--dimmed
    (setq blinker--dimmed nil)
    (blinker--run))
  (unless blinker--functions
    (remove-function after-focus-change-function #'blinker--focus-change)))

(defun blinker--blink ()
  "Dim or restore blinking text; stop if there is none."
  (setq blinker--dimmed (not blinker--dimmed))
  (unless (blinker--run)
    (blinker--stop)))

(defun blinker--start-timer ()
  "Start the blink timer unless it runs or no frame has focus."
  (when (and (not blinker--timer) (blinker--focused-p))
    (setq blinker--timer
          (run-with-timer blinker-interval blinker-interval
                          #'blinker--blink))))

(defun blinker--focus-change ()
  "Stop blinking if no frame has focus, else start it.
For `after-focus-change-function'."
  (if (blinker--focused-p)
      (blinker--start-timer)
    (blinker--stop)))

;;;###autoload
(defun blinker-start (function)
  "Blink the text that FUNCTION redisplays.
Call FUNCTION with no arguments on each blink, and with the text
restored when Emacs loses focus.  It redisplays its text of
`blinker-string' and returns non-nil if it has any; once it returns
nil, it is not called again.  Call this when such text appears."
  (unless (memq function blinker--functions)
    (push function blinker--functions))
  (add-function :after after-focus-change-function #'blinker--focus-change)
  (blinker--start-timer))

(provide 'blinker)
;;; blinker.el ends here

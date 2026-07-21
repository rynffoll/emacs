;;; popframe.el --- Toggle any buffer in a centered child frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (posframe "1.4.0"))
;; Keywords: convenience, frames
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

;; Toggle a centered, focused child frame, built on posframe.
;;
;; The frame is created lazily on the first `popframe-toggle' from
;; `popframe-buffer-function' (a buffer, a buffer name, or a function
;; returning one; a function is called inside `save-window-excursion', so
;; providers with window side effects like `ghostel-project' work as-is):
;;
;;   (setq popframe-buffer-function #'ghostel-project)
;;
;; The frame is then reused: each time it is shown `popframe-buffer-function'
;; is re-resolved and its buffer swapped into the frame's window, while the
;; frame itself persists.  Bind the single command `popframe-toggle' to a key.

;;; Code:

;; posframe is required lazily in `popframe--show' so byte-compilation does
;; not depend on the ELPA package being on `load-path' yet.
(declare-function posframe-show "posframe" (buffer-or-name &rest args))
(declare-function posframe-poshandler-frame-center "posframe" (info))


(defgroup popframe nil
  "Toggle any buffer in a centered child frame."
  :group 'convenience)

(defcustom popframe-buffer-function nil
  "Buffer shown by `popframe-toggle'.
A buffer, a buffer name, or a function returning one.  A function is
called inside `save-window-excursion'."
  :type '(choice (const :tag "None" nil) function string))

(defcustom popframe-width-ratio 0.8
  "Child frame width as a fraction of the parent frame width."
  :type 'number)

(defcustom popframe-height-ratio 0.7
  "Child frame height as a fraction of the parent frame height."
  :type 'number)

(defcustom popframe-override-parameters nil
  "Extra frame parameters for the child frame, as an alist.
Passed verbatim to posframe's :override-parameters.  For example,
`((alpha . 90))' sets the frame opacity."
  :type '(alist :key-type symbol :value-type sexp))


(defvar popframe--frame nil
  "The child frame created by `popframe-toggle', or nil when none.")

(defun popframe--live-frame ()
  "Return `popframe--frame' if it is live, clearing a stale reference."
  (if (frame-live-p popframe--frame)
      popframe--frame
    (setq popframe--frame nil)))

(defun popframe--resolve (spec)
  "Resolve SPEC to a live buffer, or nil.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'."
  (get-buffer
   (if (functionp spec)
       (save-window-excursion (funcall spec))
     spec)))

(defun popframe--hide (frame)
  "Make FRAME invisible and return focus to its parent frame."
  (let ((parent (frame-parameter frame 'parent-frame)))
    (make-frame-invisible frame)
    (when (frame-live-p parent)
      (select-frame-set-input-focus parent))))

(defun popframe--fit (frame)
  "Resize FRAME to the configured ratios of its parent and re-center it.
Resizes only when the target character size differs from the current one,
then always re-centers, so a parent resized while FRAME was hidden (for
example toggling fullscreen) does not leave FRAME oversized or off-center."
  (let* ((parent (frame-parameter frame 'parent-frame))
         (parent (if (frame-live-p parent) parent (selected-frame)))
         (cols  (round (* (frame-width  parent) popframe-width-ratio)))
         (lines (round (* (frame-height parent) popframe-height-ratio))))
    (unless (and (= cols  (frame-width  frame))
                 (= lines (frame-height frame)))
      (set-frame-size frame cols lines))
    (set-frame-position
     frame
     (max 0 (/ (- (frame-pixel-width  parent) (frame-pixel-width  frame)) 2))
     (max 0 (/ (- (frame-pixel-height parent) (frame-pixel-height frame)) 2)))))

(defun popframe--reveal (frame buffer)
  "Show BUFFER in FRAME, make it visible, and give it input focus.
Reuses FRAME's window (only its buffer changes).  The window is dedicated
\(so killing its buffer deletes the frame), so dedication is lifted around
the buffer swap and restored afterwards.  FRAME is refit to its parent
first, in case the parent was resized while FRAME was hidden."
  (let ((win (frame-root-window frame)))
    (set-window-dedicated-p win nil)
    (set-window-buffer win buffer)
    (set-window-dedicated-p win t)
    (popframe--fit frame)
    (make-frame-visible frame)
    (select-frame-set-input-focus frame)))

(defun popframe--create (buffer)
  "Create and show a centered child frame displaying BUFFER.
Store it in `popframe--frame' and focus it."
  (posframe-show buffer
                 :poshandler #'posframe-poshandler-frame-center
                 :width  (round (* (frame-width)  popframe-width-ratio))
                 :height (round (* (frame-height) popframe-height-ratio))
                 :override-parameters popframe-override-parameters
                 :left-fringe 8
                 :right-fringe 8
                 ;; :internal-border-width 3
                 ;; :internal-border-color (face-background 'region nil t)
                 :respect-mode-line t
                 :respect-header-line t
                 :accept-focus t
                 :cursor t
                 :window-point (with-current-buffer buffer (point)))
  (let ((frame (buffer-local-value 'posframe--frame buffer)))
    (setq popframe--frame frame)
    ;; posframe marks BUFFER as its own via a buffer-local `posframe--frame',
    ;; and `posframe-delete-all' kills every buffer so marked.  We display
    ;; real buffers we must not kill and track the frame ourselves, so drop
    ;; the mark.  posframe still finds the frame by its `posframe-buffer'
    ;; frame parameter, so `posframe-hide' etc. keep working.
    (with-current-buffer buffer (kill-local-variable 'posframe--frame))
    (select-frame-set-input-focus frame)))


;;;###autoload
(defun popframe-toggle ()
  "Toggle the popframe child frame.
The frame is created lazily on first use and reused afterwards: each
time it is shown, `popframe-buffer-function' is re-resolved and its
buffer swapped into the frame's window, while the frame itself persists."
  (interactive)
  (require 'posframe)
  (let ((frame (popframe--live-frame)))
    (if (and frame (frame-visible-p frame))
        (popframe--hide frame)
      (let ((buffer (popframe--resolve popframe-buffer-function)))
        (unless (buffer-live-p buffer)
          (user-error "popframe: `popframe-buffer-function' yielded no buffer"))
        (if frame
            (popframe--reveal frame buffer)
          (popframe--create buffer))))))

(provide 'popframe)
;;; popframe.el ends here

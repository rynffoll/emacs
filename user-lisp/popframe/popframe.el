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

;; Toggle any buffer in a centered, focused child frame, built on posframe.
;;
;; Which buffer to pop is chosen by `popframe-buffer-function': a buffer,
;; a buffer name, or a function returning one.  A function is called inside
;; `save-window-excursion', so buffer providers with window side effects
;; (e.g. `ghostel-project') can be used as-is:
;;
;;   (setq popframe-buffer-function #'ghostel-project)
;;
;; Then bind the single command `popframe-toggle'.  Toggling while visible
;; hides the frame; toggling a different buffer replaces it.

;;; Code:

;; posframe is required lazily in `popframe--show' so byte-compilation does
;; not depend on the ELPA package being on `load-path' yet.
(declare-function posframe-show "posframe" (buffer-or-name &rest args))
(declare-function posframe-hide "posframe" (buffer-or-name))
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


(defvar popframe--buffer nil
  "Buffer currently shown in the popframe, if any.")

(defun popframe--resolve (spec)
  "Resolve SPEC to a live buffer, or nil.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'."
  (get-buffer
   (if (functionp spec)
       (save-window-excursion (funcall spec))
     spec)))

(defun popframe--frame (buffer)
  "Return the child frame BUFFER is displayed in, or nil.
Reads posframe's buffer-local `posframe--frame': posframe exposes no
public accessor, so this is the de-facto seam its own code uses too."
  (buffer-local-value 'posframe--frame buffer))

(defun popframe--visible-p (buffer)
  "Non-nil when BUFFER's child frame is live and visible."
  (when-let* ((frame (popframe--frame buffer)))
    (and (frame-live-p frame) (frame-visible-p frame))))

(defun popframe--hide (buffer)
  "Hide BUFFER's child frame and return focus to its parent frame."
  (let* ((frame  (popframe--frame buffer))
         (parent (and (frame-live-p frame)
                      (frame-parameter frame 'parent-frame))))
    (posframe-hide buffer)
    (when (frame-live-p parent)
      (select-frame-set-input-focus parent))))

(defun popframe--show (buffer)
  "Show BUFFER in a centered, focused child frame."
  (posframe-show buffer
                 :poshandler #'posframe-poshandler-frame-center
                 :width  (round (* (frame-width)  popframe-width-ratio))
                 :height (round (* (frame-height) popframe-height-ratio))
                 :left-fringe 8
                 :right-fringe 8
                 :internal-border-width 3
                 :internal-border-color (face-background 'region nil t)
                 :respect-mode-line t
                 :respect-header-line t
                 :accept-focus t
                 :cursor t
                 :window-point (with-current-buffer buffer (point)))
  (select-frame-set-input-focus (popframe--frame buffer)))


;;;###autoload
(defun popframe-toggle ()
  "Toggle a centered child frame showing `popframe-buffer-function'."
  (interactive)
  (require 'posframe)
  (let ((buffer (popframe--resolve popframe-buffer-function)))
    (unless (buffer-live-p buffer)
      (user-error "popframe: `popframe-buffer-function' yielded no buffer"))
    (cond
     ((popframe--visible-p buffer)
      (popframe--hide buffer))
     (t
      (when (and popframe--buffer
                 (buffer-live-p popframe--buffer)
                 (not (eq popframe--buffer buffer)))
        (posframe-hide popframe--buffer))
      (popframe--show buffer)
      (setq popframe--buffer buffer)))))

(provide 'popframe)
;;; popframe.el ends here

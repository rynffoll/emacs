;;; project-x.el --- Project extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

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

;; Project extensions built on top of project.el.
;; Provides per-project named layouts via `project-x-layouts'.
;; Each layout is a function or a list of functions arranging windows.
;; When more than one layout is defined, `project-x-open-layout' prompts
;; for the one to open.  Can be set per-project via .dir-locals.el.

;;; Code:

(require 'project)
(require 'seq)

(defgroup project-x nil
  "Project extensions."
  :group 'project)


(defun project-x--layout-value-p (layout)
  "Return non-nil when LAYOUT is a function or a list of functions."
  (or (functionp layout)
      (and (listp layout) (seq-every-p #'functionp layout))))

(defcustom project-x-layouts nil
  "Named layouts for projects.
An alist mapping a layout NAME (a string) to a LAYOUT, where LAYOUT is
either a function or a list of functions called in order to build the
layout.  Can be set per-project via .dir-locals.el."
  :type '(alist :key-type string
                :value-type (choice function (repeat function)))
  :group 'project-x
  :safe (lambda (v)
          (and (listp v)
               (seq-every-p
                (lambda (entry)
                  (and (consp entry)
                       (stringp (car entry))
                       (project-x--layout-value-p (cdr entry))))
                v))))


(defun project-x--run-layout (layout)
  "Run LAYOUT, a function or a list of functions."
  (cond
   ((functionp layout) (funcall layout))
   ((listp layout) (mapc #'funcall layout))))

;;;###autoload
(defun project-x-open-layout (&optional name)
  "Open a project layout from `project-x-layouts'.
With a single layout defined, open it directly.  With several, prompt
for NAME.  With a prefix argument, always prompt."
  (interactive
   (list (when (or current-prefix-arg (cdr project-x-layouts))
           (completing-read "Layout: " (mapcar #'car project-x-layouts)
                            nil t))))
  (let* ((name (or name (caar project-x-layouts)))
         (layout (cdr (assoc name project-x-layouts))))
    (project-x--run-layout layout)))

(provide 'project-x)
;;; project-x.el ends here

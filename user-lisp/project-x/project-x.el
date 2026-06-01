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
;; Provides per-project layout support via `project-x-layout'.
;; Set `project-x-layout' in .dir-locals.el to a function or list of functions.

;;; Code:

(require 'project)

(defgroup project-x nil
  "Project extensions."
  :group 'project)


(defcustom project-x-layout nil
  "Layout for the current project.
Can be a function or a list of functions called in order.
Set per-project via .dir-locals.el."
  :type '(choice (const nil) function (repeat function))
  :group 'project-x
  :safe (lambda (v) (or (functionp v) (and (listp v) (seq-every-p #'functionp v)))))


;;;###autoload
(defun project-x-open-layout ()
  "Open project layout defined by `project-x-layout'."
  (interactive)
  (cond
   ((functionp project-x-layout)
    (funcall project-x-layout))
   ((listp project-x-layout)
    (mapc #'funcall project-x-layout))))

(provide 'project-x)
;;; project-x.el ends here

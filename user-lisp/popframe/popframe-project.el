;;; popframe-project.el --- Per-project popframe tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
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

;; Group popframe tabs by project:
;;
;;   (setq popframe-pin-key-function #'popframe-project-pin-key)

;;; Code:

(require 'project)


;;;###autoload
(defun popframe-project-pin-key ()
  "Return the current project's root, or nil outside a project.
For `popframe-pin-key-function'."
  (when-let* ((project (project-current)))
    (project-root project)))

(provide 'popframe-project)
;;; popframe-project.el ends here

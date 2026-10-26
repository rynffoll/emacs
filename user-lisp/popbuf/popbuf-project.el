;;; popbuf-project.el --- Per-project popbuf tabs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, frames, windows
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

;; Group popbuf tabs by project:
;;
;;   (setq popbuf-key-function #'popbuf-project-key)

;;; Code:

(require 'project)


;;;###autoload
(defun popbuf-project-key ()
  "Return the current project's root, or nil outside a project.
For `popbuf-key-function'."
  (when-let* ((project (project-current)))
    (project-root project)))

(provide 'popbuf-project)
;;; popbuf-project.el ends here

;;; project-ghostel.el --- Project Ghostel  -*- lexical-binding: t; -*-

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

;; Integration `ghostel' and `project'

;;; Code:

(require 'project)
(require 'ghostel nil t)


(defgroup project-ghostel
  nil
  "Integration ghostel and project."
  :group 'project)



;;;###autoload
(defun project-ghostel ()
  "Open ghostel in the current project's root directory.
If a buffer already exists for running ghostel in the project's root,
switch to it.  Otherwise, create a new ghostel buffer.
With \\[universal-argument] prefix arg, create a new ghostel buffer even
if one already exists.
With numeric prefix arg, switch to the session with that number, or
create it if it doesn't already exist."
  (interactive)
  (defvar ghostel-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (ghostel-buffer-name (project-prefixed-buffer-name "ghostel")))
    (ghostel current-prefix-arg)))

(provide 'project-ghostel)
;;; project-ghostel.el ends here

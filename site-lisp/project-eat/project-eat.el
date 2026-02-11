;;; project-eat.el --- Project Eat  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ruslan Kamashev

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

;; Integration `eat' and `project'

;;; Code:

(require 'project)
(require 'eat)


(defgroup project-eat
  nil
  "Integration eat and project."
  :group 'project)



;;;###autoload
(defun project-eat ()
  "Open Eat in the current project's root directory.
If a buffer already exists for running Eat in the project's root,
switch to it.  Otherwise, create a new Eat buffer.
With \\[universal-argument] prefix arg, create a new Eat buffer even
if one already exists.
With numeric prefix arg, switch to the session with that number, or
create it if it doesn't already exist."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (eat-buffer-name (project-prefixed-buffer-name "eat")))
    (eat nil current-prefix-arg)))

(provide 'project-eat)
;;; project-eat.el ends here

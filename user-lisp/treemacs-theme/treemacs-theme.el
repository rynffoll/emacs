;;; treemacs-theme.el --- Treemacs Theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ruslan Kamashev

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

;; Theme for `treemacs'

;;; Code:

(require 'treemacs nil t)
(require 'all-the-icons nil t)


(defgroup treemacs-theme
  nil
  "Treemacs Theme."
  :group 'treemacs)



(treemacs-create-theme "Icons"
  :config
  (progn
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2))
	 :extensions (root-open))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2))
	 :extensions (root-closed))

	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-directory" :v-adjust 0))
	 :extensions (dir-open))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-directory" :v-adjust 0))
	 :extensions (dir-closed))

	(treemacs-create-icon
	 :icon (format "  %s " (all-the-icons-octicon "tag" :v-adjust 0))
	 :extensions (tag-leaf))
	(treemacs-create-icon
	 :icon (format "%s %s "
				   (all-the-icons-octicon "chevron-down" :v-adjust 0)
				   (all-the-icons-octicon "tag" :v-adjust 0))
	 :extensions (tag-open))
	(treemacs-create-icon
	 :icon (format "%s %s "
				   (all-the-icons-octicon "chevron-right" :v-adjust 0)
				   (all-the-icons-octicon "tag" :v-adjust 0))
	 :extensions (tag-closed))

	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "alert" :v-adjust 0 :face 'error))
	 :extensions (error))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "stop"  :v-adjust 0 :face 'warning))
	 :extensions (warning))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "info"  :v-adjust 0 :face 'success))
	 :extensions (info))

	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-text" :v-adjust 0))
	 :extensions ("md" "markdown" "rst" "log" "org" "txt"
				  "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-zip" :v-adjust 0))
	 :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"
				  "xz" "dmg" "iso"))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-binary" :v-adjust 0))
	 :extensions ("exe" "dll" "obj" "so" "o" "out" "elc"))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-pdf" :v-adjust 0))
	 :extensions ("pdf"))
	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-media" :v-adjust 0))
	 :extensions ("png" "jpg" "jpeg" "gif" "ico" "svg" "bmp"
				  "mov" "avi" "mp4" "webm" "mkv"
				  "wav" "mp3" "ogg" "midi"))

	(treemacs-create-icon
	 :icon (format "%s " (all-the-icons-octicon "file-code" :v-adjust 0))
	 :extensions (fallback))))

;;;###autoload
(defun treemacs-theme-setup ()
  "Enable the `Icons' theme for `treemacs'."
  (treemacs-load-theme "Icons"))

(provide 'treemacs-theme)
;;; treemacs-theme.el ends here

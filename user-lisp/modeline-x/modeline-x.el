;;; modeline-x.el --- Mode-line extras  -*- lexical-binding: t; -*-

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

;; Custom mode-line segments: evil state, buffer identification, VC branch,
;; position, and misc info.

;;; Code:

(declare-function nerd-icons-icon-for-buffer "nerd-icons" ())
(declare-function nerd-icons-devicon "nerd-icons" (icon &rest args))
(declare-function nerd-icons-mdicon "nerd-icons" (icon &rest args))
(defvar evil-state)
(defvar evil-mode-line-tag)


(defgroup modeline-x nil
  "Mode-line extras."
  :group 'mode-line)


(defface modeline-x-evil-normal
  '((t (:inherit shadow :weight normal :slant normal)))
  "Face for `evil-normal-state'."
  :group 'modeline-x)

(defface modeline-x-evil-insert
  '((t (:inherit success :weight normal :slant normal)))
  "Face for `evil-insert-state'."
  :group 'modeline-x)

(defface modeline-x-evil-visual
  '((t (:inherit font-lock-keyword-face :weight normal :slant normal)))
  "Face for `evil-visual-state'."
  :group 'modeline-x)

(defface modeline-x-evil-emacs
  '((t (:inherit font-lock-builtin-face :weight normal :slant normal)))
  "Face for `'evil-emacs-state'."
  :group 'modeline-x)

(defface modeline-x-evil-other
  '((t (:inherit shadow :weight normal :slant normal)))
  "Face for evil states (replace, motion, operator, ...)."
  :group 'modeline-x)


(defvar-local modeline-x-vc
  '(:eval
    (when (mode-line-window-selected-p)
      (format "%s%s"
              (nerd-icons-devicon "nf-dev-git_branch" :v-adjust 0.1)
              (string-trim vc-mode))))
  "VC branch segment with icon.
The `vc-mode' string format is described in `vc-default-mode-line-string'.")


(defvar-local modeline-x-position
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-position))
  "Position segment showing `mode-line-position' for the selected window.")


(defun modeline-x--buffer-id-face ()
  "Return face for buffer identification based on modified state."
  (cond
   ((and (buffer-file-name) (buffer-modified-p) (not buffer-read-only))
    (if (mode-line-window-selected-p)
        '(warning mode-line-buffer-id)
      'warning))
   ((mode-line-window-selected-p) 'mode-line-buffer-id)))

(defvar-local modeline-x-buffer-identification
  '(:eval
    (propertize
     ;; (format-mode-line mode-line-buffer-identification)
     (format-mode-line (propertized-buffer-identification "%b"))
     'face (modeline-x--buffer-id-face)))
  "Buffer name, colored when the buffer is modified.")


(defvar-local modeline-x-major-mode-icon
  '(:eval
    (if (mode-line-window-selected-p)
        (nerd-icons-icon-for-buffer :v-adjust 0.1)
      (nerd-icons-icon-for-buffer :v-adjust 0.1 :face 'shadow)))
  "Major-mode icon.")


(defvar-local modeline-x-winum
  '(:eval (winum-get-number-string))
  "Window number string.")

(defvar-local modeline-x-winum-icon
  '(:eval
    (let* ((num      (winum-get-number))
           (selected (mode-line-window-selected-p))
           (icon     (if (<= 1 num 10)
                         (if selected "nf-md-numeric_%d_circle" "nf-md-numeric_%d_circle_outline")
                       (if selected "nf-md-record_circle" "nf-md-record_circle_outline"))))
      (nerd-icons-mdicon (format icon num) :v-adjust 0.1 :face 'shadow)))
  "Window number icon.")


(defun modeline-x--evil-face ()
  "Return the modeline-x face for the current evil state."
  (pcase evil-state
    ('normal  'modeline-x-evil-normal)
    ('insert  'modeline-x-evil-insert)
    ('visual  'modeline-x-evil-visual)
    ('emacs   'modeline-x-evil-emacs)
    (_        'modeline-x-evil-other)))

(defun modeline-x--evil-icon ()
  "Return a nerd icon for the current evil state."
  (nerd-icons-mdicon
   (pcase evil-state
     ('normal   "nf-md-alpha_n_box")
     ('insert   "nf-md-alpha_i_box")
     ('visual   "nf-md-alpha_v_box")
     ('replace  "nf-md-alpha_r_box")
     ('operator "nf-md-alpha_o_box")
     ('motion   "nf-md-alpha_m_box")
     ('emacs    "nf-md-alpha_e_box")
     (_         "nf-md-alpha_n_box"))
   :v-adjust 0.1
   :face (modeline-x--evil-face)))

(defvar-local modeline-x-evil-state-tag
  '(:eval
    (propertize
     (string-trim evil-mode-line-tag)
     'face `(bold ,(modeline-x--evil-face))))
  "Evil modal state tag, colored by state.")

(defvar-local modeline-x-evil-state-icon
  '(:eval (modeline-x--evil-icon))
  "Evil modal state icon, colored by state.")


(defvar-local modeline-x-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line misc info, shown only for the selected window.")


(declare-function flymake-diagnostics "flymake" (&optional beg end))

(defvar-local modeline-x-flymake
  '(:eval
    (when (and (mode-line-window-selected-p)
               (flymake-diagnostics))
      flymake-mode-line-counters))
  "Flymake counters, shown only when diagnostics are present.")


(defun modeline-x-reset ()
  "Reset `mode-line-format' in all buffers to the current default."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'mode-line-format))))


(dolist (construct '(modeline-x-flymake
                     modeline-x-misc-info
                     modeline-x-vc
                     modeline-x-position
                     modeline-x-buffer-identification
                     modeline-x-major-mode-icon
                     modeline-x-winum
                     modeline-x-winum-icon
                     modeline-x-evil-state-tag
                     modeline-x-evil-state-icon))
  (put construct 'risky-local-variable t))


(provide 'modeline-x)
;;; modeline-x.el ends here

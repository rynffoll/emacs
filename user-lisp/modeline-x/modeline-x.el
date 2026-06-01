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
;; position, selection info, and misc info.

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
  "Face for evil normal state tag."
  :group 'modeline-x)

(defface modeline-x-evil-insert
  '((t (:inherit success :weight normal :slant normal)))
  "Face for evil insert state tag."
  :group 'modeline-x)

(defface modeline-x-evil-visual
  '((t (:inherit font-lock-keyword-face :weight normal :slant normal)))
  "Face for evil visual state tag."
  :group 'modeline-x)

(defface modeline-x-evil-emacs
  '((t (:inherit font-lock-builtin-face :weight normal :slant normal)))
  "Face for evil emacs state tag."
  :group 'modeline-x)

(defface modeline-x-evil-other
  '((t (:inherit shadow :weight normal :slant normal)))
  "Face for evil replace, motion, and operator state tags."
  :group 'modeline-x)



(defvar-local modeline-x-vc
  '(:eval
    (when (mode-line-window-selected-p)
      (concat " " (nerd-icons-devicon "nf-dev-git_branch") (string-trim vc-mode))))
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
    (propertize (format-mode-line mode-line-buffer-identification)
                'face (modeline-x--buffer-id-face)))
  "Buffer name, colored when the buffer has unsaved changes.")



(defvar-local modeline-x-major-mode-icon
  '(:eval (nerd-icons-icon-for-buffer))
  "Major-mode icon for use in `mode-line-format'.")



(defvar winum-format)

(defvar-local modeline-x-winum
  '(:eval (format winum-format (winum-get-number-string)))
  "Window number via winum, using `winum-format'.")



(defun modeline-x--evil-face (&optional state)
  "Return the modeline-x face for evil STATE."
  (pcase (or state evil-state)
    ('normal  'modeline-x-evil-normal)
    ('insert  'modeline-x-evil-insert)
    ('visual  'modeline-x-evil-visual)
    ('emacs   'modeline-x-evil-emacs)
    (_        'modeline-x-evil-other)))

(defun modeline-x--evil-icon ()
  "Return a nerd icon for the current evil state."
  (nerd-icons-mdicon
   (pcase evil-state
     ('normal   "nf-md-alpha_n_circle")
     ('insert   "nf-md-alpha_i_circle")
     ('visual   "nf-md-alpha_v_circle")
     ('replace  "nf-md-alpha_r_circle")
     ('operator "nf-md-alpha_o_circle")
     ('motion   "nf-md-alpha_m_circle")
     ('emacs    "nf-md-alpha_e_circle")
     (_         "nf-md-alpha_n_circle"))
   :face (modeline-x--evil-face)))

(defvar-local modeline-x-evil-state
  '(:eval (propertize evil-mode-line-tag 'face (modeline-x--evil-face)))
  "Evil modal state tag, colored by state.")

(defvar-local modeline-x-evil-state-icon
  '(:eval (concat " " (modeline-x--evil-icon) " "))
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


(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)

(defvar-local modeline-x-selection-info
  '(:eval
    (when (and (mode-line-window-selected-p)
               (or mark-active
                   (and (bound-and-true-p evil-local-mode)
                        (eq evil-state 'visual))))
      (cl-destructuring-bind (beg . end)
          (if (and (bound-and-true-p evil-local-mode)
                   (eq evil-state 'visual))
              (cons evil-visual-beginning evil-visual-end)
            (cons (region-beginning) (region-end)))
        (let ((lines (count-lines beg (min end (point-max)))))
          (propertize
           (concat " "
                   (cond
                    ((and (bound-and-true-p evil-visual-selection)
                          (eq evil-visual-selection 'line))
                     (format "%dL" lines))
                    ((> lines 1)
                     (format "%dC %dL" (- end beg) lines))
                    (t
                     (format "%dC" (- end beg))))
                   " ")
           'face 'success)))))
  "Selection info: chars/lines count for active region or evil visual state.")



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
                     modeline-x-evil-state
                     modeline-x-evil-state-icon
                     modeline-x-selection-info))
  (put construct 'risky-local-variable t))


(provide 'modeline-x)
;;; modeline-x.el ends here

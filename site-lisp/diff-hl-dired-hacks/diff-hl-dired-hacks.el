;;; diff-hl-dired-hacks.el --- Hacks for diff-hl-dired + dired-subtree  -*- lexical-binding: t; -*-

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

;; Three hacks for `diff-hl-dired' + `dired-subtree' interaction:
;;
;; Problem 1: stale .git/index.lock
;;   `diff-hl-dired-update' kills running git process on concurrent calls
;;   (e.g. rapid subtree expand), leaving .git/index.lock behind.
;;
;; Problem 2: flickering on subtree open
;;   `diff-hl-dired-update' calls `diff-hl-dired-clear' immediately,
;;   then starts async git — highlights disappear until git finishes.
;;
;; Problem 3: overlay leak on subtree close
;;   diff-hl overlays are zero-width (point, point). When
;;   `dired-subtree-remove' does `delete-region', they don't get deleted
;;   — they collapse and appear on the next visible line.
;;
;; Flow on subtree open (no concurrent git):
;;
;; 1. `diff-hl-dired-update' called → :around suppresses `diff-hl-dired-clear'
;;    (old highlights stay on screen)
;; 2. Async git starts, `let' binding ends → suppress-clear = nil
;; 3. Git finishes → `diff-hl-dired-highlight-items':
;;    a. :before advice → `diff-hl-dired-clear' (now allowed) → removes old
;;    b. original → draws new overlays
;;    Steps a+b are synchronous → no flicker
;;
;; Flow on concurrent calls (git busy):
;;
;; 1. New call arrives → git is running → set `diff-hl-dired-hacks--pending' flag
;;    (don't kill process, don't clear — old highlights stay)
;; 2. Git finishes → `diff-hl-dired-highlight-items' → draws + :after checks flag
;; 3. Flag set → `run-at-time' 0 → re-run on next event loop
;; 4. 10 rapid calls collapse into 2 git processes (flag is boolean, not counter)
;;
;; Flow on subtree close:
;;
;; 1. :before advice on `dired-subtree-remove' → removes diff-hl overlays
;;    in subtree region
;; 2. `delete-region' → lines deleted, nothing leaks

;;; Code:

(require 'diff-hl-dired)

(defvar-local diff-hl-dired-hacks--pending nil)
(defvar diff-hl-dired-hacks--suppress-clear nil)

(define-advice diff-hl-dired-update (:around (fn) queue-on-conflict)
  "Queue update instead of killing running git process.
Suppress `diff-hl-dired-clear' at start — it will run in
`diff-hl-dired-highlight-items' :before advice instead."
  (if (and (buffer-live-p diff-hl-dired-process-buffer)
           (get-buffer-process diff-hl-dired-process-buffer)
           (process-live-p (get-buffer-process diff-hl-dired-process-buffer)))
      (setq diff-hl-dired-hacks--pending t)
    (setq diff-hl-dired-hacks--pending nil)
    (let ((diff-hl-dired-hacks--suppress-clear t))
      (funcall fn))))

(define-advice diff-hl-dired-clear (:around (fn &rest args) suppress)
  "Skip clear when called from `diff-hl-dired-update' start."
  (unless diff-hl-dired-hacks--suppress-clear
    (apply fn args)))

(define-advice diff-hl-dired-highlight-items (:before (_alist) clear-before-highlight)
  "Clear old overlays right before drawing new ones (no flicker)."
  (diff-hl-dired-clear))

(define-advice diff-hl-dired-highlight-items (:after (_alist) flush-pending)
  "Re-run update if one was queued while process was running."
  (when diff-hl-dired-hacks--pending
    (setq diff-hl-dired-hacks--pending nil)
    (run-at-time 0 nil #'diff-hl-dired-update)))

(define-advice dired-subtree-remove (:before () clean-diff-hl-overlays)
  "Remove diff-hl overlays in subtree region before deletion."
  (when (bound-and-true-p diff-hl-dired-mode)
    (-when-let (ov (dired-subtree--get-ov))
      (diff-hl-remove-overlays (overlay-start ov) (overlay-end ov)))))

(provide 'diff-hl-dired-hacks)
;;; diff-hl-dired-hacks.el ends here

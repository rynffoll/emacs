;;; ai-buffer.el --- Buffers made by an AI agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1") (blinker "0.1"))
;; Keywords: tools
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

;; A base for buffers an AI agent generates to show something: a
;; dashboard of a running process, refreshed by `revert-buffer' and a
;; timer, or a one-off report.  The buffer is named "*ai: TITLE*", is
;; in `ai-buffer-mode' and keeps buffer-local:
;;
;; - `ai-buffer-render-function': inserts the contents for STATE;
;; - `ai-buffer-state-file': the JSON file read as STATE, or nil;
;; - `ai-buffer-refresh-interval': seconds between refreshes, or nil.
;;
;; `ai-buffer-display' sets them on each call:
;;
;;   (ai-buffer-display "reindex" #'my-reindex--render
;;                      :state-file "/tmp/reindex/state.json" :interval 5)
;;
;;   (ai-buffer-display "note" (lambda (_) (insert "Done.")))
;;
;; The render function inserts any contents, with the buffer's window
;; selected.  The mode re-renders when the window size changes.  The
;; header line shows the title and, on the right, the render time, the
;; refresh interval and a flag for an invalid state file; an indicator
;; blinks before them while the refresh timer runs.  A failed render
;; shows its error instead of the contents.  The timer skips a buffer
;; no window shows.
;;
;; `ai-buffer-color' and `ai-buffer-shade' take colors from faces, for
;; SVG rendering.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'blinker)


(defvar-local ai-buffer-render-function nil
  "Function inserting the contents at point in the empty buffer.
Called with STATE, the parsed `ai-buffer-state-file' or nil, while a
window showing the buffer, if any, is selected.")

(defvar-local ai-buffer-state-file nil
  "JSON file read as the state to render, or nil.
Objects become alists with symbol keys.")

(defvar-local ai-buffer-refresh-interval nil
  "Seconds between refreshes, or nil for no timer.")

(defvar-local ai-buffer--status nil
  "Right part of the header line, set on each refresh.")
;; Keep its `display' and `face' properties in the header line.
(put 'ai-buffer--status 'risky-local-variable t)

(defvar-local ai-buffer--timer nil
  "Refresh timer, or nil.")


(defun ai-buffer--state ()
  "Return the parsed `ai-buffer-state-file', or nil if missing.
Signal `json-error' if invalid."
  (when-let* ((file ai-buffer-state-file) ; buffer-local: read it here
              ((file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (json-parse-buffer :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil))))

(defun ai-buffer--format-status (state-error)
  "Return the status: render time, refresh interval, STATE-ERROR."
  (let* ((line
          (string-join
           (delq nil
                 (list (format-time-string "%T")
                       (and ai-buffer-refresh-interval
                            (format "every %ss" ai-buffer-refresh-interval))
                       (and state-error
                            (propertize "invalid state file"
                                        'face 'warning
                                        'help-echo (error-message-string
                                                    state-error)))))
           " · "))
         (indicator (and ai-buffer-refresh-interval
                         '((:eval (ai-buffer--indicator)) " "))))
    (add-face-text-property 0 (length line) 'shadow t line)
    (list (propertize " " 'display
                      ;; A column of margin; "● " is two more.
                      `(space :align-to (- right ,(+ 1 (if indicator 2 0)
                                                     (string-width line)))))
          indicator
          line)))

(defun ai-buffer--flush-images ()
  "Remove the images shown in the buffer from the image cache."
  ;; A new SVG on each refresh would otherwise pile up in the cache.
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let ((display (get-text-property pos 'display)))
        (when (eq (car-safe display) 'image)
          (image-flush display)))
      (setq pos (next-single-property-change pos 'display nil (point-max))))))

(defun ai-buffer--cancel-timer ()
  "Cancel the refresh timer of the current buffer."
  (when ai-buffer--timer
    (cancel-timer ai-buffer--timer)
    (setq ai-buffer--timer nil)))

(defun ai-buffer--tick (buffer)
  "Refresh BUFFER if a window shows it."
  (when (get-buffer-window buffer t)
    (ai-buffer-refresh buffer)))

(defun ai-buffer--title ()
  "Return TITLE of the buffer \"*ai: TITLE*\", for the header line."
  (string-replace "%" "%%" (string-trim (buffer-name) "\\*ai: " "\\*")))

(defun ai-buffer--indicator ()
  "Return the blinking refresh timer indicator for the header line."
  (blinker-string (propertize "●" 'face 'shadow)))

(defun ai-buffer--blink ()
  "Redisplay the indicator in the shown buffers with a refresh timer.
Return non-nil if there is one.  For `blinker-start'."
  (let (found)
    (dolist (window (window-list-1 nil 'nomini t) found)
      (with-current-buffer (window-buffer window)
        (when ai-buffer--timer
          (force-mode-line-update)
          (setq found t))))))

(defun ai-buffer--schedule ()
  "Start the refresh timer if `ai-buffer-refresh-interval' asks for one.
Start blinking its indicator too."
  (when ai-buffer-refresh-interval
    (unless ai-buffer--timer
      (setq ai-buffer--timer
            (run-with-timer ai-buffer-refresh-interval
                            ai-buffer-refresh-interval
                            #'ai-buffer--tick (current-buffer))))
    (blinker-start #'ai-buffer--blink)))

(defun ai-buffer--size-change (window)
  "Re-render WINDOW's buffer in WINDOW.
For the buffer-local `window-size-change-functions'."
  ;; `ai-buffer-refresh' renders in the selected window if it shows the
  ;; buffer.
  (with-selected-window window
    (ai-buffer-refresh)))

(defun ai-buffer--rgb (face &optional attribute)
  "Return the ATTRIBUTE color of FACE as RGB, or nil if unknown.
See `ai-buffer-color'."
  (color-name-to-rgb (face-attribute face (or attribute :foreground)
                                     nil 'default)))

(defun ai-buffer-color (face &optional attribute)
  "Return the ATTRIBUTE color of FACE as \"#rrggbb\", or nil if unknown.
ATTRIBUTE is :foreground by default; unspecified attributes come from
the `default' face."
  (when-let* ((rgb (ai-buffer--rgb face attribute)))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

(defun ai-buffer-shade (percent)
  "Return the background mixed with PERCENT of the foreground.
The value is \"#rrggbb\", or nil if a color is unknown: lighter on a
dark theme and darker on a light one, for cards, borders and tracks."
  (when-let* ((foreground (ai-buffer--rgb 'default))
              (background (ai-buffer--rgb 'default :background)))
    (apply #'color-rgb-to-hex
           (append (color-blend foreground background (/ percent 100.0))
                   '(2)))))

(defun ai-buffer-refresh (&optional buffer)
  "Render BUFFER, the current buffer by default, from its state.
Start the refresh timer if `ai-buffer-refresh-interval' is set."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((windows (mapcar (lambda (window)
                              (list window (window-start window)
                                    (window-point window)))
                            (get-buffer-window-list nil nil t)))
           (point (point))
           (inhibit-read-only t)
           (state-error nil)
           (state (condition-case err
                      (ai-buffer--state)
                    (json-error (setq state-error err)
                                nil))))
      (ai-buffer--flush-images)
      (erase-buffer)
      (condition-case-unless-debug err
          (save-selected-window
            (when-let* ((window (get-buffer-window nil t)))
              (select-window window 'norecord))
            (funcall ai-buffer-render-function state))
        (error
         (erase-buffer)
         (insert (propertize (concat "Render failed: "
                                     (error-message-string err))
                             'face 'error))))
      (setq ai-buffer--status (ai-buffer--format-status state-error))
      (force-mode-line-update)
      (goto-char point)
      ;; `erase-buffer' resets every window's start and point.
      (pcase-dolist (`(,window ,start ,pos) windows)
        (set-window-start window start t)
        (set-window-point window pos))
      (ai-buffer--schedule))))

(defun ai-buffer-revert (&rest _)
  "The `revert-buffer-function' for `ai-buffer-mode'."
  (ai-buffer-refresh))

(define-derived-mode ai-buffer-mode special-mode "AI"
  "Major mode of buffers generated by an AI agent.
`ai-buffer-display' sets the buffer up."
  (setq-local revert-buffer-function #'ai-buffer-revert)
  (setq header-line-format '(" " (:eval (ai-buffer--title)) ai-buffer--status))
  (add-hook 'window-size-change-functions #'ai-buffer--size-change nil t)
  ;; Re-entering a mode kills the local variables, the timer's too.
  (add-hook 'change-major-mode-hook #'ai-buffer--cancel-timer nil t)
  (add-hook 'kill-buffer-hook #'ai-buffer--cancel-timer nil t))

;;;###autoload
(cl-defun ai-buffer-display (title render &key state-file interval)
  "Display the buffer \"*ai: TITLE*\", rendered by RENDER, and return it.
Do not select it.  RENDER, STATE-FILE and INTERVAL set
`ai-buffer-render-function', `ai-buffer-state-file' and
`ai-buffer-refresh-interval'; `default-directory' is that of the
selected window's buffer.  An existing buffer is set up anew, its
timer restarted, and refreshed."
  (let ((directory (buffer-local-value 'default-directory (window-buffer)))
        (buffer (get-buffer-create (format "*ai: %s*" title))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ai-buffer-mode)
        (ai-buffer-mode))
      (ai-buffer--cancel-timer)
      (setq ai-buffer-render-function render
            ai-buffer-state-file state-file
            ai-buffer-refresh-interval interval
            default-directory directory))
    (display-buffer buffer)
    (ai-buffer-refresh buffer)
    buffer))

(provide 'ai-buffer)
;;; ai-buffer.el ends here

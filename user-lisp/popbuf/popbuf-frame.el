;;; popbuf-frame.el --- Toggle any buffer in a centered child frame -*- lexical-binding: t; -*-

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

;; Toggle a centered, focused child frame, built directly on Emacs's core
;; child-frame support (`make-frame' with a `parent-frame' parameter).
;;
;; The child frame is created lazily on the first `popbuf-frame' call from a
;; buffer spec (a buffer, a buffer name, or a function returning one; a
;; function is called inside `save-window-excursion', so providers with
;; window side effects like `ghostel-project' work as-is).  `popbuf-frame' takes
;; the spec as an optional argument, defaulting to `popbuf-frame-default-buffer':
;;
;;   (setq popbuf-frame-default-buffer #'ghostel-project)
;;   (popbuf-frame)             ; toggle the default buffer
;;   (popbuf-frame #'ghostel)   ; toggle some other buffer
;;
;; A single child frame is shared and reused: showing a new spec swaps its
;; buffer into the child frame's window, while the child frame itself
;; persists.  Calling `popbuf-frame' for the buffer already shown hides the
;; child frame.
;;
;; `popbuf-frame' with no argument shows tabs, grouped by
;; `popbuf-key-function'; with a spec, no tabs.  The tab of
;; `popbuf-frame-default-buffer' is static: always present, first, and
;; recreated on demand; killing it closes the child frame.
;; `popbuf-frame-pin' toggles the current buffer as a dynamic tab; killing a
;; shown dynamic tab shows its neighbor.
;;
;; Selecting a window outside the child frame hides it.

;;; Code:

(require 'popbuf)


(defgroup popbuf-frame nil
  "Toggle any buffer in a centered child frame."
  :group 'popbuf)

(defcustom popbuf-frame-default-buffer nil
  "Default buffer spec shown by `popbuf-frame' when called with no argument.
A buffer, a buffer name, or a function returning one.  A function is
called inside `save-window-excursion'."
  :type '(choice (const :tag "None" nil) function string))

(defcustom popbuf-frame-parameters
  '((width . 0.8)
    (height . 0.7)
    (title . "popbuf-frame")
    (min-width . 0)
    (min-height . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (unsplittable . t)
    (no-other-frame . t)
    (no-special-glyphs . t)
    (undecorated . nil)
    (undecorated-round . t)
    (alpha . 95)
    (desktop-dont-save . t))
  "Frame parameters of the child frame, applied on every showing.
Floats in `width' and `height' are fractions of the parent frame.  The
frame is centered.  `parent-frame', `minibuffer', `visibility' and
`auto-hide-function' are reserved."
  :type '(alist :key-type symbol :value-type sexp))

(defcustom popbuf-frame-mode-line-pin "■"
  "Mode line marker of a buffer pinned by `popbuf-frame-pin'.
See `popbuf-frame-mode-line-format'."
  :type 'string)

(defface popbuf-frame-mode-line '((t :inherit shadow))
  "Face of `popbuf-frame-mode-line-pin' on the mode line.")


(defvar popbuf-frame--child-frame nil
  "The child frame created by `popbuf-frame', or nil when none.")

(defvar popbuf-frame--child-frame-created-fullscreen nil
  "The parent frame's `fullscreen' state when the child frame was created.")

(defvar-local popbuf-frame--pin nil
  "Pin key this buffer is pinned to, or nil; see `popbuf--key'.")

(defvar-local popbuf-frame--pinned nil
  "Non-nil in a buffer pinned by `popbuf-frame-pin'.
Not in the buffer pinned from `popbuf-frame-default-buffer'.")

;; A user's decision, not mode state: survive `kill-all-local-variables',
;; run on every mode re-run (e.g. `magit-status' on its open buffer).
(put 'popbuf-frame--pin 'permanent-local t)
(put 'popbuf-frame--pinned 'permanent-local t)

(defun popbuf-frame--pinned-buffers (key)
  "Return the live buffers pinned to KEY, the default one first, then by name.
KEY is a pin key; see `popbuf--key'."
  (sort (seq-filter (lambda (buffer)
                      (equal key (buffer-local-value 'popbuf-frame--pin buffer)))
                    (buffer-list))
        :key (lambda (buffer)
               (list (if (buffer-local-value 'popbuf-frame--pinned buffer) 1 0)
                     (buffer-name buffer)))))

(defun popbuf-frame--shown-buffer ()
  "Return the buffer in the child frame's window, or nil."
  (when-let* ((window (popbuf-frame--window)))
    (window-buffer window)))

(defun popbuf-frame--key-buffer (key)
  "Return the buffer `popbuf-frame' shows for KEY with no spec.
The shown buffer if pinned to KEY, else KEY's first pinned buffer.  KEY's
default buffer is resolved from `popbuf-frame-default-buffer' and pinned
first if missing, e.g. after it was killed."
  (let ((pinned (popbuf-frame--pinned-buffers key))
        (shown (popbuf-frame--shown-buffer)))
    ;; Pin the default unless a visible tab is toggled off or it is pinned.
    (unless (or (and (memq shown pinned) (popbuf-frame--showing-p shown))
                (and pinned
                     (not (buffer-local-value 'popbuf-frame--pinned (car pinned)))))
      (when-let* ((buffer (and popbuf-frame-default-buffer
                               (popbuf--resolve popbuf-frame-default-buffer))))
        (with-current-buffer buffer
          (setq popbuf-frame--pin key))
        (push buffer pinned)))
    (if (memq shown pinned) shown (car pinned))))

(defun popbuf-frame--window ()
  "Return the window of the live popbuf-frame child frame, or nil."
  (when-let* ((child-frame (popbuf-frame--live-child-frame)))
    ;; Main, not root: a side window, e.g. a transient's, splits the root.
    (window-main-window child-frame)))

(defun popbuf-frame--display-tabs ()
  "Show the popbuf-frame's tabs in a tab line if there are two or more."
  (when-let* ((window (popbuf-frame--window)))
    (popbuf--update-tab-line window)))

(defun popbuf-frame--kill-buffer-update ()
  "Unpin the buffer being killed and display the tabs anew.
A shown tab is replaced by its neighbor first, so the child frame is not
deleted with it.  Not the default buffer: killing it closes the child
frame, and `popbuf-frame' shows a new one.  On `kill-buffer-hook'."
  (when popbuf-frame--pin
    (let* ((buffer (current-buffer))
           (tabs (popbuf-frame-tabs))
           (pos (seq-position tabs buffer))
           (rest (remq buffer tabs)))
      (when (and pos rest popbuf-frame--pinned
                 (eq buffer (popbuf-frame--shown-buffer)))
        (popbuf--switch-to-tab (popbuf-frame--window)
                               (nth (min pos (1- (length rest))) rest))))
    (kill-local-variable 'popbuf-frame--pin)
    (popbuf-frame--display-tabs)))

(defun popbuf-frame--live-child-frame ()
  "Return `popbuf-frame--child-frame' if it is live, or nil."
  (and (frame-live-p popbuf-frame--child-frame) popbuf-frame--child-frame))

(defun popbuf-frame--live-parent-frame ()
  "Return the parent frame of `popbuf-frame--child-frame' if it is live, or nil."
  (when-let* ((child-frame (popbuf-frame--live-child-frame)))
    (frame-parent child-frame)))

(defun popbuf-frame--showing-p (buffer)
  "Return non-nil if the child frame is visible and shows BUFFER."
  (and (eq buffer (popbuf-frame--shown-buffer))
       (frame-visible-p popbuf-frame--child-frame)))

(defun popbuf-frame--focus-parent-frame ()
  "Give input focus to the child frame's live parent frame, if any."
  (when-let* ((parent-frame (popbuf-frame--live-parent-frame)))
    (select-frame-set-input-focus parent-frame)))

(defun popbuf-frame--hide (&optional keep-focus)
  "Make the child frame invisible and bury its buffer.
Return focus to its parent frame unless KEEP-FOCUS is non-nil."
  (bury-buffer (popbuf-frame--shown-buffer))
  (make-frame-invisible popbuf-frame--child-frame)
  (unless keep-focus
    (popbuf-frame--focus-parent-frame)))

(defun popbuf-frame--hide-on-leave (_frame)
  "Hide the visible child frame once a window outside it is selected.
On `window-selection-change-functions'.  The minibuffer window, which is
the parent frame's, does not count."
  (when-let* ((child-frame (popbuf-frame--live-child-frame)))
    (unless (or (not (frame-visible-p child-frame))
                (eq (selected-frame) child-frame)
                (window-minibuffer-p))
      ;; The user already moved focus.
      (popbuf-frame--hide t))))

(defun popbuf-frame--restore-parent-focus (deleted-frame)
  "Focus the parent frame when DELETED-FRAME is `popbuf-frame--child-frame'.
On `delete-frame-functions'."
  (when (eq deleted-frame popbuf-frame--child-frame)
    (popbuf-frame--focus-parent-frame)))

(defun popbuf-frame--fit (child-frame parameters)
  "Apply PARAMETERS to CHILD-FRAME, setting only changed ones, and center it.
Runs on every showing: the parent may have been resized meanwhile."
  (modify-frame-parameters
   child-frame
   (seq-remove (lambda (parameter)
                 (or (memq (car parameter)
                           '(parent-frame minibuffer visibility
                             auto-hide-function))
                     (equal (frame-parameter child-frame (car parameter))
                            (cdr parameter))))
               (seq-uniq parameters (lambda (a b) (eq (car a) (car b))))))
  ;; Not `left' and `top' floats: unsupported on text terminals, see
  ;; (info "(elisp) Position Parameters").
  (let ((parent-frame (frame-parent child-frame)))
    (set-frame-position
     child-frame
     (max 0 (/ (- (frame-pixel-width parent-frame)
                  (frame-pixel-width child-frame))
               2))
     (max 0 (/ (- (frame-pixel-height parent-frame)
                  (frame-pixel-height child-frame))
               2)))))

(defun popbuf-frame--refuse-split (&rest _)
  "Refuse to split a child frame's window.
Set as the `split-window' window parameter."
  (user-error "Cannot split a popbuf-frame window"))

(defun popbuf-frame--set-buffer (window buffer)
  "Show BUFFER in the child frame's WINDOW and dedicate WINDOW to it."
  (set-window-dedicated-p window nil)
  (set-window-buffer window buffer)
  (set-window-dedicated-p window t))

(defun popbuf-frame--reveal (child-frame buffer parameters)
  "Show BUFFER in CHILD-FRAME, fit it with PARAMETERS, and focus it."
  (popbuf-frame--set-buffer (window-main-window child-frame) buffer)
  (popbuf-frame--fit child-frame parameters)
  ;; Raising, it also makes CHILD-FRAME visible.
  (select-frame-set-input-focus child-frame))

(defun popbuf-frame--create (buffer parameters)
  "Create a child frame with PARAMETERS, show BUFFER in it and focus it."
  (let* ((parent-frame (selected-frame))
         ;; First match wins: reserved parameters go first.
         (child-frame
          (make-frame
           `((parent-frame . ,parent-frame)
             (minibuffer . ,(minibuffer-window parent-frame))
             (visibility . nil)
             ;; Else `frame-auto-hide-function' (`iconify-frame'), a
             ;; no-op on a child frame.
             (auto-hide-function . popbuf-frame-hide)
             ,@parameters))))
    (setq popbuf-frame--child-frame child-frame
          popbuf-frame--child-frame-created-fullscreen
          (frame-parameter parent-frame 'fullscreen))
    ;; `unsplittable' only stops `display-buffer'; this stops `split-window'.
    (set-window-parameter (frame-root-window child-frame)
                          'split-window #'popbuf-frame--refuse-split)
    ;; `auto-hide-function' is skipped when Emacs deletes the frame
    ;; itself, e.g. as its last buffer is killed.
    (add-hook 'delete-frame-functions #'popbuf-frame--restore-parent-focus)
    (add-hook 'window-selection-change-functions #'popbuf-frame--hide-on-leave)
    (add-hook 'kill-buffer-hook #'popbuf-frame--kill-buffer-update)
    (popbuf--install)
    (popbuf-frame--reveal child-frame buffer parameters)))

(defun popbuf-frame--reusable-child-frame (parent-frame)
  "Return the live child frame, or nil after deleting a stale one.
Stale: PARENT-FRAME's `fullscreen' changed since its creation."
  (when-let* ((child-frame (popbuf-frame--live-child-frame)))
    ;; A child frame is bound to the macOS Space it was created on.
    (if (equal (frame-parameter parent-frame 'fullscreen)
               popbuf-frame--child-frame-created-fullscreen)
        child-frame
      (popbuf-frame-delete)
      nil)))


(defun popbuf-frame-tabs ()
  "Return the popbuf-frame's tabs, the buffers pinned to its pin key, or nil.
The key is set by `popbuf-frame' without a spec."
  (when-let* ((window (popbuf-frame--window))
              (key (window-parameter window 'popbuf-frame-tabs)))
    (popbuf-frame--pinned-buffers key)))

;;;###autoload
(defun popbuf-frame-pin ()
  "Pin the current buffer as a popbuf-frame tab of its pin key.
See `popbuf-key-function'.  Called in a pinned buffer, unpin it,
hiding the popbuf-frame when it is on show.  Pins last for the session and
are dropped with their buffer."
  (interactive)
  ;; Before `popbuf-frame--hide' makes the parent frame's buffer current.
  (let ((name (buffer-name)))
    (if popbuf-frame--pin
        (progn
          (kill-local-variable 'popbuf-frame--pin)
          (kill-local-variable 'popbuf-frame--pinned)
          (popbuf-frame--display-tabs)
          ;; Before the message: it goes to the parent frame's echo area.
          (when (popbuf-frame--showing-p (current-buffer))
            (popbuf-frame--hide))
          (message "popbuf-frame: unpinned buffer `%s'" name))
      (setq popbuf-frame--pin (popbuf--key)
            popbuf-frame--pinned t)
      (popbuf-frame--display-tabs)
      (message "popbuf-frame: pinned buffer `%s'" name))))

;;;###autoload
(defun popbuf-frame-delete ()
  "Delete the popbuf-frame child frame instead of hiding it.
`popbuf-frame' recreates it on its next call."
  (interactive)
  (when-let* ((child-frame (popbuf-frame--live-child-frame)))
    (delete-frame child-frame))
  (setq popbuf-frame--child-frame nil))

;;;###autoload
(defun popbuf-frame (&optional spec parameters)
  "Toggle the popbuf-frame child frame showing SPEC.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'.  Without SPEC, show the last
shown of the buffers pinned under the current key, else the one from
`popbuf-frame-default-buffer'; they are tabs when there are two or more.
Return the buffer if shown, nil if hidden.

PARAMETERS override `popbuf-frame-parameters' for this call."
  (interactive)
  (let* ((key (unless spec (popbuf--key)))
         (parameters (append parameters popbuf-frame-parameters))
         (parent-frame (or (popbuf-frame--live-parent-frame) (selected-frame)))
         ;; Parent selected: a provider's window side effects must land
         ;; there, not on the child frame.
         (buffer (with-selected-frame parent-frame
                   (if spec
                       (popbuf--resolve spec)
                     (popbuf-frame--key-buffer key)))))
    (unless (buffer-live-p buffer)
      (user-error "popbuf-frame: spec yielded no buffer"))
    (cond
     ((popbuf-frame--showing-p buffer)
      (popbuf-frame--hide)
      nil)
     (t
      (if-let* ((child-frame (popbuf-frame--reusable-child-frame parent-frame)))
          (popbuf-frame--reveal child-frame buffer parameters)
        (popbuf-frame--create buffer parameters))
      (let ((window (popbuf-frame--window)))
        (set-window-parameter window 'popbuf-frame-tabs key)
        (set-window-parameter window 'popbuf-tabs #'popbuf-frame-tabs))
      (popbuf-frame--display-tabs)
      buffer))))

;;;###autoload
(defun popbuf-frame-hide ()
  "Hide the popbuf-frame child frame if it is visible."
  (interactive)
  (when-let* ((child-frame (popbuf-frame--live-child-frame))
              ((frame-visible-p child-frame)))
    (popbuf-frame--hide)))

(defvar popbuf-frame-mode-line-format '(:eval (popbuf-frame-mode-line-format))
  "Mode line construct marking a buffer pinned by `popbuf-frame-pin'.
Put it into `mode-line-format'.")
(put 'popbuf-frame-mode-line-format 'risky-local-variable t)

(defun popbuf-frame-mode-line-format ()
  "Return `popbuf-frame-mode-line-pin' in a pinned buffer, else nil."
  (when popbuf-frame--pinned
    (concat " "
            (propertize popbuf-frame-mode-line-pin
                        'face 'popbuf-frame-mode-line
                        'help-echo "popbuf-frame: pinned"))))

(provide 'popbuf-frame)
;;; popbuf-frame.el ends here

;;; popframe.el --- Toggle any buffer in a centered child frame -*- lexical-binding: t; -*-

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
;; The child frame is created lazily on the first `popframe' call from a
;; buffer spec (a buffer, a buffer name, or a function returning one; a
;; function is called inside `save-window-excursion', so providers with
;; window side effects like `ghostel-project' work as-is).  `popframe' takes
;; the spec as an optional argument, defaulting to `popframe-default-buffer':
;;
;;   (setq popframe-default-buffer #'ghostel-project)
;;   (popframe)             ; toggle the default buffer
;;   (popframe #'ghostel)   ; toggle some other buffer
;;
;; A single child frame is shared and reused: showing a new spec swaps its
;; buffer into the child frame's window, while the child frame itself
;; persists.  Calling `popframe' for the buffer already shown hides the
;; child frame.
;;
;; `popframe' with no argument shows tabs, grouped by
;; `popframe-pin-key-function'; with a spec, no tabs.
;; `popframe-tabs-display-function' shows them.  The tab of
;; `popframe-default-buffer' is static: always present, first, and
;; recreated on demand; killing it closes the child frame.
;; `popframe-pin' toggles the current buffer as a dynamic tab; killing a
;; shown dynamic tab shows its neighbor.  In the child frame,
;; `next-buffer' and `previous-buffer' switch tabs.
;;
;; Selecting a window outside the child frame hides it.

;;; Code:


(defgroup popframe nil
  "Toggle any buffer in a centered child frame."
  :group 'convenience)

(defcustom popframe-default-buffer nil
  "Default buffer spec shown by `popframe' when called with no argument.
A buffer, a buffer name, or a function returning one.  A function is
called inside `save-window-excursion'."
  :type '(choice (const :tag "None" nil) function string))

(defcustom popframe-pin-key-function #'ignore
  "Function returning the current buffer's pin key, or nil.
Buffers pinned under one key share tabs; nil stands for a single global
key.  See `popframe-project-pin-key' for per-project tabs."
  :type 'function)

(defcustom popframe-tabs-display-function nil
  "Function displaying the popframe's tabs, or nil.
Called with the popframe window whenever `popframe-tabs' may have
changed: after `popframe' shows the window, and on pinning, unpinning
or killing a pinned buffer.  See `popframe-tab-line-display'."
  :type '(choice (const :tag "None" nil) function))

(defcustom popframe-frame-parameters
  '((width . 0.8)
    (height . 0.7)
    (title . "popframe")
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

(defvar-keymap popframe-tabs-map
  :doc "Keys of the child frame while it shows tabs, as `tab-line-mode''s."
  "<remap> <next-buffer>" '(menu-item "" popframe-switch-to-next-tab
                                      :filter popframe--tab-command)
  "<remap> <previous-buffer>" '(menu-item "" popframe-switch-to-prev-tab
                                          :filter popframe--tab-command)
  ;; Remapped by name only: no dependency on Evil.
  "<remap> <evil-next-buffer>" '(menu-item "" popframe-switch-to-next-tab
                                           :filter popframe--tab-command)
  "<remap> <evil-prev-buffer>" '(menu-item "" popframe-switch-to-prev-tab
                                           :filter popframe--tab-command))


(defvar popframe--child-frame nil
  "The child frame created by `popframe', or nil when none.")

(defvar popframe--child-frame-created-fullscreen nil
  "The parent frame's `fullscreen' state when the child frame was created.")

(defvar-local popframe--pin nil
  "Pin key this buffer is pinned to, or nil; see `popframe--pin-key'.")

(defvar-local popframe--default nil
  "Non-nil in the buffer pinned from `popframe-default-buffer'.")

;; A user's decision, not mode state: survive `kill-all-local-variables',
;; run on every mode re-run (e.g. `magit-status' on its open buffer).
(put 'popframe--pin 'permanent-local t)
(put 'popframe--default 'permanent-local t)

(defun popframe--pin-key ()
  "Return the current buffer's pin key; see `popframe-pin-key-function'."
  (or (funcall popframe-pin-key-function) t))

(defun popframe--pinned-buffers (key)
  "Return the live buffers pinned to KEY, the default one first, then by name.
KEY is a pin key; see `popframe--pin-key'."
  (sort (seq-filter (lambda (buffer)
                      (equal key (buffer-local-value 'popframe--pin buffer)))
                    (buffer-list))
        :key (lambda (buffer)
               (list (if (buffer-local-value 'popframe--default buffer) 0 1)
                     (buffer-name buffer)))))

(defun popframe--shown-buffer ()
  "Return the buffer in the child frame's window, or nil."
  (when-let* ((window (popframe--window)))
    (window-buffer window)))

(defun popframe--key-buffer (key)
  "Return the buffer `popframe' shows for KEY with no spec.
The shown buffer if pinned to KEY, else KEY's first pinned buffer.  KEY's
default buffer is resolved from `popframe-default-buffer' and pinned
first if missing, e.g. after it was killed."
  (let ((pinned (popframe--pinned-buffers key))
        (shown (popframe--shown-buffer)))
    ;; Pin the default unless a visible tab is toggled off or it is pinned.
    (unless (or (and (memq shown pinned) (popframe--showing-p shown))
                (and pinned
                     (buffer-local-value 'popframe--default (car pinned))))
      (when-let* ((buffer (and popframe-default-buffer
                               (popframe--resolve popframe-default-buffer))))
        (with-current-buffer buffer
          (setq popframe--pin key
                popframe--default t))
        (push buffer pinned)))
    (if (memq shown pinned) shown (car pinned))))

(defun popframe--window ()
  "Return the window of the live popframe child frame, or nil."
  (when-let* ((child-frame (popframe--live-child-frame)))
    ;; Main, not root: a side window, e.g. a transient's, splits the root.
    (window-main-window child-frame)))

(defun popframe--display-tabs ()
  "Call `popframe-tabs-display-function' with the popframe window."
  (when-let* ((function popframe-tabs-display-function)
              (window (popframe--window)))
    (funcall function window)))

(defun popframe--kill-buffer-update ()
  "Unpin the buffer being killed and display the tabs anew.
A shown tab is replaced by its neighbor first, so the child frame is not
deleted with it.  Not the default buffer: killing it closes the child
frame, and `popframe' shows a new one.  On `kill-buffer-hook'."
  (when popframe--pin
    (let* ((buffer (current-buffer))
           (tabs (popframe-tabs))
           (pos (seq-position tabs buffer))
           (rest (remq buffer tabs)))
      (when (and pos rest (not popframe--default)
                 (eq buffer (popframe--shown-buffer)))
        (popframe-switch-to-tab (nth (min pos (1- (length rest))) rest))))
    (kill-local-variable 'popframe--pin)
    (popframe--display-tabs)))

(defun popframe--live-child-frame ()
  "Return `popframe--child-frame' if it is live, or nil."
  (and (frame-live-p popframe--child-frame) popframe--child-frame))

(defun popframe--live-parent-frame ()
  "Return the parent frame of `popframe--child-frame' if it is live, or nil."
  (when-let* ((child-frame (popframe--live-child-frame)))
    (frame-parent child-frame)))

(defun popframe--showing-p (buffer)
  "Return non-nil if the child frame is visible and shows BUFFER."
  (and (eq buffer (popframe--shown-buffer))
       (frame-visible-p popframe--child-frame)))

(defun popframe--resolve (spec)
  "Resolve SPEC to a live buffer, or nil.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'."
  (when-let* ((buffer (if (functionp spec)
                          (save-window-excursion (funcall spec))
                        spec)))
    (get-buffer buffer)))

(defun popframe--focus-parent-frame ()
  "Give input focus to the child frame's live parent frame, if any."
  (when-let* ((parent-frame (popframe--live-parent-frame)))
    (select-frame-set-input-focus parent-frame)))

(defun popframe--hide (&optional keep-focus)
  "Make the child frame invisible and bury its buffer.
Return focus to its parent frame unless KEEP-FOCUS is non-nil."
  (bury-buffer (popframe--shown-buffer))
  (make-frame-invisible popframe--child-frame)
  (unless keep-focus
    (popframe--focus-parent-frame)))

(defun popframe--hide-on-leave (_frame)
  "Hide the visible child frame once a window outside it is selected.
On `window-selection-change-functions'.  The minibuffer window, which is
the parent frame's, does not count."
  (when-let* ((child-frame (popframe--live-child-frame)))
    (unless (or (not (frame-visible-p child-frame))
                (eq (selected-frame) child-frame)
                (window-minibuffer-p))
      ;; The user already moved focus.
      (popframe--hide t))))

(defun popframe--restore-parent-focus (deleted-frame)
  "Focus the parent frame when DELETED-FRAME is `popframe--child-frame'.
On `delete-frame-functions'."
  (when (eq deleted-frame popframe--child-frame)
    (popframe--focus-parent-frame)))

(defun popframe--fit (child-frame parameters)
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

(defun popframe--refuse-split (&rest _)
  "Refuse to split a child frame's window.
Set as the `split-window' window parameter."
  (user-error "Cannot split a popframe window"))

(defun popframe--set-buffer (window buffer)
  "Show BUFFER in the child frame's WINDOW and dedicate WINDOW to it."
  (set-window-dedicated-p window nil)
  (set-window-buffer window buffer)
  (set-window-dedicated-p window t))

(defun popframe--reveal (child-frame buffer parameters)
  "Show BUFFER in CHILD-FRAME, fit it with PARAMETERS, and focus it."
  (popframe--set-buffer (window-main-window child-frame) buffer)
  (popframe--fit child-frame parameters)
  ;; Raising, it also makes CHILD-FRAME visible.
  (select-frame-set-input-focus child-frame))

(defun popframe--selected-tabs ()
  "Return the tabs of the child frame if it is selected, else nil."
  (and (eq (selected-frame) popframe--child-frame)
       (popframe-tabs)))

(defun popframe--tab-command (command)
  "Return COMMAND in the selected child frame with tabs, else nil.
A `menu-item' filter: elsewhere the key keeps its usual binding."
  (and (popframe--selected-tabs) command))

(defun popframe--create (buffer parameters)
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
             (auto-hide-function . popframe-hide)
             ,@parameters))))
    (setq popframe--child-frame child-frame
          popframe--child-frame-created-fullscreen
          (frame-parameter parent-frame 'fullscreen))
    ;; `unsplittable' only stops `display-buffer'; this stops `split-window'.
    (set-window-parameter (frame-root-window child-frame)
                          'split-window #'popframe--refuse-split)
    ;; `auto-hide-function' is skipped when Emacs deletes the frame
    ;; itself, e.g. as its last buffer is killed.
    (add-hook 'delete-frame-functions #'popframe--restore-parent-focus)
    (add-hook 'window-selection-change-functions #'popframe--hide-on-leave)
    (add-hook 'kill-buffer-hook #'popframe--kill-buffer-update)
    ;; Before minor mode maps, which may bind the same keys.
    (add-to-list 'emulation-mode-map-alists
                 `((popframe--child-frame . ,popframe-tabs-map)))
    (popframe--reveal child-frame buffer parameters)))

(defun popframe--reusable-child-frame (parent-frame)
  "Return the live child frame, or nil after deleting a stale one.
Stale: PARENT-FRAME's `fullscreen' changed since its creation."
  (when-let* ((child-frame (popframe--live-child-frame)))
    ;; A child frame is bound to the macOS Space it was created on.
    (if (equal (frame-parameter parent-frame 'fullscreen)
               popframe--child-frame-created-fullscreen)
        child-frame
      (popframe-delete)
      nil)))


(defun popframe-tabs ()
  "Return the popframe's tabs, the buffers pinned to its pin key, or nil.
The key is set by `popframe' without a spec."
  (when-let* ((window (popframe--window))
              (key (window-parameter window 'popframe-tabs)))
    (popframe--pinned-buffers key)))

(defun popframe-switch-to-tab (buffer)
  "Show BUFFER, a popframe tab, in the popframe window."
  (popframe--set-buffer (popframe--window) buffer))

;;;###autoload
(defun popframe-pinned-p ()
  "Return non-nil if the current buffer is pinned by `popframe-pin'.
The automatically pinned default buffer does not count."
  (and popframe--pin (not popframe--default)))

;;;###autoload
(defun popframe-pin ()
  "Pin the current buffer as a popframe tab of its pin key.
See `popframe-pin-key-function'.  Called in a pinned buffer, unpin it,
hiding the popframe when it is on show.  Pins last for the session and
are dropped with their buffer."
  (interactive)
  ;; Before `popframe--hide' makes the parent frame's buffer current.
  (let ((name (buffer-name)))
    (if popframe--pin
        (progn
          (kill-local-variable 'popframe--pin)
          (kill-local-variable 'popframe--default)
          (popframe--display-tabs)
          ;; Before the message: it goes to the parent frame's echo area.
          (when (popframe--showing-p (current-buffer))
            (popframe--hide))
          (message "popframe: unpinned buffer `%s'" name))
      (setq popframe--pin (popframe--pin-key))
      (popframe--display-tabs)
      (message "popframe: pinned buffer `%s'" name))))

;;;###autoload
(defun popframe-delete ()
  "Delete the popframe child frame instead of hiding it.
`popframe' recreates it on its next call."
  (interactive)
  (when-let* ((child-frame (popframe--live-child-frame)))
    (delete-frame child-frame))
  (setq popframe--child-frame nil))

;;;###autoload
(defun popframe (&optional spec parameters)
  "Toggle the popframe child frame showing SPEC.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'.  Without SPEC, show the last
shown of the buffers pinned under the current key, else the one from
`popframe-default-buffer'; they are tabs when there are two or more.
Return the buffer if shown, nil if hidden.

PARAMETERS override `popframe-frame-parameters' for this call."
  (interactive)
  (let* ((key (unless spec (popframe--pin-key)))
         (parameters (append parameters popframe-frame-parameters))
         (parent-frame (or (popframe--live-parent-frame) (selected-frame)))
         ;; Parent selected: a provider's window side effects must land
         ;; there, not on the child frame.
         (buffer (with-selected-frame parent-frame
                   (if spec
                       (popframe--resolve spec)
                     (popframe--key-buffer key)))))
    (unless (buffer-live-p buffer)
      (user-error "popframe: spec yielded no buffer"))
    (cond
     ((popframe--showing-p buffer)
      (popframe--hide)
      nil)
     (t
      (if-let* ((child-frame (popframe--reusable-child-frame parent-frame)))
          (popframe--reveal child-frame buffer parameters)
        (popframe--create buffer parameters))
      (set-window-parameter (popframe--window) 'popframe-tabs key)
      (popframe--display-tabs)
      buffer))))

;;;###autoload
(defun popframe-switch-to-next-tab (&optional arg)
  "Switch to the ARGth next popframe tab, cycling."
  (interactive "p")
  (let* ((tabs (or (popframe--selected-tabs)
                   (user-error "No popframe tabs here")))
         (pos (or (seq-position tabs (popframe--shown-buffer)) -1)))
    (popframe-switch-to-tab (nth (mod (+ pos (or arg 1)) (length tabs)) tabs))))

;;;###autoload
(defun popframe-switch-to-prev-tab (&optional arg)
  "Switch to the ARGth previous popframe tab, cycling."
  (interactive "p")
  (popframe-switch-to-next-tab (- (or arg 1))))

;;;###autoload
(defun popframe-hide ()
  "Hide the popframe child frame if it is visible."
  (interactive)
  (when-let* ((child-frame (popframe--live-child-frame))
              ((frame-visible-p child-frame)))
    (popframe--hide)))

(provide 'popframe)
;;; popframe.el ends here

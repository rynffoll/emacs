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
;; `popframe-pin' pins the current buffer to the current project, so that
;; `popframe' with no argument shows it instead of `popframe-default-buffer'
;; while visiting that project.  One pin per project; the command toggles,
;; so calling it in a pinned buffer unpins it.

;;; Code:

;; project.el is built in and `project-current' is autoloaded; a non-nil
;; return means project.el is loaded, so `project-root' is available too.
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))


(defgroup popframe nil
  "Toggle any buffer in a centered child frame."
  :group 'convenience)

(defcustom popframe-default-buffer nil
  "Default buffer spec shown by `popframe' when called with no argument.
A buffer, a buffer name, or a function returning one.  A function is
called inside `save-window-excursion'."
  :type '(choice (const :tag "None" nil) function string))

(defcustom popframe-width-ratio 0.8
  "Child frame width as a fraction of the parent frame width."
  :type 'number)

(defcustom popframe-height-ratio 0.7
  "Child frame height as a fraction of the parent frame height."
  :type 'number)

(defcustom popframe-override-parameters '((alpha . 95)
                                          (undecorated . nil)
                                          (undecorated-round . t))
  "Extra frame parameters for the child frame, as an alist.
Passed verbatim to `make-frame', taking precedence over popframe's own
parameters."
  :type '(alist :key-type symbol :value-type sexp))


(defvar popframe--child-frame nil
  "The child frame created by `popframe', or nil when none.")

(defvar popframe--child-frame-created-fullscreen nil
  "The parent frame's `fullscreen' state when the child frame was created.
A child frame is bound to the macOS Space it was created on; if the
parent frame later enters or leaves native fullscreen (a different
Space), reusing `popframe--child-frame' would move focus back to the old
Space.  `popframe' compares this against the parent frame's current state
to detect that and recreate the child frame instead.")

(defvar-local popframe--pin nil
  "Pin key this buffer is pinned to, or nil when it is not pinned.
See `popframe--pin-key'.  Pins deliberately live on the buffers
themselves rather than in a central table: there is no global state to
prune, and a pin dies with its buffer.")

;; A pin is a user's decision about a buffer, not state derived from its major
;; mode, so it must survive `kill-all-local-variables' — which every mode
;; re-run calls, be it `normal-mode', `revert-buffer', a mode changed by hand,
;; or a package that re-runs its own mode on each setup (invoking
;; `magit-status' on an already open status buffer does).
(put 'popframe--pin 'permanent-local t)

(defun popframe--pin-key ()
  "Return the pin key for the current buffer's context.
The current project's root, or t when outside a project.  Never nil, so
it cannot match the nil default of `popframe--pin'."
  (if-let* ((project (project-current)))
      (project-root project)
    t))

(defun popframe--pinned-buffer (key)
  "Return the buffer pinned to KEY, or nil.
KEY is a pin key; see `popframe--pin-key'.  Scans `buffer-list', which
only holds live buffers, so a killed pinned buffer simply stops being
found."
  (seq-find (lambda (buffer)
              (equal key (buffer-local-value 'popframe--pin buffer)))
            (buffer-list)))

(defun popframe--live-child-frame ()
  "Return `popframe--child-frame' if it is live, or nil."
  (and (frame-live-p popframe--child-frame) popframe--child-frame))

(defun popframe--delete-child-frame ()
  "Delete `popframe--child-frame' if it is live, and clear it.
Focus is restored via `popframe--restore-parent-focus' on
`delete-frame-functions', left unsuppressed here: this frame is created
normally, so other packages' frame bookkeeping expects normal cleanup too."
  (when (frame-live-p popframe--child-frame)
    (delete-frame popframe--child-frame))
  (setq popframe--child-frame nil))

(defun popframe--live-parent-frame ()
  "Return `popframe--child-frame's parent frame if both are live, or nil."
  (when-let* ((child-frame (popframe--live-child-frame))
              (parent-frame (frame-parent child-frame)))
    (and (frame-live-p parent-frame) parent-frame)))

(defun popframe--effective-parent-frame ()
  "Return `popframe--child-frame's live parent frame, or the selected frame."
  (or (popframe--live-parent-frame) (selected-frame)))

(defun popframe--showing-p (buffer)
  "Return non-nil if the child frame is live, visible, and shows BUFFER."
  (when-let* ((child-frame (popframe--live-child-frame)))
    (and (frame-visible-p child-frame)
         (get-buffer-window buffer child-frame))))

(defun popframe--resolve (spec)
  "Resolve SPEC to a live buffer, or nil.
SPEC is a buffer, a buffer name, or a function returning one; a function
is called inside `save-window-excursion'."
  (get-buffer
   (if (functionp spec)
       (save-window-excursion (funcall spec))
     spec)))

(defun popframe--focus-parent-frame ()
  "Give input focus to the child frame's live parent frame, if any."
  (when-let* ((parent-frame (popframe--live-parent-frame)))
    (select-frame-set-input-focus parent-frame)))

(defun popframe--hide ()
  "Make the child frame invisible and return focus to its parent frame."
  (make-frame-invisible popframe--child-frame)
  (popframe--focus-parent-frame))

(defun popframe--dismiss (buffer)
  "Hide the child frame if it is currently showing BUFFER."
  (when (popframe--showing-p buffer)
    (popframe--hide)))

(defun popframe--auto-hide ()
  "Hide the child frame, keeping it for reuse.
Installed as the child frame's `auto-hide-function' parameter so that
`quit-window', `bury-buffer' and similar commands dismiss the popframe
instead of falling back to `frame-auto-hide-function' (whose default
`iconify-frame' is a no-op on a child frame)."
  (when (popframe--live-child-frame)
    (popframe--hide)))

(defun popframe--restore-parent-focus (deleted-frame)
  "Focus the popframe's parent frame when DELETED-FRAME is the child frame.
Added to `delete-frame-functions' by `popframe--create'; removes itself
once it fires.  Covers what `auto-hide-function' (`popframe--auto-hide')
cannot: `window--delete' skips it when Emacs auto-deletes the frame
because its last buffer was killed (e.g. a shell exiting on its own)."
  (when (eq deleted-frame popframe--child-frame)
    (remove-hook 'delete-frame-functions #'popframe--restore-parent-focus)
    (popframe--focus-parent-frame)))

(defun popframe--target-size (parent-frame)
  "Return the child frame's target size as (WIDTH . HEIGHT).
Sized as the configured ratios of PARENT-FRAME's dimensions."
  (cons (round (* (frame-width  parent-frame) popframe-width-ratio))
        (round (* (frame-height parent-frame) popframe-height-ratio))))

(defun popframe--fit (child-frame)
  "Resize CHILD-FRAME to the configured ratios of its parent frame and
re-center it.  Resizes only when the target character size differs from
the current one, then always re-centers, so a parent frame resized while
CHILD-FRAME was hidden (for example toggling fullscreen) does not leave
CHILD-FRAME oversized or off-center."
  (pcase-let* ((parent-frame (popframe--effective-parent-frame))
               (`(,width . ,height) (popframe--target-size parent-frame)))
    (unless (and (= width  (frame-width  child-frame))
                 (= height (frame-height child-frame)))
      (set-frame-size child-frame width height))
    (set-frame-position
     child-frame
     (max 0 (/ (- (frame-pixel-width  parent-frame)
                  (frame-pixel-width  child-frame))
               2))
     (max 0 (/ (- (frame-pixel-height parent-frame)
                  (frame-pixel-height child-frame))
               2)))))

(defun popframe--reveal (child-frame buffer)
  "Show BUFFER in CHILD-FRAME, make it visible, and give it input focus.
Reuses CHILD-FRAME's window (only its buffer changes).  The window is
dedicated (so killing its buffer deletes the child frame), so dedication is
lifted around the buffer swap and restored afterwards.  CHILD-FRAME is
refit to its parent frame first, in case the parent frame was resized
while CHILD-FRAME was hidden."
  (let ((window (frame-root-window child-frame)))
    (set-window-dedicated-p window nil)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (popframe--fit child-frame)
    (make-frame-visible child-frame)
    (select-frame-set-input-focus child-frame)))

(defun popframe--create (buffer)
  "Create and show a centered child frame displaying BUFFER.
Store it in `popframe--child-frame' and focus it."
  (pcase-let* ((parent-frame (selected-frame))
               (`(,width . ,height) (popframe--target-size parent-frame))
               (minibuffer (minibuffer-window parent-frame))
               (defaults `((parent-frame . ,parent-frame)
                           (minibuffer . ,minibuffer)
                           (title . "popframe")
                           (width . ,width)
                           (height . ,height)
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
                           (undecorated . t)
                           (visibility . nil)
                           (desktop-dont-save . t)
                           (auto-hide-function . popframe--auto-hide)))
               ;; `make-frame' resolves duplicate parameters the same way
               ;; `default-frame-alist' does: first match wins, so prepending lets
               ;; POPFRAME-OVERRIDE-PARAMETERS take precedence over our defaults.
               (params (append popframe-override-parameters defaults))
               (child-frame (make-frame params)))
    (setq popframe--child-frame child-frame
          popframe--child-frame-created-fullscreen
          (frame-parameter parent-frame 'fullscreen))
    (add-hook 'delete-frame-functions #'popframe--restore-parent-focus)
    (popframe--reveal child-frame buffer)))

(defun popframe--delete-stale-child-frame (child-frame parent-frame)
  "Delete CHILD-FRAME unless it is live and fresh relative to PARENT-FRAME.
Return CHILD-FRAME unchanged if so, else nil.
See `popframe--child-frame-created-fullscreen' for what makes it stale."
  (if (and child-frame
           (frame-live-p child-frame)
           (equal (frame-parameter parent-frame 'fullscreen)
                  popframe--child-frame-created-fullscreen))
      child-frame
    (when child-frame
      (popframe--delete-child-frame))
    nil))


;;;###autoload
(defun popframe-pinned-p ()
  "Return non-nil if the current buffer is pinned by `popframe-pin'.
Meant for mode-line constructs and the like, so that callers need not
reach into popframe's internals."
  (and popframe--pin t))

;;;###autoload
(defun popframe-pin ()
  "Pin the current buffer as the popframe buffer for the current project.
`popframe' called with no argument then shows this buffer instead of
`popframe-default-buffer' while visiting that project.

There is one pin per project: pinning replaces the project's previous
pin.  Called in an already pinned buffer, this unpins it, dismissing the
popframe when it is the buffer on show.  Pins last for the session only
and are dropped when the buffer is killed."
  (interactive)
  ;; Captured before dismissing: dismissing reselects the parent frame's
  ;; window, which makes that window's buffer current, so `buffer-name'
  ;; would no longer name the buffer being unpinned by the time we report it.
  (let ((name (buffer-name)))
    (if popframe--pin
        (progn
          (kill-local-variable 'popframe--pin)
          ;; Dismiss before messaging, so the message appears in the echo
          ;; area of the parent frame that dismissing focuses.
          (popframe--dismiss (current-buffer))
          (message "popframe: unpinned buffer `%s'" name))
      (let* ((key (popframe--pin-key))
             (previous (popframe--pinned-buffer key)))
        (when previous
          (with-current-buffer previous (kill-local-variable 'popframe--pin)))
        (setq popframe--pin key)
        (if previous
            (message "popframe: pinned buffer `%s' → `%s'"
                     (buffer-name previous) name)
          (message "popframe: pinned buffer `%s'" name))))))

;;;###autoload
(defun popframe-delete ()
  "Delete the popframe child frame instead of merely hiding it.
Unlike `popframe', which toggles visibility and reuses the child frame,
this deletes it immediately — useful if the child frame stops responding
or looks broken.  `popframe' recreates it lazily on its next call."
  (interactive)
  (popframe--delete-child-frame))

;;;###autoload
(defun popframe (&optional spec)
  "Toggle the popframe child frame showing SPEC.
SPEC is a buffer, a buffer name, or a function returning one.  It
defaults to the buffer pinned to the current project by `popframe-pin',
falling back to `popframe-default-buffer'.  A function is called inside
`save-window-excursion'.

A single child frame is shared and reused: the child frame is created
lazily on first use, and showing a different SPEC swaps its buffer into
the child frame's window while the child frame itself persists.  Calling
this command for the buffer already shown hides the child frame, so each
spec toggles independently."
  (interactive)
  (let* ((spec (or spec
                   (popframe--pinned-buffer (popframe--pin-key))
                   popframe-default-buffer))
         (child-frame (popframe--live-child-frame))
         (parent-frame (popframe--effective-parent-frame))
         ;; Resolve with the parent frame selected so a provider's window side
         ;; effects (buffer display, tab-bar) land on the real parent frame,
         ;; not on the floating child frame — whose frame parameters
         ;; `save-window-excursion' inside `popframe--resolve' would not
         ;; restore.
         (buffer (with-selected-frame parent-frame
                   (popframe--resolve spec))))
    (unless (buffer-live-p buffer)
      (user-error "popframe: spec yielded no buffer"))
    (if (popframe--showing-p buffer)
        (popframe--hide)
      (setq child-frame
            (popframe--delete-stale-child-frame child-frame parent-frame))
      (if child-frame
          (popframe--reveal child-frame buffer)
        (popframe--create buffer)))))

(provide 'popframe)
;;; popframe.el ends here

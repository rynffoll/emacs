;;; review.el --- Annotate code into review.org and feed it to an agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, convenience
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

;; Lightweight, agent-oriented code review.
;;
;; Annotate code with review notes, see them inline as you work, and hand
;; them to an AI agent that applies the fixes.

;;; Code:

(require 'project)
(require 'subr-x)
(require 'seq)
(require 'rx)

(declare-function org-capture "org-capture" (&optional goto keys))
(declare-function org-capture-get "org-capture" (prop &optional local))
(declare-function org-capture-target-buffer "org-capture" (file))
(declare-function org-set-regexps-and-options "org" (&optional tags-only))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-narrow-to-subtree "org" (&optional element))
(declare-function org-fold-show-subtree "org-fold" ())
(declare-function org-element-parse-buffer "org-element" (&optional granularity visible-only))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element-property "org-element" (property node))
(declare-function flymake-make-diagnostic "flymake" (locus beg end type text &optional data overlay-properties))
(defvar org-capture-initial)

(defgroup review nil
  "Annotate code into review.org and feed it to an agent."
  :group 'tools)

(defconst review--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where review.el and its bundled template.org live.")

(defcustom review-send-message "@review.org\n"
  "Message `review-send' hands to `review-send-function'.
The default is a bare `@'-reference to the project's review.org (Claude
Code opens the file, whose `* COMMENT' heading from
`review-file-template' holds the instructions).  Adjust to suit your
agent.

The trailing newline submits the prompt.  It must travel with the text
as one string: a Return sent separately (after the text) is swallowed by
Claude Code's `@'-mention file picker, leaving the prompt typed but not
submitted."
  :type 'string)

(defcustom review-send-function nil
  "Function that hands the review to an agent.
Called by `review-send' with one argument, `review-send-message',
after review.org is saved.  Set it to integrate with your agent — e.g. a
wrapper around claude-code-ide, aider, or gptel."
  :type '(choice (const :tag "None" nil) function))

(defcustom review-capture-template
  '("r" "Review note" entry
    (function review--goto-review-file)
    "* TODO %(review--link)\n%(review--src-block)\n%?"
    :empty-lines 1
    :after-finalize review--after-finalize)
  "`org-capture' template entry for leaving a review note."
  :type 'sexp)

(defcustom review-note-prefix "review: "
  "String prepended to each review note in its Flymake diagnostic message.
Marks notes from this backend apart from other `:note' diagnostics (eglot,
etc.) in the echo area, sideline, and `flymake-show-buffer-diagnostics'.
Set to \"rev: \" for a shorter tag, or \"\" to disable."
  :type 'string)


;;; Location helpers

(defconst review-file-name "review.org"
  "Base name of the per-project review file.")

(defun review--root ()
  "Project root (or `default-directory' outside a project) — review.org's dir."
  (or (ignore-errors (project-root (project-current))) default-directory))

(defun review-file ()
  "Path to the current project's review.org."
  (expand-file-name review-file-name (review--root)))

(defun review--review-file-p (path)
  "Non-nil when PATH is a review file, judged by its base name."
  (and path (equal (file-name-nondirectory path) review-file-name)))


;;; org-capture template helpers

(defun review--lang ()
  "Src-block language from the buffer `org-capture' was invoked in."
  (let* ((buf  (and (fboundp 'org-capture-get) (org-capture-get :original-buffer)))
         (mode (if (buffer-live-p buf) (buffer-local-value 'major-mode buf) major-mode)))
    ;; python-ts-mode -> python, go-mode -> go, emacs-lisp-mode -> emacs-lisp
    (replace-regexp-in-string (rx (? "-ts") "-mode" eos) "" (symbol-name mode))))

(defun review--src-block ()
  "Return a `#+begin_src' block for the captured text, or \"\" when empty.
The captured text is the region `org-capture' stashed as :initial."
  (let ((code (string-trim-right (or (org-capture-get :initial) ""))))
    (if (string-empty-p code)
        ""
      (format "#+begin_src %s\n%s\n#+end_src\n" (review--lang) code))))

(defvar review--here nil
  "Plist (:file :line :base) of the annotated spot.
Dynamically bound by `review-note' around the capture; :base is
review.org's directory, so the link is relative to where it is inserted.")

(defun review--make-link (file line base)
  "Org backlink to FILE at LINE, with FILE made relative to BASE."
  (format "[[file:%s::%d]]" (file-relative-name file base) line))

(defun review--relativize-link (link)
  "Rewrite the file path in LINK (\"[[file:PATH::…]]\") relative to review.org's dir."
  (let ((base (file-name-directory (review-file))))
    (if (string-match (rx "[[file:" (group (+? nonl)) (group (or "::" "]"))) link)
        ;; capture match positions BEFORE expand/relative clobber the match data
        (let ((start (match-beginning 1))
              (raw   (match-string 1 link))
              (tail  (substring link (match-beginning 2))))
          (concat (substring link 0 start)
                  (file-relative-name (expand-file-name raw) base)
                  tail))
      link)))

(defun review--link ()
  "Single-line backlink: path relative to review.org's dir + line number.
Outside `review-note' (e.g. the global `org-capture' menu)
`review--here' is unset; fall back to the capture annotation (%a),
also rewritten relative to review.org's directory."
  (let ((file (plist-get review--here :file))
        (line (plist-get review--here :line))
        (base (plist-get review--here :base)))
    (if (and file line base)
        (review--make-link file line base)
      (review--relativize-link
       (or (and (fboundp 'org-capture-get) (org-capture-get :annotation)) "")))))

(defcustom review-file-template
  (with-temp-buffer
    (insert-file-contents (expand-file-name "template.org" review--dir))
    (buffer-string))
  "Content inserted into a freshly created review.org.
Defaults to the bundled template.org, read when the package loads.  It
carries the `-*-' cookie (buffer-local save/revert hooks that re-push
notes to code buffers; whitelist them in `safe-local-variable-values'),
the `#+TODO:' status keywords, and a `* COMMENT Agent instructions'
heading — a real heading so it folds away, and \"COMMENT\" keeps
`review--parse' (which only matches headlines with a file: link) and
Org export from touching it.  Edit template.org before creating new
review.org files, not after."
  :type 'string)

(defun review--ensure-template ()
  "Set up review.org from the template when the current buffer is empty.
Insert `review-file-template', activate its `#+TODO:' keywords, and apply
the `-*-' hook cookie, so a freshly created (or empty) review.org wires up."
  (when (= (point-min) (point-max))
    (insert review-file-template)
    (org-set-regexps-and-options)        ; activate #+TODO
    (hack-local-variables)))             ; apply the -*- save/revert hooks

(defun review--goto-review-file ()
  "`org-capture' target function: visit the project's review.org at its end."
  (set-buffer (org-capture-target-buffer (review-file)))
  (review--ensure-template)
  (goto-char (point-max)))


;;; Commands

(defun review--bounds ()
  "Char bounds (BEG . END) of the active region, else of the current line."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (line-beginning-position) (line-end-position))))

;;;###autoload
(defun review-note ()
  "Leave a review note on the region, or the current line if no region.
The selection (or line) is captured into the entry's `#+begin_src' block."
  (interactive)
  (pcase-let ((`(,beg . ,end) (review--bounds)))
    (let ((org-capture-initial (buffer-substring-no-properties beg end))
          (review--here
           (list :file buffer-file-name
                 :line (line-number-at-pos beg)
                 :base (review--root))))
      (org-capture nil "r"))))

;;;###autoload
(defun review-open ()
  "Open the current project's review.org (creating it from the template if new)."
  (interactive)
  (find-file (review-file))
  (review--ensure-template))

;;;###autoload
(defun review-send ()
  "Save review.org and hand it to the agent via `review-send-function'."
  (interactive)
  (let ((file (review-file)))
    (unless (file-exists-p file)
      (user-error "No review.org yet — annotate first (`review-note')"))
    (unless (functionp review-send-function)
      (user-error "Set `review-send-function' to send the review"))
    (when-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf (when (buffer-modified-p) (save-buffer))))
    (funcall review-send-function review-send-message)))


;;; Flymake backend — open (TODO) notes as :note diagnostics (eglot-style)

(defvar review--db (make-hash-table :test 'equal)
  "Cache: review.org path -> (SIGNAL . TABLE).
TABLE maps a target file's truename to a list of (LINE STATUS NOTE).")

(defun review--signal (review)
  "Change signal for REVIEW: buffer modification tick when visited, else mtime."
  (if-let* ((buf (find-buffer-visiting review)))
      (buffer-chars-modified-tick buf)
    (file-attribute-modification-time (file-attributes review))))

(defun review--parse (review)
  "Parse REVIEW into a hash: target truename -> list of (LINE STATUS NOTE)."
  (let ((base (file-name-directory review))
        (table (make-hash-table :test 'equal)))
    (with-current-buffer (find-file-noselect review)
      (save-restriction
        (widen)
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hl)
            (let* ((link   (org-element-map (org-element-property :title hl)
                               'link #'identity nil t))
                   (search (and link (org-element-property :search-option link))))
              (when (and link
                         (equal (org-element-property :type link) "file")
                         search (string-match-p (rx bos (+ (any "0-9")) eos) search))
                (let ((key  (file-truename
                             (expand-file-name (org-element-property :path link) base)))
                      (body (org-element-map hl 'paragraph
                              (lambda (p)
                                (buffer-substring-no-properties
                                 (org-element-property :contents-begin p)
                                 (org-element-property :contents-end p)))
                              nil t)))
                  (push (list (string-to-number search)
                              (org-element-property :todo-keyword hl)
                              (string-trim (or body "")))
                        (gethash key table)))))))))
    table))

(defun review-annotations (file)
  "Cached list of (LINE STATUS NOTE) for FILE from the project's review.org.
review.org is parsed once per change (keyed on `review--signal')."
  (let ((review (review-file)))
    (when (file-exists-p review)
      (let ((sig   (review--signal review))
            (entry (gethash review review--db)))
        (unless (and entry (equal (car entry) sig))
          (setq entry (cons sig (review--parse review)))
          (puthash review entry review--db))
        (gethash (file-truename file) (cdr entry))))))

(defvar-local review--report-fn nil
  "Stashed Flymake `report-fn' for this buffer (eglot-style).
Re-invoked by `review--report' when review.org changes, without
re-running `flymake-start'.")

(defun review--report (buffer)
  "Push current review notes to BUFFER via its stashed `review--report-fn'.
Replaces this backend's diagnostics (the :region covers the whole buffer)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when review--report-fn
        (let ((file buffer-file-name) diags)
          (when (and file (not (review--review-file-p file)))
            (save-excursion
              (dolist (a (review-annotations file))
                (unless (equal (nth 1 a) "DONE")          ; ignore DONE
                  (let ((note (nth 2 a)))
                    (goto-char (point-min))
                    (forward-line (1- (max 1 (nth 0 a))))  ; clamp line >= 1
                    (push (flymake-make-diagnostic
                           buffer (pos-bol) (pos-eol)
                           :note (concat review-note-prefix
                                         (if (string-empty-p note) "(no note)" note)))
                          diags))))))
          (funcall review--report-fn diags
                   :region (cons (point-min) (point-max))))))))

(defun review-flymake (report-fn &rest _)
  "Flymake backend: stash REPORT-FN and report current review notes.
Add to `flymake-diagnostic-functions'.  Notes come from review.org, not the
buffer text, so updates are pushed via `review--report' when review.org
changes (`review--after-finalize', `review--after-review-change'),
not on buffer edits."
  (setq review--report-fn report-fn)
  (review--report (current-buffer)))

(defun review--propagate (root)
  "Re-push review notes to every Flymake buffer under ROOT."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'review--report-fn buf)
      (let ((f (buffer-local-value 'buffer-file-name buf)))
        (when (and f (file-in-directory-p f root))
          (review--report buf))))))

(defun review--after-finalize ()
  "Push the freshly captured note to the source buffer (template :after-finalize)."
  (when-let* ((buf (org-capture-get :original-buffer)))
    (review--report buf)))

(defun review--after-review-change ()
  "On save/revert of a review.org, push notes to that project's buffers.
For `after-save-hook' and `after-revert-hook'."
  (when (review--review-file-p buffer-file-name)
    (review--propagate (file-name-directory buffer-file-name))))

;;;###autoload
(defun review-edit ()
  "Open the review.org note covering the current line/region, to edit it.
Matches an entry whose recorded line falls within the selection (or the
current line).  Errors when there is no note there."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (review--bounds))
               (file buffer-file-name)
               (lo (line-number-at-pos beg))
               (hi (line-number-at-pos end))
               (match (seq-find (lambda (a) (<= lo (nth 0 a) hi))
                                (and file (review-annotations file)))))
    (unless match (user-error "No review note on these lines"))
    (let* ((review (review-file))
           (link (review--make-link file (nth 0 match)
                                    (file-name-directory review))))
      (find-file review)
      (goto-char (point-min))
      (unless (search-forward link nil t)
        (user-error "Entry not found in review.org"))
      (org-back-to-heading t)
      (when (fboundp 'org-fold-show-subtree) (org-fold-show-subtree))
      ;; jump to the start of the note (first body paragraph), past the src block
      (let ((note (save-restriction
                    (org-narrow-to-subtree)
                    (org-element-map (org-element-parse-buffer) 'paragraph
                      (lambda (p) (org-element-property :contents-begin p))
                      nil t))))
        (if note (goto-char note) (org-end-of-subtree))))))

(provide 'review)
;;; review.el ends here

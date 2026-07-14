;;; code-review.el --- Annotate code into review.org and feed it to an agent  -*- lexical-binding: t; -*-

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

(defgroup code-review nil
  "Annotate code into review.org and feed it to an agent."
  :group 'tools)

(defconst code-review--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where code-review.el and its bundled template.org live.")

(defcustom code-review-agent-message "@review.org"
  "Message `code-review-send' hands to `code-review-send-function'.
The default is a bare `@'-reference to the project's review.org (Claude
Code opens the file, whose `* COMMENT' heading from
`code-review-file-template' holds the instructions).  Adjust to suit your
agent."
  :type 'string)

(defcustom code-review-send-function nil
  "Function that hands the review to an agent.
Called by `code-review-send' with one argument, `code-review-agent-message',
after review.org is saved.  Set it to integrate with your agent — e.g. a
wrapper around claude-code-ide, aider, or gptel."
  :type '(choice (const :tag "None" nil) function))

(defcustom code-review-capture-template
  '("r" "Review note" entry
    (function code-review--goto-review-file)
    "* TODO %(code-review--link)\n%(code-review--src-block)\n%?"
    :empty-lines 1
    :after-finalize code-review--after-finalize)
  "`org-capture' template entry for leaving a review note."
  :type 'sexp)

(defcustom code-review-note-prefix "review: "
  "String prepended to each review note in its Flymake diagnostic message.
Marks notes from this backend apart from other `:note' diagnostics (eglot,
etc.) in the echo area, sideline, and `flymake-show-buffer-diagnostics'.
Set to \"rev: \" for a shorter tag, or \"\" to disable."
  :type 'string)


;;; Location helpers

(defconst code-review-file-name "review.org"
  "Base name of the per-project review file.")

(defun code-review--root ()
  "Project root (or `default-directory' outside a project) — review.org's dir."
  (or (ignore-errors (project-root (project-current))) default-directory))

(defun code-review-file ()
  "Path to the current project's review.org."
  (expand-file-name code-review-file-name (code-review--root)))

(defun code-review--review-file-p (path)
  "Non-nil when PATH is a review file, judged by its base name."
  (and path (equal (file-name-nondirectory path) code-review-file-name)))


;;; org-capture template helpers

(defun code-review--lang ()
  "Src-block language from the buffer `org-capture' was invoked in."
  (let* ((buf  (and (fboundp 'org-capture-get) (org-capture-get :original-buffer)))
         (mode (if (buffer-live-p buf) (buffer-local-value 'major-mode buf) major-mode)))
    ;; python-ts-mode -> python, go-mode -> go, emacs-lisp-mode -> emacs-lisp
    (replace-regexp-in-string (rx (? "-ts") "-mode" eos) "" (symbol-name mode))))

(defun code-review--src-block ()
  "Return a `#+begin_src' block for the captured text, or \"\" when empty.
The captured text is the region `org-capture' stashed as :initial."
  (let ((code (string-trim-right (or (org-capture-get :initial) ""))))
    (if (string-empty-p code)
        ""
      (format "#+begin_src %s\n%s\n#+end_src\n" (code-review--lang) code))))

(defvar code-review--here nil
  "Plist (:file :line :base) of the annotated spot.
Dynamically bound by `code-review-annotate' around the capture; :base is
review.org's directory, so the link is relative to where it is inserted.")

(defun code-review--make-link (file line base)
  "Org backlink to FILE at LINE, with FILE made relative to BASE."
  (format "[[file:%s::%d]]" (file-relative-name file base) line))

(defun code-review--relativize-link (link)
  "Rewrite the file path in LINK (\"[[file:PATH::…]]\") relative to review.org's dir."
  (let ((base (file-name-directory (code-review-file))))
    (if (string-match (rx "[[file:" (group (+? nonl)) (group (or "::" "]"))) link)
        ;; capture match positions BEFORE expand/relative clobber the match data
        (let ((start (match-beginning 1))
              (raw   (match-string 1 link))
              (tail  (substring link (match-beginning 2))))
          (concat (substring link 0 start)
                  (file-relative-name (expand-file-name raw) base)
                  tail))
      link)))

(defun code-review--link ()
  "Single-line backlink: path relative to review.org's dir + line number.
Outside `code-review-annotate' (e.g. the global `org-capture' menu)
`code-review--here' is unset; fall back to the capture annotation (%a),
also rewritten relative to review.org's directory."
  (let ((file (plist-get code-review--here :file))
        (line (plist-get code-review--here :line))
        (base (plist-get code-review--here :base)))
    (if (and file line base)
        (code-review--make-link file line base)
      (code-review--relativize-link
       (or (and (fboundp 'org-capture-get) (org-capture-get :annotation)) "")))))

(defcustom code-review-file-template
  (with-temp-buffer
    (insert-file-contents (expand-file-name "template.org" code-review--dir))
    (buffer-string))
  "Content inserted into a freshly created review.org.
Defaults to the bundled template.org, read when the package loads.  It
carries the `-*-' cookie (buffer-local save/revert hooks that re-push
notes to code buffers; whitelist them in `safe-local-variable-values'),
the `#+TODO:' status keywords, and a `* COMMENT Agent instructions'
heading — a real heading so it folds away, and \"COMMENT\" keeps
`code-review--parse' (which only matches headlines with a file: link) and
Org export from touching it.  Edit template.org before creating new
review.org files, not after."
  :type 'string)

(defun code-review--ensure-template ()
  "Set up review.org from the template when the current buffer is empty.
Insert `code-review-file-template', activate its `#+TODO:' keywords, and apply
the `-*-' hook cookie, so a freshly created (or empty) review.org wires up."
  (when (= (point-min) (point-max))
    (insert code-review-file-template)
    (org-set-regexps-and-options)        ; activate #+TODO
    (hack-local-variables)))             ; apply the -*- save/revert hooks

(defun code-review--goto-review-file ()
  "`org-capture' target function: visit the project's review.org at its end."
  (set-buffer (org-capture-target-buffer (code-review-file)))
  (code-review--ensure-template)
  (goto-char (point-max)))


;;; Commands

(defun code-review--bounds ()
  "Char bounds (BEG . END) of the active region, else of the current line."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (line-beginning-position) (line-end-position))))

;;;###autoload
(defun code-review-annotate ()
  "Leave a review note on the region, or the current line if no region.
The selection (or line) is captured into the entry's `#+begin_src' block."
  (interactive)
  (pcase-let ((`(,beg . ,end) (code-review--bounds)))
    (let ((org-capture-initial (buffer-substring-no-properties beg end))
          (code-review--here
           (list :file buffer-file-name
                 :line (line-number-at-pos beg)
                 :base (code-review--root))))
      (org-capture nil "r"))))

;;;###autoload
(defun code-review-open ()
  "Open the current project's review.org (creating it from the template if new)."
  (interactive)
  (find-file (code-review-file))
  (code-review--ensure-template))

;;;###autoload
(defun code-review-send ()
  "Save review.org and hand it to the agent via `code-review-send-function'."
  (interactive)
  (let ((file (code-review-file)))
    (unless (file-exists-p file)
      (user-error "No review.org yet — annotate first (`code-review-annotate')"))
    (unless (functionp code-review-send-function)
      (user-error "Set `code-review-send-function' to send the review"))
    (when-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf (when (buffer-modified-p) (save-buffer))))
    (funcall code-review-send-function code-review-agent-message)))


;;; Flymake backend — open (TODO) notes as :note diagnostics (eglot-style)

(defvar code-review--db (make-hash-table :test 'equal)
  "Cache: review.org path -> (SIGNAL . TABLE).
TABLE maps a target file's truename to a list of (LINE STATUS NOTE).")

(defun code-review--signal (review)
  "Change signal for REVIEW: buffer modification tick when visited, else mtime."
  (if-let* ((buf (find-buffer-visiting review)))
      (buffer-chars-modified-tick buf)
    (file-attribute-modification-time (file-attributes review))))

(defun code-review--parse (review)
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

(defun code-review-annotations (file)
  "Cached list of (LINE STATUS NOTE) for FILE from the project's review.org.
review.org is parsed once per change (keyed on `code-review--signal')."
  (let ((review (code-review-file)))
    (when (file-exists-p review)
      (let ((sig   (code-review--signal review))
            (entry (gethash review code-review--db)))
        (unless (and entry (equal (car entry) sig))
          (setq entry (cons sig (code-review--parse review)))
          (puthash review entry code-review--db))
        (gethash (file-truename file) (cdr entry))))))

(defvar-local code-review--report-fn nil
  "Stashed Flymake `report-fn' for this buffer (eglot-style).
Re-invoked by `code-review--report' when review.org changes, without
re-running `flymake-start'.")

(defun code-review--report (buffer)
  "Push current review notes to BUFFER via its stashed `code-review--report-fn'.
Replaces this backend's diagnostics (the :region covers the whole buffer)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when code-review--report-fn
        (let ((file buffer-file-name) diags)
          (when (and file (not (code-review--review-file-p file)))
            (save-excursion
              (dolist (a (code-review-annotations file))
                (unless (equal (nth 1 a) "DONE")          ; ignore DONE
                  (let ((note (nth 2 a)))
                    (goto-char (point-min))
                    (forward-line (1- (max 1 (nth 0 a))))  ; clamp line >= 1
                    (push (flymake-make-diagnostic
                           buffer (pos-bol) (pos-eol)
                           :note (concat code-review-note-prefix
                                         (if (string-empty-p note) "(no note)" note)))
                          diags))))))
          (funcall code-review--report-fn diags
                   :region (cons (point-min) (point-max))))))))

(defun code-review-flymake (report-fn &rest _)
  "Flymake backend: stash REPORT-FN and report current review notes.
Add to `flymake-diagnostic-functions'.  Notes come from review.org, not the
buffer text, so updates are pushed via `code-review--report' when review.org
changes (`code-review--after-finalize', `code-review--after-review-change'),
not on buffer edits."
  (setq code-review--report-fn report-fn)
  (code-review--report (current-buffer)))

(defun code-review--propagate (root)
  "Re-push review notes to every Flymake buffer under ROOT."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'code-review--report-fn buf)
      (let ((f (buffer-local-value 'buffer-file-name buf)))
        (when (and f (file-in-directory-p f root))
          (code-review--report buf))))))

(defun code-review--after-finalize ()
  "Push the freshly captured note to the source buffer (template :after-finalize)."
  (when-let* ((buf (org-capture-get :original-buffer)))
    (code-review--report buf)))

(defun code-review--after-review-change ()
  "On save/revert of a review.org, push notes to that project's buffers.
For `after-save-hook' and `after-revert-hook'."
  (when (code-review--review-file-p buffer-file-name)
    (code-review--propagate (file-name-directory buffer-file-name))))

;;;###autoload
(defun code-review-edit ()
  "Open the review.org note covering the current line/region, to edit it.
Matches an entry whose recorded line falls within the selection (or the
current line).  Errors when there is no note there."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (code-review--bounds))
               (file buffer-file-name)
               (lo (line-number-at-pos beg))
               (hi (line-number-at-pos end))
               (match (seq-find (lambda (a) (<= lo (nth 0 a) hi))
                                (and file (code-review-annotations file)))))
    (unless match (user-error "No review note on these lines"))
    (let* ((review (code-review-file))
           (link (code-review--make-link file (nth 0 match)
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

(provide 'code-review)
;;; code-review.el ends here

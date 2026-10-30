;;; tab-bar-project.el --- A tab-bar tab per project -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: convenience, frames, vc
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

;; One tab-bar tab per project.  A project tab stores its root in the
;; `tab-bar-project-root' tab parameter, which desktop saves.  Its name
;; and group are set once, on creation, by `tab-bar-project-name-function'
;; and `tab-bar-project-group-function', through the tab-bar commands so
;; their hooks run.  Tabs are looked up in the selected frame only.
;; `tab-bar-project-group-by-repository' groups the git working trees of
;; a repository.
;;
;; `tab-bar-project-mode' remaps `project-switch-project' and
;; `project-kill-buffers' to this package's commands, selects the tab of
;; a project chosen at the `project-prompter' prompt, and gives a buffer
;; outside any project, such as *scratch*, its tab's project.  A buffer
;; in another project keeps its own.
;;
;; The remapping affects key bindings only: Lisp callers of
;; `project-switch-project', e.g. `vc-working-tree-switch-project', get
;; no tab and should call `tab-bar-project-switch-project'.

;;; Code:

(require 'tab-bar)
(require 'project)
(require 'seq)
(require 'subr-x)


(defgroup tab-bar-project nil
  "A tab-bar tab per project."
  :group 'tab-bar
  :prefix "tab-bar-project-")

(defcustom tab-bar-project-name-function #'project-name
  "Function returning the name of a new project tab.
Called with one argument, PROJECT.  The default honors `project-vc-name'."
  :type 'function
  :group 'tab-bar-project)

(defcustom tab-bar-project-group-function nil
  "Function returning the group of a new project tab, or nil for none.
Called with one argument, PROJECT.  `tab-bar-project-group-by-repository'
puts the git working trees of a repository in one group."
  :type '(choice (const :tag "No group" nil) function)
  :group 'tab-bar-project)


(defvar tab-bar-project--prompting nil
  "Non-nil while a command of this package prompts for a project.")

(defun tab-bar-project--root (project)
  "Return PROJECT's root as an absolute directory name."
  (file-name-as-directory (expand-file-name (project-root project))))

(defvar tab-bar-project--repository-cache (make-hash-table :test #'equal)
  "Repository directory of each root.")

(defun tab-bar-project--repository (root)
  "Return the git repository directory of working tree ROOT.
ROOT itself, unless ROOT is a linked working tree."
  (with-memoization (gethash root tab-bar-project--repository-cache)
    ;; PERF: Parse the `.git' file; no git subprocess.
    (let ((dotgit (expand-file-name ".git" root)))
      ;; A directory name, so ROOT with or without a trailing slash
      ;; gives the same result.
      (file-name-as-directory
       (or (and (file-regular-p dotgit)
                (with-temp-buffer
                  (ignore-errors (insert-file-contents dotgit))
                  (goto-char (point-min))
                  (when (re-search-forward
                         "gitdir: \\(.+?\\)/\\(?:\\.git/\\)?worktrees/" nil t)
                    (match-string 1))))
           root)))))

;;;###autoload
(defun tab-bar-project-group-by-repository (project)
  "Return the name of the repository PROJECT is a working tree of.
For `tab-bar-project-group-function'."
  (let ((name (file-name-nondirectory
               (directory-file-name
                (tab-bar-project--repository (project-root project))))))
    ;; A bare repository `proj.git' gives the group `proj'.
    (string-remove-suffix ".git" name)))

;;;###autoload
(defun tab-bar-project-root (&optional tab)
  "Return the project root of TAB, the current tab by default, or nil."
  (alist-get 'tab-bar-project-root
             (or tab (assq 'current-tab (funcall tab-bar-tabs-function)))))

;;;###autoload
(defun tab-bar-project-contains-p (tab dir)
  "Return non-nil if TAB's project root contains directory DIR."
  (when-let* ((root (tab-bar-project-root tab)))
    ;; PERF: Expand DIR only if needed: `expand-file-name' scans
    ;; `file-name-handler-alist', and callers may test every tab on
    ;; each redisplay.
    (string-prefix-p root (if (string-prefix-p "/" dir)
                              dir
                            (expand-file-name dir)))))

(defun tab-bar-project--tab-index (root)
  "Return the index of the tab with project ROOT in the selected frame."
  (seq-position (funcall tab-bar-tabs-function) root
                (lambda (tab root) (equal (tab-bar-project-root tab) root))))

(defun tab-bar-project--project (dir)
  "Return the project in DIR, ignoring the tab's own project."
  (let ((project-find-functions
         (remq #'tab-bar-project-try-tab project-find-functions)))
    (project-current nil dir)))

;;;###autoload
(defun tab-bar-project-select (dir)
  "Select the tab of the project in DIR, creating it if needed.
Return non-nil if the tab was created."
  (let* ((project (or (tab-bar-project--project dir)
                      (user-error "No project in %s" dir)))
         (root (tab-bar-project--root project)))
    (if-let* ((index (tab-bar-project--tab-index root)))
        (progn (tab-bar-select-tab (1+ index)) nil)
      (tab-bar-new-tab)
      (setf (alist-get 'tab-bar-project-root
                       (cdr (assq 'current-tab (funcall tab-bar-tabs-function))))
            root)
      (tab-bar-rename-tab (funcall tab-bar-project-name-function project))
      ;; Also clears a group inherited per `tab-bar-new-tab-group'.
      (tab-bar-change-tab-group
       (or (and tab-bar-project-group-function
                (funcall tab-bar-project-group-function project))
           ""))
      t)))


;;;###autoload
(defun tab-bar-project-switch-project (dir)
  "Switch to the tab of the project in DIR.
A tab created for it also runs `project-switch-project' for DIR."
  (interactive (let ((tab-bar-project--prompting t))
                 (list (funcall project-prompter))))
  (when (tab-bar-project-select dir)
    (project-switch-project dir)))

;;;###autoload
(defun tab-bar-project-kill-buffers ()
  "Kill the current project's buffers, then close its tab."
  (interactive)
  (let* ((project (let ((tab-bar-project--prompting t))
                    (project-current t)))
         (root (tab-bar-project--root project)))
    (when (project-kill-buffers nil project)
      (when-let* ((index (tab-bar-project--tab-index root)))
        (tab-bar-close-tab (1+ index))))))


(defun tab-bar-project--select-prompted (dir)
  "Select the tab of the project DIR chosen at a prompt; return DIR.
A DIR without a project is left to `project-current', which makes a
transient project of it."
  (when (and (not tab-bar-project--prompting)
             dir
             (tab-bar-project--project dir))
    (tab-bar-project-select dir))
  dir)

(defun tab-bar-project-try-tab (dir)
  "Return the current tab's project if DIR is the current buffer's directory.
For `project-find-functions', after the other members."
  ;; Last member, so only a buffer outside any project reaches it.  Any
  ;; other DIR comes from a caller asking whether DIR is a project.
  (when-let* ((root (tab-bar-project-root))
              ((equal (file-name-as-directory (expand-file-name dir))
                      (file-name-as-directory
                       (expand-file-name default-directory)))))
    (tab-bar-project--project root)))

(defvar-keymap tab-bar-project-mode-map
  :doc "Keymap for `tab-bar-project-mode'."
  "<remap> <project-switch-project>" #'tab-bar-project-switch-project
  "<remap> <project-kill-buffers>" #'tab-bar-project-kill-buffers)

;;;###autoload
(define-minor-mode tab-bar-project-mode
  "Keep a tab-bar tab per project.
Remap `project-switch-project' and `project-kill-buffers' to
`tab-bar-project-switch-project' and `tab-bar-project-kill-buffers'.
Select the tab of a project chosen at the `project-prompter' prompt.
Give a buffer that belongs to no project its tab's project."
  :global t
  :group 'tab-bar-project
  (if tab-bar-project-mode
      (progn
        (add-function :filter-return project-prompter
                      #'tab-bar-project--select-prompted)
        (add-hook 'project-find-functions #'tab-bar-project-try-tab 90))
    (remove-function project-prompter #'tab-bar-project--select-prompted)
    (remove-hook 'project-find-functions #'tab-bar-project-try-tab)))

(provide 'tab-bar-project)
;;; tab-bar-project.el ends here

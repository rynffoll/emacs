#!/usr/bin/env -S emacs --script
;;; package-changes.el --- Print what to read for changed packages  -*- lexical-binding: t; -*-

;; Usage: package-changes.el OLD.tsv
;;
;; Compare the snapshot OLD.tsv of package-versions.el with the installed
;; packages that the config needs.  For each changed package, print a
;; block: the old and new versions, the copies between them, the old and
;; new directories, and the NEWS/CHANGELOG files of the new copy.  Run
;; from the repository root.

(load (expand-file-name "config-packages" (file-name-directory load-file-name))
      nil t)
(require 'package-vc)
(package-initialize)

(defun package-changes--snapshot (file)
  "Return an alist (NAME . VERSION-STRING) from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar (lambda (line)
              (let ((fields (split-string line "\t")))
                (cons (intern (car fields)) (cadr fields))))
            (split-string (buffer-string) "\n" t))))

(defun package-changes--version (desc)
  "Return the version string of DESC as package-versions.el prints it."
  (if (package-vc-p desc)
      (package-vc-commit desc)
    (package-version-join (package-desc-version desc))))

(defun package-changes--news (dir)
  "Return the NEWS and CHANGELOG files in DIR."
  (directory-files dir nil "\\`\\(?:NEWS\\|CHANGELOG\\|CHANGES\\)"))

(let* ((old (package-changes--snapshot (pop command-line-args-left)))
       (needed (config-packages)))
  (setq command-line-args-left nil)
  (let ((emacs (format "%s %s" emacs-version emacs-repository-version)))
    (unless (equal emacs (alist-get 'emacs old))
      (princ (format "emacs\n  old: %s\n  new: %s\n\n"
                     (alist-get 'emacs old) emacs))))
  (dolist (name (sort (remq 'emacs needed)))
    (let* ((descs (cdr (assq name package-alist)))
           (new (car descs))
           (old-version (alist-get name old)))
      (cond
       ((and (null new) old-version)
        (princ (format "%s\n  removed: %s\n\n" name old-version)))
       ((null new))
       ((null old-version)
        (princ (format "%s\n  added: %s\n\n"
                       name (package-changes--version new))))
       ((not (equal old-version (package-changes--version new)))
        (let* (;; `package-alist' copies are newest first.
               (old-desc (seq-find (lambda (d)
                                     (equal (package-changes--version d)
                                            old-version))
                                   descs))
               (between (cdr (memq new (reverse (cdr (memq old-desc
                                                          (reverse descs))))))))
          (princ (format "%s\n  old: %s\n  new: %s\n" name old-version
                         (package-changes--version new)))
          (when (and old-desc between)
            (princ (format "  between: %s\n"
                           (config-packages-versions (reverse between)))))
          (when old-desc
            (princ (format "  old-dir: %s\n"
                           (file-relative-name (package-desc-dir old-desc)))))
          (princ (format "  new-dir: %s%s\n"
                         (file-relative-name (package-desc-dir new))
                         (if (package-vc-p new) " (vc)" "")))
          (when-let* ((news (package-changes--news (package-desc-dir new))))
            (princ (format "  news: %s\n" (string-join news " "))))
          (terpri)))))))

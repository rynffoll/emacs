#!/usr/bin/env -S emacs --script
;;; package-gc.el --- Print or delete stale and unused packages  -*- lexical-binding: t; -*-

;; Copies past the active one and the newest old one, installed packages
;; older than the built-in they shadow, and packages that neither the
;; config nor `user-lisp/' needs.  With --delete, delete the extra copies
;; and the unused packages.  Run from the repository root.

(load (expand-file-name "config-packages" (file-name-directory load-file-name))
      nil t)
(package-initialize)
;; Without it, `package-delete' deletes package-quickstart.el.
(setq package-quickstart t)

(let ((delete (member "--delete" command-line-args-left))
      (needed (config-packages)))
  (setq command-line-args-left nil)
  ;; `package-alist' copies are newest first.
  (pcase-dolist (`(,name . ,descs) package-alist)
    (let ((builtin (package-builtin-package-version name))
          (version (package-desc-version (car descs))))
      (if (not (memq name needed))
          (progn
            (princ (format "%s\tunused: %s\n" name
                           (config-packages-versions descs)))
            (when delete
              (dolist (d descs)
                (package-delete d t t))))
        ;; Keep the active copy and one old one for rollback.
        (when-let* ((extra (nthcdr 2 descs)))
          (princ (format "%s\textra copies: %s\n" name
                         (config-packages-versions extra)))
          (when delete
            ;; FORCE: an active copy of a dependent package still needs
            ;; this name.  NOSAVE: keep `package-selected-packages'.
            (dolist (d extra)
              (package-delete d t t))))
        (when (and builtin (version-list-< version builtin))
          (princ (format "%s\t%s older than built-in %s, active: %s\n"
                         name (package-version-join version)
                         (package-version-join builtin)
                         (locate-library (symbol-name name)))))))))

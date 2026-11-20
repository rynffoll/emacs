#!/usr/bin/env -S emacs --script
;;; config-lint.el --- Lint the config  -*- lexical-binding: t; -*-

;; Loads the config, byte-compiles a link-commented tangle of config.org
;; and user-lisp/*.el, and finds options set to their standard value.
;; Prints FILE:LINE: LEVEL: MESSAGE, tangled lines mapped back to
;; config.org.

(require 'bytecomp)
(require 'cl-lib)
(require 'ob-tangle)

(setq default-directory user-emacs-directory)

(package-activate-all)
(prepare-user-lisp t)
(load (expand-file-name "init.el") nil t)

(defun lint--default-settings (file)
  "Return a lint message for each option FILE sets to its standard value."
  (let ((symbols-with-pos-enabled t)
        forms found)
    (with-temp-buffer
      (insert-file-contents file)
      (ignore-error end-of-file
        (while t
          (push (read-positioning-symbols (current-buffer)) forms))))
    (named-let walk ((form forms))
      (when (proper-list-p form)
        (when (memq (car form) '(setq setopt setq-default))
          (cl-loop for (var val) on (cdr form) by #'cddr
                   for value = (byte-run-strip-symbol-positions val)
                   when (and (symbolp var) (macroexp-const-p value)
                             (custom--standard-value-p
                              (bare-symbol var) (eval value t)))
                   do (push (list file (symbol-with-pos-pos var) :info
                                  (format-message
                                   "`%s' set to its default %S"
                                   (bare-symbol var) (eval value t)))
                            found)))
        (mapc #'walk form)))
    (nreverse found)))

(let* ((dir (make-temp-file "lint" t))
       (org (expand-file-name "config.org" dir))
       (init (expand-file-name "init.el" dir))
       (elc (expand-file-name "out.elc" dir))
       (byte-compile-dest-file-function (lambda (_) elc))
       (enable-local-variables nil)
       ;; Only what an upgrade breaks; `noruntime' is lazy-loading noise.
       (byte-compile-warnings '(obsolete unresolved callargs))
       messages)
  (unwind-protect
      (progn
        (copy-file "config.org" org)
        (let ((org-babel-default-header-args
               (cons '(:comments . "link") org-babel-default-header-args)))
          (org-babel-tangle-file org))
        ;; The first link comment pushes the cookie line down.
        (with-temp-file init
          (insert ";; -*- lexical-binding: t -*-\n")
          (insert-file-contents init))
        (let ((byte-compile-log-warning-function
               (lambda (string position _fill level)
                 (push (list byte-compile-current-file position level string)
                       messages))))
          (byte-compile-file init)
          (mapc #'byte-compile-file
                (directory-files-recursively user-lisp-directory
                                             "\\`[^.].*\\.el\\'")))
        (pcase-dolist (`(,file ,pos ,level ,string)
                       (nconc (nreverse messages) (lint--default-settings init)))
          (let* ((tangled (equal file init))
                 (line (with-current-buffer (find-file-noselect file)
                         (goto-char pos)
                         (ignore-errors
                           (when tangled (org-babel-tangle-jump-to-org))
                           (line-number-at-pos)))))
            (princ (format "%s:%s: %s: %s\n"
                           (if tangled "config.org" (file-relative-name file))
                           (or line "?")
                           (substring (symbol-name (or level :warning)) 1)
                           string)))))
    (delete-directory dir t)))

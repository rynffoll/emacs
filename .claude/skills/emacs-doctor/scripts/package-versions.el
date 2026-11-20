#!/usr/bin/env -S emacs --script
;;; package-versions.el --- Print installed versions  -*- lexical-binding: t; -*-

;; "NAME<TAB>VERSION" lines for Emacs and each package, newest copy; a
;; :vc package's version is its commit.

(require 'package-vc)

(package-load-all-descriptors)
(princ (format "emacs\t%s %s\n" emacs-version emacs-repository-version))
(pcase-dolist (`(,name ,desc . ,_) (sort package-alist :key #'car))
  (princ (format "%s\t%s\n" name
                 (if (package-vc-p desc)
                     (package-vc-commit desc)
                   (package-version-join (package-desc-version desc))))))

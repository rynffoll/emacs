;;; config-packages.el --- Packages the config needs  -*- lexical-binding: t; -*-

;; Loaded by the other scripts.  Run them from the repository root.

(require 'lisp-mnt)

(defun config-packages--use-package-names (form acc)
  "Push onto ACC the packages that `use-package' forms in FORM ensure."
  (when (consp form)
    (when (and (eq (car form) 'use-package) (symbolp (cadr form)))
      (let ((ensure (plist-member (cddr form) :ensure)))
        (pcase (cadr ensure)
          ;; `use-package-always-ensure' is t.
          ('nil (unless ensure (push (cadr form) acc)))
          ('t (push (cadr form) acc))
          ((and (pred symbolp) name) (push name acc))
          (`(,(and (pred symbolp) name) . ,_) (push name acc)))))
    (while (consp form)
      (setq acc (config-packages--use-package-names (car form) acc)
            form (cdr form))))
  acc)

(defun config-packages--roots ()
  "Return the packages that init.el and `user-lisp/' require."
  (let (roots)
    (with-temp-buffer
      (insert-file-contents "init.el")
      (condition-case nil
          (while t
            (setq roots (config-packages--use-package-names
                         (read (current-buffer)) roots)))
        (end-of-file)))
    (dolist (file (directory-files-recursively "user-lisp" "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (req (lm-package-requires))
          (push (car req) roots))))
    roots))

(defun config-packages ()
  "Return the roots and the packages that their active copies require.
Call it after `package-alist' is filled."
  (let ((todo (config-packages--roots))
        needed)
    (while todo
      (let ((name (pop todo)))
        (unless (memq name needed)
          (push name needed)
          (when-let* ((desc (cadr (assq name package-alist))))
            (dolist (req (package-desc-reqs desc))
              (push (car req) todo))))))
    needed))

(defun config-packages-versions (descs)
  "Return the versions of DESCS as one string."
  (mapconcat (lambda (d) (package-version-join (package-desc-version d)))
             descs " "))

(provide 'config-packages)

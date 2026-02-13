;;; dothttp-jq.el --- jq response processing for dothttp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;;; Commentary:

;; Process HTTP/gRPC responses with jq expressions.
;; Supports `> jq EXPR' for filtering and `> jq-set VAR EXPR' for
;; storing results as global variables.

;;; Code:

(require 'dothttp)


;;; Customization

(defcustom dothttp-jq-executable "jq"
  "Path to jq executable."
  :group 'dothttp
  :type 'string)


;;; Processing

(defun dothttp-jq--run (body expr)
  "Run jq EXPR on BODY string.  Return result string."
  (unless (executable-find dothttp-jq-executable)
    (user-error "Executable not found: %s" dothttp-jq-executable))
  (with-temp-buffer
    (insert body)
    (let ((exit-code (call-process-region (point-min) (point-max)
                                          dothttp-jq-executable
                                          t t nil expr)))
      (if (zerop exit-code)
          (string-trim (buffer-string))
        (format "jq error (exit %d): %s" exit-code (string-trim (buffer-string)))))))

(defun dothttp-jq-process (req body)
  "Process jq directives from REQ on response BODY.
Returns possibly modified BODY string.  Updates global variables for jq-set."
  (let ((jq-expr (plist-get req :jq-expr))
        (jq-set (plist-get req :jq-set)))
    (if (and body (or jq-expr jq-set))
        (let* ((expr (or jq-expr (cdr jq-set)))
               (result (dothttp-jq--run body expr)))
          (when jq-set
            (puthash (car jq-set) (string-trim result) dothttp--global-variables))
          (concat body "\n\n// jq: " expr "\n" result))
      body)))

(provide 'dothttp-jq)
;;; dothttp-jq.el ends here

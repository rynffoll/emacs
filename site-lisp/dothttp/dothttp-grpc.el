;;; dothttp-grpc.el --- gRPC support for dothttp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev

;;; Commentary:

;; gRPC backend for dothttp using grpcurl and grpcui.
;; Use `# @plaintext' directive to connect without TLS.
;;
;; Load this extension to enable GRPC method support:
;;   (require 'dothttp-grpc)

;;; Code:

(require 'dothttp)


;;; Customization

(defcustom dothttp-grpcurl-executable "grpcurl"
  "Path to grpcurl executable."
  :group 'dothttp
  :type 'string)

(defcustom dothttp-grpcui-executable "grpcui"
  "Path to grpcui executable."
  :group 'dothttp
  :type 'string)


;;; Variables

(defvar-local dothttp-grpc--processes nil
  "List of grpcui processes launched from this buffer.")


;;; Command Builder

(defun dothttp-grpc--parse-url (url)
  "Parse gRPC URL into (HOST-PORT . SERVICE/METHOD)."
  (if (string-match "\\`\\([^/]+\\)/\\(.*\\)\\'" url)
      (cons (match-string 1 url) (match-string 2 url))
    (cons url nil)))

(defun dothttp-grpc--base-args (req)
  "Build common grpcurl/grpcui args from REQ.
Return (ARGS HOST-PORT SERVICE-METHOD)."
  (let* ((url (plist-get req :url))
         (parsed (dothttp-grpc--parse-url url))
         (host-port (car parsed))
         (service-method (cdr parsed))
         (meta (plist-get req :meta))
         (plaintext (alist-get 'plaintext meta))
         (proto (alist-get 'proto meta))
         (args '()))
    (when plaintext
      (push "-plaintext" args))
    (when proto
      (let ((dir (file-name-directory proto))
            (file (file-name-nondirectory proto)))
        (push "-import-path" args)
        (push (or dir ".") args)
        (push "-proto" args)
        (push file args)))
    (list (nreverse args) host-port service-method)))

(defun dothttp-grpc--build-args (req)
  "Build grpcurl argument list from resolved request REQ."
  (pcase-let* ((`(,base-args ,host-port ,service-method) (dothttp-grpc--base-args req))
               (headers (plist-get req :headers))
               (body (plist-get req :body))
               (action (alist-get 'action (plist-get req :meta))))
    (append
     base-args
     (if (member action '("list" "describe"))
         (append (list host-port action)
                 (when (and service-method (not (string-empty-p service-method)))
                   (list service-method)))
       (append
        (mapcan (lambda (h)
                  (list "-H" (format "%s: %s" (car h) (cdr h))))
                headers)
        (list "-d" (or body "{}") host-port)
        (when service-method (list service-method)))))))


;;; Interactive Commands

;;;###autoload
(defun dothttp-open-grpcui ()
  "Launch grpcui for the gRPC service at point."
  (interactive)
  (unless (executable-find dothttp-grpcui-executable)
    (user-error "Executable not found: %s" dothttp-grpcui-executable))
  (pcase-let* ((req (dothttp--resolve-request (dothttp--parse-request-at-point)))
               (`(,base-args ,host-port ,_service) (dothttp-grpc--base-args req))
               (args (append base-args (list host-port)))
               (source-buf (current-buffer))
               (output-acc ""))
    (message "dothttp: launching grpcui for %s..." host-port)
    (let ((proc (make-process
                 :name "dothttp-grpcui"
                 :command (cons dothttp-grpcui-executable args)
                 :filter
                 (lambda (_proc chunk)
                   (setq output-acc (concat output-acc chunk))
                   (when (string-match "http://[^ \n]+" output-acc)
                     (browse-url (match-string 0 output-acc))))
                 :sentinel
                 (lambda (proc _event)
                   (when (eq (process-status proc) 'exit)
                     (when (buffer-live-p source-buf)
                       (with-current-buffer source-buf
                         (setq dothttp-grpc--processes
                               (delq proc dothttp-grpc--processes))))
                     (unless (zerop (process-exit-status proc))
                       (message "dothttp: grpcui exited with code %d"
                                (process-exit-status proc))))))))
      (push proc dothttp-grpc--processes))))

(defun dothttp-grpc--kill-processes ()
  "Kill all grpcui processes launched from this buffer."
  (dolist (proc dothttp-grpc--processes)
    (when (process-live-p proc)
      (delete-process proc)))
  (setq dothttp-grpc--processes nil))


;;; Registration

(add-to-list 'dothttp-methods "GRPC")
(dothttp-rebuild-font-lock)

(setf (alist-get "GRPC" dothttp-backend-alist nil nil #'string=)
      (list :executable 'dothttp-grpcurl-executable
            :build-args #'dothttp-grpc--build-args
            :raw-response t))

(define-key dothttp-mode-map (kbd "C-c C-o") #'dothttp-open-grpcui)
(add-hook 'dothttp-kill-buffer-hook #'dothttp-grpc--kill-processes)

(provide 'dothttp-grpc)
;;; dothttp-grpc.el ends here

;;; dothttp.el --- JetBrains-style HTTP client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ruslan Kamashev
;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: comm, tools
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

;; HTTP client for .http files compatible with JetBrains HTTP Client.
;; gRPC support via optional `dothttp-grpc' extension.
;; Backends: curl (HTTP), grpcurl (gRPC).  Response processing via jq.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)

(declare-function dothttp-jq-process "dothttp-jq")

(defvar json-ts-mode--font-lock-settings)
(defvar json-ts--indent-rules)


;;; Customization

(defgroup dothttp nil
  "JetBrains-style HTTP/gRPC client."
  :group 'tools)

(defcustom dothttp-curl-executable "curl"
  "Path to curl executable."
  :type 'string)

(defcustom dothttp-timeout 30
  "Default request timeout in seconds."
  :type 'integer)

(defcustom dothttp-response-display-alist
  '((display-buffer-in-side-window)
    (window-height . 0.4))
  "Display action for the response buffer."
  :type 'sexp)


;;; Variables

(defvar dothttp--global-variables (make-hash-table :test #'equal)
  "Global variables set by `jq-set' directives.")

(defvar-local dothttp--current-environment nil
  "Active environment name, or nil.")

(defvar-local dothttp--current-process nil
  "Running request process, or nil.")

(defvar dothttp-backend-alist nil
  "Alist mapping method to backend plist.
Each plist: (:executable SYMBOL :build-args FUNCTION :raw-response BOOL).
Methods not listed use curl.")

(defvar dothttp-kill-buffer-hook nil
  "Hook run when a dothttp buffer is killed.")


;;; Faces

(defface dothttp-method-face
  '((t :inherit font-lock-keyword-face))
  "Face for HTTP methods.")

(defface dothttp-variable-face
  '((t :inherit font-lock-constant-face))
  "Face for {{variable}} references.")

(defface dothttp-dynamic-variable-face
  '((t :inherit font-lock-function-name-face))
  "Face for {{$dynamicVariable}} references.")

(defface dothttp-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for ### request separators.")

(defface dothttp-tag-face
  '((t :inherit font-lock-builtin-face))
  "Face for @name, @proto and other tag directives.")

(defface dothttp-url-face
  '((t :inherit font-lock-string-face))
  "Face for request URLs.")

(defface dothttp-header-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for header names.")

(defface dothttp-header-value-face
  '((t :inherit font-lock-string-face))
  "Face for header values.")

(defface dothttp-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments.")

(defface dothttp-handler-face
  '((t :inherit font-lock-doc-face))
  "Face for jq handler lines.")


;;; Font Lock

(defvar dothttp-methods
  '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS" "TRACE")
  "Supported HTTP methods.
Extensions add methods via `add-to-list' then call `dothttp-rebuild-font-lock'.")

(defconst dothttp-tags
  '("@name" "@no-redirect" "@proto" "@action" "@plaintext")
  "Supported tags in comment directives.")

(defun dothttp--build-font-lock-keywords ()
  "Compute font-lock keywords from current `dothttp-methods'."
  `(;; Separator: ### lines
    ("^###.*$" (0 'dothttp-separator-face t))
    ;; Tags: @name, @proto, etc.
    (,(concat "^#\\s-*\\(" (regexp-opt dothttp-tags) "\\)")
     (1 'dothttp-tag-face t))
    ;; Request line: METHOD URL
    (,(concat "^" (regexp-opt dothttp-methods t) "\\s-+\\(.+\\)$")
     (1 'dothttp-method-face)
     (2 'dothttp-url-face))
    ;; Headers: Name: Value
    ("^\\([A-Za-z][A-Za-z0-9-]*\\):\\s-*\\(.*\\)$"
     (1 'dothttp-header-name-face)
     (2 'dothttp-header-value-face))
    ;; Variables {{var}}
    ("{{\\([^}]+\\)}}" (0 'dothttp-variable-face t))
    ;; Dynamic variables {{$...}} (overrides general variables above)
    ("{{\\(\\$[^}]+\\)}}" (0 'dothttp-dynamic-variable-face t))
    ;; Comment lines: # ... (but not ### separators)
    ("^\\(#\\(?:[ \t]\\|$\\).*\\)" (0 'dothttp-comment-face t))
    ;; Inline comments in headers
    ("^[A-Za-z][A-Za-z0-9-]*:[ \t]*.*?\\([ \t]+#.*\\)$"
     (1 'dothttp-comment-face t))
    ;; jq handler lines
    ("^>\\s-+jq\\(?:-set\\)?\\s-+.*$" . 'dothttp-handler-face)))

(defvar dothttp-font-lock-keywords (dothttp--build-font-lock-keywords)
  "Font lock keywords for `dothttp-mode'.")

(defvar dothttp--method-re
  (concat "^" (regexp-opt dothttp-methods) "\\s-+")
  "Regexp matching HTTP method at beginning of line.")

(defvar dothttp--body-end-re
  (concat "^\\(?:###\\|>[ \t]+jq\\)\\|" dothttp--method-re)
  "Regexp matching the end of a request body region.")

(defun dothttp-rebuild-font-lock ()
  "Rebuild regexps and font-lock after `dothttp-methods' changes."
  (setq dothttp--method-re
        (concat "^" (regexp-opt dothttp-methods) "\\s-+"))
  (setq dothttp--body-end-re
        (concat "^\\(?:###\\|>[ \t]+jq\\)\\|" dothttp--method-re))
  (setq dothttp-font-lock-keywords
        (dothttp--build-font-lock-keywords)))


;;; Imenu

(defvar dothttp-imenu-expressions
  '(("Request" "^###\\s-*\\(.+\\)$" 1)
    ("Named" "^#\\s-*@name\\s-+\\(.+\\)$" 1))
  "Imenu expressions for `dothttp-mode'.")


;;; Major Mode

(defvar dothttp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'dothttp-send-request)
    (define-key map (kbd "C-c C-k") #'dothttp-kill-request)
    (define-key map (kbd "C-c C-e") #'dothttp-select-environment)
    (define-key map (kbd "C-c C-r") #'dothttp-show-response)
    (define-key map (kbd "C-c C-n") #'dothttp-next-request)
    (define-key map (kbd "C-c C-p") #'dothttp-prev-request)
    map)
  "Keymap for `dothttp-mode'.")

(defvar dothttp-mode-syntax-table
  (let ((st (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `dothttp-mode'.")

(defun dothttp--syntax-propertize (start end)
  "Mark # as comment start between START and END.
Only matches `# ' (space/tab), `#@' (tags), and `#' at EOL.
Skips `###' separators and stray `#' inside request bodies."
  (goto-char start)
  (while (re-search-forward "^#\\(?:[ \t@]\\|$\\)" end t)
    (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                       'syntax-table (string-to-syntax "<"))))

;;;###autoload
(define-derived-mode dothttp-mode prog-mode "HTTP"
  "Major mode for editing JetBrains-style .http files."
  :syntax-table dothttp-mode-syntax-table
  (setq-local mode-name '("HTTP" (:eval (when dothttp--current-environment
                                          (format "[%s]" dothttp--current-environment)))))
  (setq-local font-lock-defaults '(dothttp-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local imenu-generic-expression dothttp-imenu-expressions)
  (setq-local syntax-propertize-function #'dothttp--syntax-propertize)
  (add-hook 'kill-buffer-hook #'dothttp--kill-buffer-cleanup nil t)
  (dothttp--setup-treesit))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.http\\'" . dothttp-mode))


;;; Parser

(defun dothttp--parse-meta (line)
  "Parse # @key value from LINE into (KEY . VALUE) or nil."
  (when (string-match "^#\\s-*@\\(\\w+\\)\\s-*\\(.*\\)$" line)
    (let ((key (intern (match-string 1 line)))
          (val (string-trim (match-string 2 line))))
      (cons key (if (string-empty-p val) t val)))))

(defun dothttp--try-parse-jq (line)
  "Parse > jq handler from LINE.
Return (:jq-set . (VAR . EXPR)), (:jq-expr . EXPR), or nil."
  (cond
   ((string-match "^>\\s-+jq-set\\s-+\\(\\S-+\\)\\s-+\\(.*\\)$" line)
    (cons :jq-set (cons (match-string 1 line) (string-trim (match-string 2 line)))))
   ((string-match "^>\\s-+jq\\s-+\\(.*\\)$" line)
    (cons :jq-expr (string-trim (match-string 1 line))))))

(defun dothttp--parse-request-at-bounds (beg end)
  "Parse request between BEG and END into a plist."
  (let ((lines (split-string (buffer-substring-no-properties beg end) "\n"))
        (state :pre)
        name method url headers body-lines meta jq-expr jq-set)
    (dolist (line lines)
      (let ((jq (and (not (eq state :pre)) (dothttp--try-parse-jq line))))
        (if jq
            (pcase (car jq)
              (:jq-set
               (when jq-set
                 (message "dothttp: warning: multiple jq-set handlers, using last"))
               (setq jq-set (cdr jq)))
              (:jq-expr
               (when jq-expr
                 (message "dothttp: warning: multiple jq handlers, using last"))
               (setq jq-expr (cdr jq))))
          (pcase state
            (:pre
             (cond
              ((string-match-p "^\\s-*$" line) nil)
              ((string-match "^###\\s-*\\(.*\\)$" line)
               (let ((n (string-trim (match-string 1 line))))
                 (unless (string-empty-p n) (setq name n))))
              ((string-match "^#\\s-*@" line)
               (when-let* ((m (dothttp--parse-meta line)))
                 (when (eq (car m) 'name) (setq name (cdr m)))
                 (push m meta)))
              ((string-match-p "^#" line) nil)
              ((string-match dothttp--method-re line)
               (setq method (string-trim-right (match-string 0 line)))
               (setq url (string-trim (substring line (match-end 0))))
               (setq state :headers))))
            (:headers
             (cond
              ((string-match-p "^\\s-*$" line)
               (setq state :body))
              ((string-match-p "^#" line) nil)
              ;; URL continuation: indented line before any headers
              ((and (null headers) (string-match "^\\s-+\\(\\S-.*\\)$" line))
               (setq url (concat url (string-trim (match-string 1 line)))))
              ((string-match "^\\([A-Za-z][A-Za-z0-9-]*\\):\\s-*\\(.*\\)$" line)
               (let ((hdr-name (match-string 1 line))
                     (val (string-trim (match-string 2 line))))
                 (when (string-match "[ \t]+#" val)
                   (setq val (string-trim-right (substring val 0 (match-beginning 0)))))
                 (push (cons hdr-name val) headers)))
              (t (push line body-lines)
                 (setq state :body))))
            (:body
             (push line body-lines))))))
    (when method
      ;; Strip trailing blank/comment lines from body
      (while (and body-lines
                  (let ((l (string-trim (car body-lines))))
                    (or (string-empty-p l) (string-prefix-p "#" l))))
        (setq body-lines (cdr body-lines)))
      (let ((body (string-trim (mapconcat #'identity (nreverse body-lines) "\n"))))
        (list :name name
              :method method
              :url url
              :headers (nreverse headers)
              :body (unless (string-empty-p body) body)
              :meta (nreverse meta)
              :jq-expr jq-expr
              :jq-set jq-set
              :bounds (cons beg end))))))

(defun dothttp--request-bounds-at-point ()
  "Return (BEG . END) of the request block at point."
  (save-excursion
    (let ((beg (if (re-search-backward "^###" nil t)
                   (line-beginning-position)
                 (point-min))))
      (goto-char (line-end-position))
      (let ((end (if (re-search-forward "^###" nil t)
                     (line-beginning-position)
                   (point-max))))
        (cons beg end)))))

(defun dothttp--parse-request-at-point ()
  "Parse the HTTP request at point."
  (let* ((bounds (dothttp--request-bounds-at-point))
         (req (dothttp--parse-request-at-bounds (car bounds) (cdr bounds))))
    (unless req
      (user-error "No valid request found at point"))
    req))


;;; Tree-sitter

(defun dothttp--body-ranges (_beg _end)
  "Find all JSON body ranges in buffer for the JSON parser.
BEG and END are ignored; ranges cover the whole buffer."
  (let ((ranges nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward dothttp--method-re nil t)
        (forward-line 1)
        ;; Skip non-blank lines (headers, comments, URL continuations)
        (while (and (not (eobp))
                    (not (looking-at-p "^[ \t]*$")))
          (forward-line 1))
        ;; Skip blank separator line(s)
        (while (and (not (eobp))
                    (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (let ((body-beg (point))
              (body-end (if (re-search-forward dothttp--body-end-re nil t)
                            (prog1 (match-beginning 0)
                              (goto-char (match-beginning 0)))
                          (point-max))))
          ;; Trim trailing blank lines
          (save-excursion
            (goto-char body-end)
            (skip-chars-backward " \t\n" body-beg)
            (unless (= (point) body-beg)
              (setq body-end (min body-end (line-beginning-position 2)))))
          (when (< body-beg body-end)
            (push (cons body-beg body-end) ranges)))))
    (treesit-parser-set-included-ranges
     (treesit-parser-create 'json)
     (or (nreverse ranges)
         (and (< (point-min) (point-max))
              `((,(point-min) . ,(1+ (point-min)))))))))

(defun dothttp--fontify-syntactically (beg end &optional loudly)
  "Fontify comments via syntax table and JSON bodies via tree-sitter.
BEG, END, and LOUDLY are passed to both fontifiers."
  (font-lock-default-fontify-syntactically beg end loudly)
  (treesit-font-lock-fontify-region beg end loudly))

(defun dothttp--indent-line ()
  "Indent current line: JSON rules in body regions, default elsewhere."
  (if (treesit-node-at (save-excursion (back-to-indentation) (point)) 'json)
      (treesit-indent)
    (indent-relative)))

(defun dothttp--setup-treesit ()
  "Set up tree-sitter JSON injection for body regions."
  (when (and (fboundp 'treesit-ready-p) (treesit-ready-p 'json t))
    (require 'json-ts-mode)
    (let ((parser (treesit-parser-create 'json)))
      (treesit-parser-add-notifier parser #'treesit--font-lock-notifier))
    (setq-local treesit-range-settings
                (treesit-range-rules #'dothttp--body-ranges))
    (setq-local treesit-font-lock-settings
                json-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((constant number pair string)
                  (escape-sequence)
                  (bracket delimiter error)))
    (setq-local font-lock-fontify-syntactically-function
                #'dothttp--fontify-syntactically)
    (setq-local treesit-simple-indent-rules json-ts--indent-rules)
    (setq-local indent-line-function #'dothttp--indent-line)
    (treesit-font-lock-recompute-features)))


;;; Environment & Variables

(defun dothttp--project-root ()
  "Return the project root directory, or `default-directory'."
  (or (when-let* ((proj (project-current)))
        (project-root proj))
      default-directory))

(defun dothttp--load-env-files ()
  "Load and merge http-client.env.json files."
  (let ((root (dothttp--project-root))
        (result (make-hash-table :test #'equal)))
    (dolist (dir (if (file-equal-p default-directory root)
                     (list root)
                   (list root default-directory)))
      (dolist (file '("http-client.env.json" "http-client.private.env.json"))
        (let ((path (expand-file-name file dir)))
          (when (file-exists-p path)
            (let ((data (json-parse-string
                         (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))
                         :object-type 'hash-table)))
              (maphash
               (lambda (env vars)
                 (let ((existing (gethash env result (make-hash-table :test #'equal))))
                   (maphash (lambda (k v) (puthash k v existing)) vars)
                   (puthash env existing result)))
               data))))))
    result))

(defun dothttp--env-names ()
  "Return list of available environment names."
  (let ((envs (dothttp--load-env-files))
        names)
    (maphash (lambda (k _v) (push k names)) envs)
    (nreverse names)))

(defun dothttp--random-string (length chars)
  "Generate a random string of LENGTH from CHARS."
  (let ((len (length chars)))
    (apply #'string (cl-loop repeat length collect (aref chars (random len))))))

(defun dothttp--gen-uuid ()
  "Generate a random UUID v4 string."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536) (random 65536)
          (logior (ash 4 12) (random 4096))
          (logior #x8000 (random 16384))
          (random 65536) (random 65536) (random 65536)))

(defun dothttp--gen-timestamp ()
  "Generate UNIX timestamp."
  (format-time-string "%s"))

(defun dothttp--gen-iso-timestamp ()
  "Generate ISO-8601 timestamp."
  (format-time-string "%FT%T%z"))

(defun dothttp--gen-random-int ()
  "Generate random integer 0-999."
  (number-to-string (random 1000)))

(defun dothttp--gen-random-email ()
  "Generate random email address."
  (format "%s@%s.example.com"
          (dothttp--random-string 8 "abcdefghijklmnopqrstuvwxyz")
          (dothttp--random-string 5 "abcdefghijklmnopqrstuvwxyz")))

(defun dothttp--gen-random-integer (args)
  "Generate random integer in [from, to) from ARGS."
  (let ((from (string-to-number (nth 0 args)))
        (to (string-to-number (nth 1 args))))
    (number-to-string (+ from (random (- to from))))))

(defun dothttp--gen-random-float (args)
  "Generate random float in [from, to) from ARGS."
  (let ((from (string-to-number (nth 0 args)))
        (to (string-to-number (nth 1 args))))
    (number-to-string (+ from (* (- to from) (/ (float (random 1000000)) 1000000))))))

(defun dothttp--make-random-string-generator (chars)
  "Return a generator function for random strings from CHARS."
  (lambda (args)
    (dothttp--random-string (string-to-number (car args)) chars)))

(defconst dothttp--dynamic-variable-alist
  `(("$uuid"                . dothttp--gen-uuid)
    ("$random.uuid"         . dothttp--gen-uuid)
    ("$timestamp"           . dothttp--gen-timestamp)
    ("$isoTimestamp"        . dothttp--gen-iso-timestamp)
    ("$randomInt"           . dothttp--gen-random-int)
    ("$random.email"        . dothttp--gen-random-email)
    ("$random.integer"      . dothttp--gen-random-integer)
    ("$random.float"        . dothttp--gen-random-float)
    ("$random.alphabetic"   . ,(dothttp--make-random-string-generator
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    ("$random.alphanumeric" . ,(dothttp--make-random-string-generator
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"))
    ("$random.hexadecimal"  . ,(dothttp--make-random-string-generator
                                "0123456789abcdef")))
  "Alist mapping dynamic variable names to generator functions.
Simple variables use nullary functions.
Parameterized variables use functions taking a list of string args.")

(defconst dothttp-dynamic-variables
  (mapcar #'car dothttp--dynamic-variable-alist)
  "Supported dynamic variables.")

(defun dothttp--dynamic-variable (name)
  "Return value for dynamic variable NAME, or nil."
  (if (string-match "\\`\\(\\$[^(]+\\)(\\(.*\\))\\'" name)
      (when-let* ((fn (alist-get (match-string 1 name)
                                 dothttp--dynamic-variable-alist
                                 nil nil #'string=)))
        (funcall fn (split-string (match-string 2 name) "," t "\\s-*")))
    (when-let* ((fn (alist-get name dothttp--dynamic-variable-alist
                               nil nil #'string=)))
      (funcall fn))))

(defun dothttp--resolve-variable (name env-data)
  "Resolve variable NAME using ENV-DATA.
Priority: global > env > dynamic."
  (or (gethash name dothttp--global-variables)
      (when-let* ((env dothttp--current-environment)
                  (env-vars (gethash env env-data)))
        (gethash name env-vars))
      (dothttp--dynamic-variable name)
      (user-error "Undefined variable: %s" name)))

(defun dothttp--interpolate (str env-data)
  "Replace {{variable}} references in STR using ENV-DATA."
  (when str
    (let ((parts '())
          (pos 0))
      (while (string-match "{{\\([^}]+\\)}}" str pos)
        (let ((name (match-string 1 str))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (push (substring str pos beg) parts)
          (push (dothttp--resolve-variable name env-data) parts)
          (setq pos end)))
      (push (substring str pos) parts)
      (apply #'concat (nreverse parts)))))

(defun dothttp--resolve-request (req)
  "Return a copy of REQ with all {{variables}} interpolated."
  (let ((env-data (dothttp--load-env-files))
        (resolved (copy-sequence req)))
    (plist-put resolved :url (dothttp--interpolate (plist-get req :url) env-data))
    (plist-put resolved :body (dothttp--interpolate (plist-get req :body) env-data))
    (plist-put resolved :headers
               (mapcar (lambda (h)
                         (cons (car h) (dothttp--interpolate (cdr h) env-data)))
                       (plist-get req :headers)))
    resolved))


;;; Curl Command Builder

(defun dothttp--build-curl-args (req)
  "Build curl argument list from resolved request REQ."
  (let* ((method (plist-get req :method))
         (url (plist-get req :url))
         (headers (plist-get req :headers))
         (body (plist-get req :body))
         (meta (plist-get req :meta))
         (no-redirect (alist-get 'no-redirect meta))
         (args '()))
    (push "-s" args)
    (push "-i" args)
    (unless (string= method "GET")
      (push "-X" args)
      (push method args))
    (unless no-redirect
      (push "-L" args))
    (push "-m" args)
    (push (number-to-string dothttp-timeout) args)
    (dolist (header headers)
      (push "-H" args)
      (push (format "%s: %s" (car header) (cdr header)) args))
    (when body
      (push "-d" args)
      (push body args))
    (push url args)
    (nreverse args)))


;;; Response Processing

(defun dothttp--response-buffer-name ()
  "Return the per-project response buffer name."
  (project-prefixed-buffer-name "dothttp-response"))

(defun dothttp--extract-status-line (output)
  "Extract the last HTTP status line from OUTPUT."
  (let ((status nil)
        (pos 0))
    (while (string-match "\\(?:\\`\\|\n\\)\\(HTTP/[0-9.]+ [0-9]+[^\n]*\\)" output pos)
      (setq status (string-trim (match-string 1 output)))
      (setq pos (match-end 0)))
    status))

(defun dothttp--extract-content-type (output)
  "Extract Content-Type from HTTP response OUTPUT."
  (let ((case-fold-search t))
    (when (string-match "^content-type:\\s-*\\([^\n;]+\\)" output)
      (string-trim (downcase (match-string 1 output))))))

(defun dothttp--split-response (output)
  "Split HTTP response OUTPUT into (HEADERS . BODY)."
  (let ((pos 0) header-end body-start)
    (while (string-match "\r?\n\r?\n" output pos)
      (setq header-end (match-beginning 0))
      (setq body-start (match-end 0))
      (setq pos body-start))
    (if body-start
        (cons (substring output 0 header-end)
              (substring output body-start))
      (cons nil output))))

(defun dothttp--response-mode (content-type action raw-p exit-code)
  "Determine major mode for response.
CONTENT-TYPE, ACTION, RAW-P, EXIT-CODE control selection."
  (cond
   ((or (not (zerop exit-code))
        (member action '("describe" "list")))
    #'fundamental-mode)
   (raw-p #'json-ts-mode)
   ((null content-type) #'fundamental-mode)
   ((string-match-p "json\\|javascript" content-type) #'json-ts-mode)
   ((string-match-p "xml" content-type) #'xml-mode)
   ((string-match-p "html" content-type) #'html-mode)
   (t #'fundamental-mode)))

(defun dothttp--format-header-line (status elapsed body exit-code)
  "Format header-line from STATUS, ELAPSED time, BODY, and EXIT-CODE."
  (let* ((status-code (when (and status (string-match "\\([0-9]+\\)" status))
                        (string-to-number (match-string 1 status))))
         (status-face (cond
                       ((not (zerop exit-code)) 'error)
                       ((null status-code) 'success)
                       ((< status-code 300) 'success)
                       ((< status-code 400) 'warning)
                       (t 'error))))
    (list " "
          (propertize (or status "Response") 'face status-face)
          (format " | %dms | %d bytes"
                  (round (* elapsed 1000))
                  (if body (string-bytes body) 0)))))

(defun dothttp--display-response (req output elapsed exit-code)
  "Display response OUTPUT for resolved REQ in the response buffer."
  (let* ((method (plist-get req :method))
         (url (plist-get req :url))
         (jq-expr (plist-get req :jq-expr))
         (jq-set (plist-get req :jq-set))
         (backend (alist-get method dothttp-backend-alist nil nil #'string=))
         (raw-p (and backend (plist-get backend :raw-response)))
         (status (unless raw-p (dothttp--extract-status-line output)))
         (split (if raw-p (cons nil output) (dothttp--split-response output)))
         (headers (car split))
         (body (cdr split))
         (content-type (unless raw-p (dothttp--extract-content-type output)))
         (action (alist-get 'action (plist-get req :meta)))
         (mode (dothttp--response-mode content-type action raw-p exit-code))
         (buf (get-buffer-create (plist-get req :response-buffer))))
    ;; jq processing
    (when (and body (or jq-expr jq-set))
      (require 'dothttp-jq)
      (setq body (dothttp-jq-process req body)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall mode)
        (local-set-key (kbd "q") #'quit-window)
        (when (fboundp 'evil-local-set-key)
          (evil-local-set-key 'normal "q" #'quit-window))
        ;; Request line
        (let ((start (point)))
          (insert (format "%s %s\n" method url))
          (when comment-start
            (comment-region start (point))))
        ;; Full request details (C-u C-c C-c)
        (when (plist-get req :show-request)
          (let ((start (point)))
            (dolist (h (plist-get req :headers))
              (insert (format "%s: %s\n" (car h) (cdr h))))
            (when comment-start
              (comment-region start (point))))
          (when (plist-get req :body)
            (insert "\n" (plist-get req :body) "\n")))
        (insert "\n")
        ;; Response headers as comments
        (when headers
          (let ((start (point)))
            (insert headers "\n")
            (when comment-start
              (comment-region start (point)))
            (insert "\n")))
        ;; Body
        (when body
          (let ((body-start (point)))
            (insert body)
            (when (and (string-match-p "\\`\\s-*[{[]" body)
                       (eq mode #'json-ts-mode))
              (condition-case nil
                  (json-pretty-print body-start (point-max))
                (error nil)))))
        (unless (eq (char-before (point-max)) ?\n)
          (goto-char (point-max))
          (insert "\n"))
        (goto-char (point-min))
        (setq-local header-line-format
                    (dothttp--format-header-line status elapsed body exit-code))
        (setq buffer-read-only t)))
    (display-buffer buf dothttp-response-display-alist)))


;;; Async Execution

(defun dothttp--send (req)
  "Send resolved request REQ asynchronously."
  (let* ((req (append (list :response-buffer (dothttp--response-buffer-name)) req))
         (method (plist-get req :method))
         (backend (alist-get method dothttp-backend-alist nil nil #'string=))
         (executable (if backend
                         (symbol-value (plist-get backend :executable))
                       dothttp-curl-executable))
         (url (plist-get req :url))
         (name (plist-get req :name))
         (args (if backend
                   (funcall (plist-get backend :build-args) req)
                 (dothttp--build-curl-args req)))
         (source-buf (current-buffer))
         (start-time (current-time))
         (output-buf (generate-new-buffer " *dothttp-output*")))
    (unless (executable-find executable)
      (user-error "Executable not found: %s" executable))
    (message "dothttp: sending %s %s..." method url)
    (setq dothttp--current-process
          (make-process
           :name (format "dothttp: %s %s" method (or name url))
           :buffer output-buf
           :command (cons executable args)
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (when (buffer-live-p source-buf)
                 (with-current-buffer source-buf
                   (setq dothttp--current-process nil)))
               (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
                      (output (replace-regexp-in-string
                               "\r\\|\e\\[[0-9;]*m"
                               ""
                               (with-current-buffer output-buf (buffer-string))))
                      (exit-code (process-exit-status proc)))
                 (kill-buffer output-buf)
                 (dothttp--display-response req output elapsed exit-code)
                 (if (zerop exit-code)
                     (message "dothttp: %s — done (%.2fs)"
                              (or name "Request") elapsed)
                   (message "dothttp: %s — failed (exit %d, %.2fs)"
                            (or name "Request") exit-code elapsed)))))))))


;;; Interactive Commands

;;;###autoload
(defun dothttp-send-request (show-request)
  "Send request at point.  With SHOW-REQUEST prefix arg, include request in output."
  (interactive "P")
  (when (and dothttp--current-process (process-live-p dothttp--current-process))
    (unless (y-or-n-p (format "Request `%s' is running.  Cancel it?"
                             (process-name dothttp--current-process)))
      (user-error "Aborted"))
    (dothttp-kill-request))
  (let ((req (dothttp--resolve-request (dothttp--parse-request-at-point))))
    (when show-request
      (setq req (plist-put req :show-request t)))
    (dothttp--send req)))

(defun dothttp-kill-request ()
  "Cancel the running request."
  (interactive)
  (if (and dothttp--current-process (process-live-p dothttp--current-process))
      (progn
        (delete-process dothttp--current-process)
        (setq dothttp--current-process nil)
        (message "dothttp: request cancelled"))
    (message "dothttp: no running request")))

(defun dothttp-select-environment ()
  "Select or clear the active environment."
  (interactive)
  (let* ((envs (dothttp--env-names))
         (env (completing-read
               (if dothttp--current-environment
                   (format "Environment (current: %s, empty to clear): "
                           dothttp--current-environment)
                 "Environment: ")
               envs nil nil)))
    (setq dothttp--current-environment (unless (string-empty-p env) env))
    (force-mode-line-update)
    (if dothttp--current-environment
        (message "dothttp: environment set to '%s'" dothttp--current-environment)
      (message "dothttp: environment cleared"))))

(defun dothttp-next-request ()
  "Jump to next request."
  (interactive)
  (end-of-line)
  (if (re-search-forward dothttp--method-re nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max))
    (message "No more requests")))

(defun dothttp-prev-request ()
  "Jump to previous request."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward dothttp--method-re nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-min))
    (message "No previous requests")))

(defvar dothttp-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'dothttp-next-request)
    (define-key map (kbd "p") #'dothttp-prev-request)
    map)
  "Repeat map for request navigation.")

(put 'dothttp-next-request 'repeat-map 'dothttp-navigation-repeat-map)
(put 'dothttp-prev-request 'repeat-map 'dothttp-navigation-repeat-map)

(defun dothttp-show-response ()
  "Display the response buffer."
  (interactive)
  (if-let* ((buf (get-buffer (dothttp--response-buffer-name)))
            (win (display-buffer buf dothttp-response-display-alist)))
      (select-window win)
    (message "No response buffer")))

(defun dothttp-clear-variables ()
  "Clear all global variables set by jq-set."
  (interactive)
  (clrhash dothttp--global-variables)
  (message "dothttp: global variables cleared"))

(defun dothttp--kill-buffer-cleanup ()
  "Clean up when a dothttp buffer is killed."
  (run-hooks 'dothttp-kill-buffer-hook))

(provide 'dothttp)
;;; dothttp.el ends here

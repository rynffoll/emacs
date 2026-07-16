;;; package-report.el --- Installed-package × archive report  -*- lexical-binding: t; -*-

;; Author: Ruslan Kamashev
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: tools
;; URL: https://github.com/rynffoll/emacs

;;; Commentary:

;; `M-x package-report' builds a matrix of every installed package across
;; the configured archives (which one is the install target, what is pinned,
;; what is upgradable), and opens it in an xwidget-webkit buffer.  Data is
;; collected into a native
;; Elisp structure (`package-report-data'), serialized to JSON, and injected
;; along with the theme and date into the sibling `template.html' (which
;; carries `__DATA__' / `__THEME__' / `__DATE__' placeholders).

;;; Code:

(require 'package)
(require 'package-vc)
(require 'lisp-mnt)
(require 'seq)
(require 'json)
(require 'color)

(defconst package-report--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding this file and its sibling `template.html'.")

(defconst package-report--file
  (locate-user-emacs-file ".cache/package-report.html")
  "Path of the generated report HTML.
An inspection artifact for opening in a normal browser — the display
path in `package-report' uses a data: URI, not this file.")

;;; Theme — derive the report palette from the active Emacs theme

(defun package-report--color (face attr fallback)
  "Return FACE's ATTR color as a string, or FALLBACK when unavailable."
  (let ((c (face-attribute face attr nil t)))
    (if (and (stringp c) (color-defined-p c)) c fallback)))

(defun package-report--theme-css ()
  "Return a `:root { … }' CSS block with colors from the active Emacs theme.
`base' is the pure default background (used for cards and as the blend
target for soft tints); the page background is a hair off it."
  (let* ((base   (package-report--color 'default :background "#ffffff"))
         (ink    (package-report--color 'default :foreground "#1b1e24"))
         (accent (package-report--color 'font-lock-keyword-face :foreground "#7c5cbf"))
         (warn   (package-report--color 'warning :foreground "#c67a10"))
         (pick   (package-report--color 'font-lock-function-name-face :foreground "#2f6fda"))
         (base-rgb (color-name-to-rgb base))
         ;; blend COLOR over the background: RATIO of COLOR, (1-RATIO) of base
         (bl (lambda (color ratio)
               (apply #'color-rgb-to-hex
                      (append (color-blend (color-name-to-rgb color) base-rgb ratio)
                              '(2))))))
    (format
     ":root {
  --bg: %s; --panel: %s;
  --ink: %s; --ink-soft: %s; --ink-faint: %s;
  --line: %s; --line-strong: %s;
  --accent: %s; --accent-soft: %s;
  --warn: %s; --warn-soft: %s;
  --pick-soft: %s;
  --mono: ui-monospace, monospace;
  --sans: system-ui, sans-serif;
  color-scheme: %s;
}"
     base (funcall bl ink 0.07)          ; --bg (theme bg), --panel (raised card)
     ink (funcall bl ink 0.62) (funcall bl ink 0.42)   ; ink / soft / faint
     (funcall bl ink 0.14) (funcall bl ink 0.24)       ; line / line-strong
     accent (funcall bl accent 0.14)                   ; accent / soft
     warn (funcall bl warn 0.16)                        ; warn / soft
     (funcall bl pick 0.16)                            ; pick-soft
     (if (color-dark-p base-rgb) "dark" "light"))))    ; color-scheme (native controls)

;;; Data collection

(defun package-report-data ()
  "Return the package × archive matrix as a native Elisp structure.
A plist: (:cols ARCHIVE-NAMES :packages LIST-OF-ALISTS).  Each package
alist is symbol-keyed, ready for `json-serialize': name, kind, installed,
pick, url, pin, outdated, ver.
`kind' is vc/archive/local; `pick' is the archive package.el would install
from (the first desc in `package-archive-contents', kept sorted by
\(priority . version)); `pin' comes from `package-pinned-packages'; `ver'
is an alist archive-name -> version for the archives that carry the
package.  `local' rows are the hand-written `user-lisp-directory'
packages.  Archive rows come first (sorted), then local."
  (let* ((cols (mapcar #'car package-archives))
         ;; Canonical "an upgrade is available" set — the same one
         ;; `package-upgrade-all' acts on; excludes vc and built-ins.
         (upgradeable (package--upgradeable-packages))
         ;; Installed via package.el (archive + vc), in sorted-name order.
         (archive-pkgs
          (mapcar
           (lambda (name)
             (let* ((desc (package-get-descriptor name 'installed))
                    (descs (cdr (assq name package-archive-contents)))
                    ;; archive-contents descs are sorted by (priority . version):
                    ;; the first one is what package.el would actually install.
                    (pick (car descs))
                    (url (or (alist-get :url (and pick (package-desc-extras pick)))
                             (alist-get :url (package-desc-extras desc))
                             ""))
                    (pin (or (alist-get name package-pinned-packages) ""))
                    ;; archive-name -> version it carries.  Within one archive
                    ;; priorities are equal, so its first desc in the sorted
                    ;; `descs' is also its highest version; seq-keep drops
                    ;; archives that don't carry the package.
                    (ver (seq-keep
                          (lambda (arch)
                            (when-let* ((d (seq-find
                                            (lambda (d)
                                              (equal (package-desc-archive d) arch))
                                            descs)))
                              (cons (intern arch)
                                    (package-version-join (package-desc-version d)))))
                          cols)))
               `((name . ,(symbol-name name))
                 (kind . ,(if (package-vc-p desc) "vc" "archive"))
                 (installed . ,(package-version-join (package-desc-version desc)))
                 (pick . ,(if pick (package-desc-archive pick) ""))
                 (url . ,url)
                 (pin . ,pin)
                 ;; outdated: package.el has a newer archive version available.
                 (outdated . ,(if (memq name upgradeable) t :false))
                 ;; empty ver (nil) serializes as {}
                 (ver . ,ver))))
           (sort (mapcar #'car package-alist) #'string<)))
         ;; Hand-written local packages (Emacs 31+ user-lisp/), in dir order.
         (local-pkgs
          (when (and (boundp 'user-lisp-directory)
                     (file-directory-p user-lisp-directory))
            (seq-keep
             (lambda (dir)
               (when-let* (((file-directory-p dir))
                           (els (directory-files dir nil "\\.el\\'")))
                 (let* ((name (file-name-nondirectory dir))
                        (main (expand-file-name
                               (if (member (concat name ".el") els)
                                   (concat name ".el")
                                 (car els))
                               dir))
                        (version (with-temp-buffer
                                   (insert-file-contents main)
                                   (or (lm-package-version) ""))))
                   `((name . ,name)
                     (kind . "local")
                     (installed . ,version)
                     (pick . "")
                     (url . "")
                     (pin . "")
                     (outdated . :false)
                     (ver . ())))))
             (sort (directory-files user-lisp-directory t "^[^.]") #'string<)))))
    (list :cols cols :packages (append archive-pkgs local-pkgs))))

;;; Summary helper

(defun package-report--summary (packages)
  "Return a short text summary of PACKAGES (tile counts, vc/local names)."
  (let* ((by-kind (seq-group-by (lambda (p) (alist-get 'kind p)) packages))
         (names (lambda (k) (mapcar (lambda (p) (alist-get 'name p))
                                    (cdr (assoc k by-kind)))))
         (vc (funcall names "vc"))
         (local (funcall names "local"))
         (n-upd (seq-count (lambda (p) (eq t (alist-get 'outdated p))) packages)))
    (string-join
     (list
      (format "TILES: total=%d (archive=%d vc=%d local=%d)  outdated=%d"
              (length packages) (length (cdr (assoc "archive" by-kind)))
              (length vc) (length local) n-upd)
      (format "VC: %s" (string-join vc ", "))
      (format "LOCAL: %s" (string-join local ", ")))
     "\n")))

(defun package-report--fill (template alist)
  "Replace each __TOKEN__ in TEMPLATE by its value in ALIST, in one pass.
Unknown tokens are left as-is; inserted values are not re-scanned."
  (replace-regexp-in-string
   "__[A-Z]+__"
   (lambda (m) (or (cdr (assoc m alist)) m))
   template t t))

(defun package-report--build ()
  "Collect data, render the report HTML, and write it to `package-report--file'.
Builds the matrix and injects the JSON payload and theme into the sibling
`template.html'.  Reports on the live session as it is — run
`package-refresh-contents' first for fresh archive data.  Return a plist
(:html HTML :summary TEXT)."
  (let* ((data (package-report-data))
         (packages (plist-get data :packages))
         ;; One JSON payload: archive order and the self-contained package
         ;; matrix, exactly as `package-report-data' emits it.
         (json (json-serialize
                (list (cons 'cols (vconcat (plist-get data :cols)))
                      (cons 'packages (vconcat packages)))))
         (tpl (expand-file-name "template.html" package-report--dir))
         (template (with-temp-buffer (insert-file-contents tpl) (buffer-string)))
         (html (package-report--fill
                template `(("__DATA__"  . ,json)
                           ("__THEME__" . ,(package-report--theme-css))
                           ("__DATE__"  . ,(format-time-string "%Y-%m-%d %H:%M"))))))
    (make-directory (file-name-directory package-report--file) t)
    (with-temp-file package-report--file (insert html))
    (list :html html :summary (package-report--summary packages))))

;;;###autoload
(defun package-report ()
  "Build the package × archive report and show it in an `xwidget-webkit' buffer.
The page is passed as a `data:' URI rather than a `file://' one so it renders
everywhere (WebKit shows a blank page for `file://' local files on macOS)."
  (interactive)
  (unless (and (display-graphic-p) (featurep 'xwidget-internal))
    (user-error "package-report requires a graphical Emacs with xwidget support"))
  (let ((url (concat "data:text/html;charset=utf-8;base64,"
                     (base64-encode-string
                      (encode-coding-string (plist-get (package-report--build) :html) 'utf-8)
                      t))))
    (xwidget-webkit-browse-url url)))

(provide 'package-report)
;;; package-report.el ends here

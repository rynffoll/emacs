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
  (expand-file-name (locate-user-emacs-file ".cache/package-report.html"))
  "Path of the generated report HTML, which `package-report' then opens.")

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

;;; Tag lag — how far the latest tag trails the tracked branch

(defun package-report--git-lines (&rest args)
  "Run git ARGS in `default-directory', returning stdout as a list of lines.
Stderr is discarded, unlike `process-lines' (which merges it into the
same buffer) — a git warning like \"refname `HEAD\\=' is ambiguous\" would
otherwise land as a spurious first line ahead of the real output.
Return nil if git exits non-zero."
  (with-temp-buffer
    (when (= 0 (apply #'call-process "git" nil (list t nil) nil args))
      (split-string (buffer-string) "\n" t))))

(defun package-report--git-ok-p (&rest args)
  "Run git ARGS in `default-directory'; return t if it exits 0.
For checks like \"does this object exist\" where the command has no
output on success, so `package-report--git-lines' (empty stdout looks
the same as failure) can't tell success from failure."
  (= 0 (apply #'call-process "git" nil nil nil args)))

(defconst package-report--repo-cache-dir
  (expand-file-name "package-report/repos/" (locate-user-emacs-file ".cache/"))
  "Directory of cached mirror clones (for archive packages' tag lag) and
of vc packages' fetch-throttle markers (`package-report--vc-fetch-marker').
Everything `package-report' writes as bookkeeping lives here — never in
a package's own checkout, which this tool doesn't own and the user may
inspect or operate on directly (`dired', `magit-status', …).")

(defconst package-report--repo-refresh-interval (* 6 60 60)
  "Minimum seconds between `git fetch' refreshes of one package's repo.
Refreshing every package's repo on every report build would mean one
network round trip per package (there can be hundreds); this caps how
often that cost is paid per package.")

(defun package-report--marker-stale-p (marker)
  "Return t if MARKER (a timestamp file) is stale or was never written."
  (or (not (file-exists-p marker))
      (> (- (float-time) (float-time (file-attribute-modification-time
                                       (file-attributes marker))))
         package-report--repo-refresh-interval)))

(defun package-report--touch-marker (marker)
  "Record MARKER's mtime as now, creating it (and its directory) if needed."
  (make-directory (file-name-directory marker) t)
  (write-region "" nil marker nil 'silent))

(defun package-report--repo-dir (name)
  "Return the cache directory for archive package NAME's mirror clone."
  (expand-file-name (symbol-name name) package-report--repo-cache-dir))

(defun package-report--repo-stale-p (dir)
  "Return t if DIR's mirror clone is due for a `git fetch' refresh.
Also gates retries of a failed clone (see `package-report--ensure-repo'),
so a URL that isn't a git remote (some GNU ELPA :url point at an HTML
package page, not a repo) is retried on this same cadence instead of on
every report build."
  (package-report--marker-stale-p (expand-file-name ".package-report-fetched" dir)))

(defun package-report--touch-fetched (dir)
  "Record DIR's clone as just attempted, for `package-report--repo-stale-p'."
  (package-report--touch-marker (expand-file-name ".package-report-fetched" dir)))

(defun package-report--vc-fetch-marker (name)
  "Return the fetch-throttle marker path for vc package NAME.
Lives under `package-report--repo-cache-dir' rather than in NAME's own
checkout — see that constant's docstring for why."
  (expand-file-name (format "%s.vc-fetched" name) package-report--repo-cache-dir))

(defun package-report--lag-stats (base target &optional base-epoch)
  "Return how far BASE trails TARGET, as an alist without `tag'.
Both are revs resolvable in `default-directory' (a tag, a commit, or a
ref like \"HEAD\"/\"origin/HEAD\").  Alist has `commits' (count of
TARGET's commits BASE lacks), `days' and `dayshuman' (age of BASE's own
commit).  BASE-EPOCH, when given, is BASE's commit time (e.g. already
in hand from a `%(creatordate:unix)' lookup) — pass it to skip the `git
log' call that would otherwise re-fetch it.  Shared by
`package-report--tag-lag-in', `package-report--commit-lag-in' and
`package-report--head-lag-in', which differ only in what BASE/TARGET
are and how `tag' gets set."
  (let* ((commits (string-to-number
                    (car (package-report--git-lines "rev-list" "--count"
                                                     (concat base ".." target)))))
         (epoch (or base-epoch
                    (string-to-number
                     (car (package-report--git-lines "log" "-1" "--format=%ct" base)))))
         (days (floor (/ (- (float-time) epoch) 86400.0)))
         (dayshuman (if (zerop days) "today" (format-seconds "%Y %D%z" (* days 86400)))))
    (list (cons 'commits commits) (cons 'days days) (cons 'dayshuman dayshuman))))

(defun package-report--commit-lag-in (dir commit)
  "Return how far COMMIT trails DIR's HEAD.
COMMIT is the exact upstream commit an archive package's current build
was made from (its `:commit' extra) — a precise anchor, available even
when the repo carries no tags at all (unlike `package-report--tag-lag-in',
which has to guess from tag names).  `tag' is always :null: this is only
ever tried after `package-report--tag-lag-in' already found none, so
looking one up here again would be redundant.  Return nil if COMMIT
isn't an object DIR actually has."
  (let ((default-directory dir))
    (when (package-report--git-ok-p "cat-file" "-e" (concat commit "^{commit}"))
      (cons (cons 'tag :null) (package-report--lag-stats commit "HEAD")))))

(defun package-report--tag-lag-in (dir)
  "Return how far the latest tag in the git repo at DIR trails its HEAD.
Return an alist (tag . TAG) (commits . N) (days . N) (dayshuman . STR),
or nil when DIR carries no tags."
  (let ((default-directory dir))
    (when-let* ((line (car (package-report--git-lines
                             "for-each-ref" "refs/tags" "--sort=-creatordate"
                             "--format=%(refname:short) %(creatordate:unix)"
                             "--count=1")))
                (parts (split-string line " "))
                (tag (car parts))
                ((not (string-empty-p tag))))
      (cons (cons 'tag tag)
            (package-report--lag-stats tag "HEAD" (string-to-number (cadr parts)))))))

(defun package-report--head-lag-in (dir)
  "Return how far DIR's checked-out HEAD trails `origin/HEAD'.
Fallback for `package-report--tag-lag-in' when the repo carries no tags
at all — there is no release to measure against, but we can still see
how far the installed commit trails what is now on the remote's default
branch.  Same return shape, with `tag' bound to :null (so it serializes
to JSON `null', not `{}'); nil if `origin/HEAD' doesn't resolve (e.g.
`git remote set-head' was never run)."
  (let ((default-directory dir))
    (when (package-report--git-ok-p "rev-parse" "--verify" "-q" "origin/HEAD")
      (cons (cons 'tag :null) (package-report--lag-stats "HEAD" "origin/HEAD")))))

(defun package-report--vc-tag-lag (desc)
  "Return DESC's tag lag, computed from its existing vc checkout.
DESC is a `package-vc-p' descriptor.  Fetches from origin first (at most
once per `package-report--repo-refresh-interval', like the archive-clone
cache below — throttled via a marker in `package-report--repo-cache-dir',
never in the checkout itself), so the result reflects upstream, not just
what was checked out at install time.  Falls back to
`package-report--head-lag-in' when the repo has no tags.  See
`package-report--tag-lag-in' for the return shape."
  (when-let* ((dir (package-desc-dir desc))
              ((file-directory-p (expand-file-name ".git" dir))))
    (let ((marker (package-report--vc-fetch-marker (package-desc-name desc))))
      (when (package-report--marker-stale-p marker)
        (package-report--touch-marker marker)
        (let ((default-directory dir))
          (ignore-errors (call-process "git" nil nil nil "fetch" "--quiet" "--tags")))))
    (or (package-report--tag-lag-in dir)
        (package-report--head-lag-in dir))))

(defun package-report--repo-usable-p (dir)
  "Return t if DIR holds a valid (bare mirror) git repo."
  (file-exists-p (expand-file-name "HEAD" dir)))

(defconst package-report--elpa-monorepos
  (list (list "\\`https?://elpa\\.gnu\\.org/"
              "https://git.savannah.gnu.org/git/emacs/elpa.git" "externals/%s")
        (list "\\`https?://elpa\\.nongnu\\.org/"
              "https://git.savannah.gnu.org/git/emacs/nongnu.git" "elpa/%s"))
  "How to reach a package's real source when its archive :url is merely
a GNU/NonGNU ELPA description page, not a git remote — every such
package still lives on its own branch of the corresponding ELPA git
monorepo, in a well-known naming scheme.  Each entry is (URL-REGEXP
MONOREPO-URL BRANCH-FORMAT); BRANCH-FORMAT is formatted with the package
name to get that package's branch.  A third hosting scheme with a
different shape (not \"one monorepo, one branch per package\") would
need a new entry shape here, not just another regexp.")

(defun package-report--clone-target (name url)
  "Return (EFFECTIVE-URL . BRANCH) to clone for archive package NAME.
BRANCH is nil for an ordinary repo URL (mirror the whole thing).  For a
GNU/NonGNU ELPA description-page URL (see
`package-report--elpa-monorepos'), EFFECTIVE-URL is the shared ELPA
monorepo and BRANCH is NAME's own branch in it, so the clone can be
scoped to just that branch instead of mirroring every ELPA package."
  (or (seq-some (pcase-lambda (`(,rx ,repo ,branch-fmt))
                  (and (string-match-p rx url)
                       (cons repo (format branch-fmt name))))
                package-report--elpa-monorepos)
      (cons url nil)))

(defun package-report--ensure-repo (dir url branch)
  "Ensure DIR holds an up-to-date clone of URL (or its BRANCH).
When BRANCH is non-nil, the clone is scoped to just that branch (bare,
single-branch) instead of mirroring the whole repo — see
`package-report--clone-target'.  Return non-nil once DIR holds a usable
repo.  A failed clone (URL unreachable, bad branch, …) is cached the
same as a success — nil, retried only after
`package-report--repo-refresh-interval' — so a permanently bad URL
doesn't get re-cloned on every report build."
  (cond
   ((package-report--repo-usable-p dir)
    (or (not (package-report--repo-stale-p dir))
        (let ((default-directory dir))
          (package-report--touch-fetched dir)
          (= 0 (call-process "git" nil nil nil "fetch" "--quiet")))))
   ((and (file-directory-p dir) (not (package-report--repo-stale-p dir)))
    nil)
   (t
    ;; `git clone' refuses a non-empty destination, so the marker can only
    ;; be written after the attempt — but DIR must still exist afterwards
    ;; even on failure, or the next call would retry immediately instead
    ;; of waiting out the backoff.
    (when (file-directory-p dir) (delete-directory dir t))
    (make-directory (file-name-directory (directory-file-name dir)) t)
    (let ((ok (= 0 (apply #'call-process "git" nil nil nil "clone" "--quiet"
                          (append (if branch
                                      (list "--bare" "--single-branch" "--branch" branch)
                                    (list "--mirror"))
                                  (list "--filter=blob:none" url dir))))))
      (make-directory dir t)
      (package-report--touch-fetched dir)
      ok))))

(defun package-report--archive-tag-lag (name url commit)
  "Return archive package NAME's tag lag, via a cached clone of URL.
Prefers the latest-tag guess (`package-report--tag-lag-in') — that is
the actually interesting number (how far the last named RELEASE trails
main).  A commit-based lag would instead mostly measure how fresh the
archive's last build is, which for a continuously-rebuilt archive is
almost always ~0 — a different, far less interesting question.  COMMIT
(the package's `:commit' extra) is used only as a fallback, for the
rarer case of a repo with no tags at all (some GNU/NonGNU ELPA packages
have none) — see `package-report--commit-lag-in'.  See
`package-report--clone-target' for how URL maps to what actually gets
cloned; nil when URL is blank or the clone/fetch fails."
  (when (and url (not (string-empty-p url)))
    (let* ((dir (package-report--repo-dir name))
           (target (package-report--clone-target name url)))
      (when (package-report--ensure-repo dir (car target) (cdr target))
        (or (package-report--tag-lag-in dir)
            (and commit (package-report--commit-lag-in dir commit)))))))

;;; Data collection

(defun package-report-data ()
  "Return the package × archive matrix as a native Elisp structure.
A plist: (:cols ARCHIVE-NAMES :packages LIST-OF-ALISTS).  Each package
alist is symbol-keyed, ready for `json-serialize': name, kind, installed,
pick, url, pin, outdated, ver, taglag.
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
                    (vc-p (package-vc-p desc))
                    (descs (cdr (assq name package-archive-contents)))
                    ;; archive-contents descs are sorted by (priority . version):
                    ;; the first one is what package.el would actually install.
                    (pick (car descs))
                    (url (or (alist-get :url (and pick (package-desc-extras pick)))
                             (alist-get :url (package-desc-extras desc))
                             ""))
                    (pin (or (alist-get name package-pinned-packages) ""))
                    (commit (and pick (alist-get :commit (package-desc-extras pick))))
                    (taglag (if vc-p
                                (package-report--vc-tag-lag desc)
                              (package-report--archive-tag-lag name url commit)))
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
                 (kind . ,(if vc-p "vc" "archive"))
                 (installed . ,(package-version-join (package-desc-version desc)))
                 (pick . ,(if pick (package-desc-archive pick) ""))
                 (url . ,url)
                 (pin . ,pin)
                 ;; outdated: package.el has a newer archive version available.
                 (outdated . ,(if (memq name upgradeable) t :false))
                 ;; empty ver/taglag (nil) serializes as {}
                 (ver . ,ver)
                 (taglag . ,taglag))))
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
                     (ver . ())
                     (taglag . ())))))
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
`template.html'.  Reports on the live session's archive data as it is.
Return a plist (:summary TEXT)."
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
    (list :summary (package-report--summary packages))))

;;;###autoload
(defun package-report ()
  "Build the package × archive report and show it in a browser.
Refreshes archive contents first so the report reflects the latest data.
Uses `xwidget-webkit' when available, otherwise the system browser."
  (interactive)
  (package-refresh-contents)
  (package-report--build)
  (let ((browse-url-browser-function
         (if (and (display-graphic-p) (featurep 'xwidget-internal))
             #'xwidget-webkit-browse-url
           browse-url-browser-function)))
    (browse-url-of-file package-report--file)))

(provide 'package-report)
;;; package-report.el ends here

;;; init.el --- Initialization -*- lexical-binding: t; no-byte-compile: t -*-

(setq user-full-name "Ruslan Kamashev"
      user-login-name "rynffoll"
      user-mail-address "rynffoll@gmail.com")

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-hook-name-suffix nil)
(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)

(use-package package
  :ensure nil
  :init
  (setq package-review-policy t)
  (setq package-retention-policy t)
  :config
  (add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize))

(use-package gnu-elpa-keyring-update)

(defgroup +feature-flags nil
  "Feature flags for this configuration."
  :group 'convenience)

(defcustom +with-evil t
  "Enable Evil integration."
  :type 'boolean
  :group '+feature-flags)

(defcustom +with-icons t
  "Enable icon integrations."
  :type 'boolean
  :group '+feature-flags)

(use-package emacs
  :ensure nil
  :init
  (setq confirm-kill-emacs 'y-or-n-p))

(use-package compile
  :ensure nil
  :init
  (setq compilation-scroll-output 'first-error))

(use-package ansi-color
  :disabled ;; replaced by ghostel
  :ensure nil
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))

(use-package time
  :ensure nil
  :init
  (setq zoneinfo-style-world-list
        '(("America/Mexico_City" "🇲🇽 Mexico City")
          ("America/Bagota"      "🇨🇴 Bogota")
          ("UTC"                 "🕒 UTC")
          ("Europe/Madrid"       "🇪🇸 Barcelona")
          ("Europe/Moscow"       "🇷🇺 Moscow")
          ("Asia/Nicosia"        "🇨🇾 Cyprus")
          ("Asia/Yerevan"        "🇦🇲 Yerevan")
          ("Asia/Almaty"         "🇰🇿 Almaty")))
  (setq world-clock-time-format "%z %R	%a %d %b (%Z)")
  (setq world-clock-sort-order "%FT%T"))

(use-package calendar
  :ensure nil
  :init
  (setq calendar-date-style 'iso)
  (setq calendar-week-start-day 1))

(use-package gcmh
  :unless (fboundp 'igc-info) ;; Emacs 32?
  :init
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold (* 64 1024 1024)) ;; 64mb
  :hook
  (emacs-startup-hook . gcmh-mode))

(use-package async
  :hook
  (after-init-hook . async-bytecomp-package-mode)
  (dired-mode-hook . dired-async-mode))

(use-package server
  :ensure nil
  :hook
  (after-init-hook . server-start))

(use-package repeat
  :ensure nil
  :hook
  (after-init-hook . repeat-mode))

(use-package which-key
  :ensure nil
  :init
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-dont-use-unicode nil)
  :hook
  (after-init-hook . which-key-mode))

(use-package evil
  :if +with-evil
  :demand
  :preface
  (defun +save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-buffer))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-motion-state-cursor 'box)  ;; █
  (setq evil-visual-state-cursor 'box)  ;; █
  (setq evil-normal-state-cursor 'box)  ;; █
  (setq evil-insert-state-cursor 'bar)  ;; ⎸
  (setq evil-emacs-state-cursor  'hbar) ;; _
  (setq evil-symbol-word-search t)
  ;; (setq evil-move-beyond-eol nil)
  ;; (setq evil-move-cursor-back t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-ex-define-cmd "q"  'kill-current-buffer)
  (evil-ex-define-cmd "wq" '+save-and-kill-buffer)
  (evil-mode t))

(use-package evil-collection
  :if +with-evil
  :demand
  :after evil
  :init
  (setq evil-collection-repl-submit-state 'insert)
  (setq evil-collection-magit-want-horizontal-movement t)
  :config
  (evil-collection-init)
  ;; Emacs 31 changed `define-globalized-minor-mode' timing, causing
  ;; `evil-normalize-keymaps' to run before `evil-collection-unimpaired-mode'
  ;; activates.  Re-normalize when the per-buffer mode turns on.
  (add-hook 'evil-collection-unimpaired-mode-hook #'evil-normalize-keymaps)
  ;; add diff-hl hunk navigation to unimpaired
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
    "[d" 'diff-hl-previous-hunk
    "]d" 'diff-hl-next-hunk)
  ;; add tab-bar navigation to unimpaired
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
    (kbd "[ TAB") 'tab-previous
    (kbd "] TAB") 'tab-next))

(use-package evil-commentary
  :if +with-evil
  :hook
  (after-init-hook . evil-commentary-mode))

(use-package evil-surround
  :if +with-evil
  :hook
  (after-init-hook . global-evil-surround-mode))

(use-package evil-org
  :if +with-evil
  :init
  (setq evil-org-key-theme '(todo textobjects insert navigation heading))
  :hook
  (org-mode-hook . evil-org-mode))

(use-package evil-org-agenda
  :if +with-evil
  :demand
  :ensure evil-org
  :after org-agenda
  :config
  (evil-org-agenda-set-keys))

(use-package evil-mc
  :if +with-evil
  :hook
  (after-init-hook . global-evil-mc-mode))

(use-package evil-terminal-cursor-changer
  :if +with-evil
  :unless (display-graphic-p)
  :init
  (setq etcc-use-color t)
  (setq etcc-use-blink nil)
  :hook
  (after-init-hook . evil-terminal-cursor-changer-activate))

(use-package general
  :config
  (general-create-definer +leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (general-create-definer +local-leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-SPC m")
  (+leader-def
    ""    '(nil :wk "leader")

    ":"   'execute-extended-command
    "."   'find-file
    ","   'switch-to-buffer

    "0"   'dired-side-jump
    "1"   'winum-select-window-1
    "2"   'winum-select-window-2
    "3"   'winum-select-window-3
    "4"   'winum-select-window-4
    "5"   'winum-select-window-5
    "6"   'winum-select-window-6
    "7"   'winum-select-window-7
    "8"   'winum-select-window-8
    "9"   'winum-select-window-9

    "b"   '(:ignore t :wk "buffer")
    "bb"  'switch-to-buffer
    "bk"  'kill-current-buffer
    "bK"  'kill-buffer-and-window
    "bn"  'evil-buffer-new
    "br"  'revert-buffer
    "bR"  'rename-buffer
    "bs"  'scratch-buffer
    "bl"  'list-buffers
    "bi"  'ibuffer

    "c"   '(:ignore t :wk "code")
    "cr"  'quickrun
    "cf"  'apheleia-format-buffer

    "e"   '(:ignore t :wk "emacs")
    "ed"  'iqa-find-user-init-directory
    "ee"  'iqa-find-user-init-file
    "ec"  'iqa-find-user-custom-file
    "er"  'iqa-reload-user-init-file
    "eR"  'restart-emacs

    "f"   '(:ignore t :wk "file")
    "fl"  'find-library
    "fr"  'recentf-open-files
    "ft"  'dired-side-toggle
    "ff"  'dired-side-follow-file
    "fR"  'crux-rename-file-and-buffer
    "fD"  'crux-delete-file-and-buffer
    "fg"  'magit-file-dispatch

    "F"   '(:ignore t :wk "frame")
    "Ff"  'select-frame-by-name
    "Fn"  'make-frame-command
    "Fc"  'delete-frame
    "FC"  'delete-other-frames
    "Fo"  'other-frame
    "Fb"  'switch-to-buffer-other-frame
    "FM"  'toggle-frame-maximized
    "FF"  'toggle-frame-fullscreen
    "F["  'ns-prev-frame
    "F]"  'ns-next-frame

    "g"   '(:ignore t :wk "git")
    "g."  'magit-dispatch
    "gf"  'magit-file-dispatch
    "gg"  'magit-status
    "gL"  'git-link-dispatch
    "gj"  'consult-git-log-grep
    "gt"  'git-timemachine

    "h"   '(:keymap help-map :package help :wk "help")

    "i"   '(:ignore t :wk "insert")
    "it"  'tempel-insert

    "j"   '(:ignore t :wk "jump") ;; TODO: goto-map (M-g)
    "ji"  'imenu
    "jc"  'avy-goto-char
    "jw"  'avy-goto-word-0
    "jW"  'avy-goto-word-1
    "jl"  'avy-goto-line
    "jL"  'avy-goto-end-of-line

    "l"   '(:ignore t :wk "llm")
    "lg"  'gptel
    "lc"  'claude-code-ide-menu
    "la"  'agent-shell

    "o"   '(:ignore t :wk "open")
    "oc"  'customize-group
    "ol"  'link-hint-open-link
    "ot"  'ghostel
    "oa"  'org-agenda
    "ox"  'org-capture

    "p"   '(:keymap project-prefix-map :package project :wk "project")

    "s"   '(:ignore t :wk "search") ;; TODO: search-map (M-s)
    "sb"  'consult-line
    "sg"  'consult-ripgrep
    "st"  'consult-todo

    "S"   '(:ignore t :wk "session")
    "Ss"  'desktop-save-in-desktop-dir
    "Sr"  'desktop-read

    "TAB" '(:keymap tab-prefix-map :wk "tab-bar")

    "t"   '(:ignore t :wk "toggle")
    "tc"  'colorful-mode
    "tf"  'focus-mode
    "ti"  'highlight-indent-guides-mode
    "tl"  'hl-line-mode
    "tL"  'global-hl-line-mode
    "tn"  'display-line-numbers-mode
    "ts"  'jinx-mode
    "tt"  'load-theme
    "tT"  'toggle-truncate-lines
    "tw"  'whitespace-mode
    "tz"  'olivetti-mode
    "tb"  'breadcrumb-mode
    "tM"  'mode-line-invisible-mode
    )
  (+local-leader-def
    ""    '(nil :wk "local leader")))

(use-package mule
  :ensure nil
  :init
  (setq default-input-method 'russian-computer))

(use-package char-fold
  :ensure nil
  :init
  (setq char-fold-symmetric t)
  (setq search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :general
  (evil-normal-state-map "C-х" 'evil-force-normal-state)
  (evil-insert-state-map "C-х" 'evil-normal-state)
  (evil-visual-state-map "C-х" 'evil-exit-visual-state)
  :init
  (setq reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  (setq reverse-im-char-fold t)
  (setq reverse-im-read-char-advice-function #'reverse-im-read-char-exclude)
  (setq reverse-im-input-methods '("russian-computer"))
  :hook
  (after-init-hook . reverse-im-mode))

(use-package xt-mouse
  :unless (display-graphic-p)
  :ensure nil
  :hook
  (after-init-hook . xterm-mouse-mode))

;; Better window divider in terminal: | -> │
;; https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
(unless (display-graphic-p)
  (with-eval-after-load 'disp-table
    (defun +update-window-divider ()
      (let ((display-table (or buffer-display-table
                               standard-display-table))
            (divider (make-glyph-code ?│)))
        (set-display-table-slot display-table 'vertical-border divider)))
    (add-hook 'window-configuration-change-hook #'+update-window-divider)))

(use-package system-taskbar
  :ensure nil
  :hook
  (after-init-hook . system-taskbar-mode))

(use-package emacs
  :ensure nil
  :init
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 101) ;; scroll one line at a time, no recenter
  (setq scroll-margin 4) ;; lines of context
  (setq fast-but-imprecise-scrolling t)
  (setq redisplay-skip-fontification-on-input t))

(use-package ultra-scroll
  :if (display-graphic-p)
  :init
  (setq scroll-margin 0) ;; important: scroll-margin>0 not yet supported
  :hook
  (after-init-hook . ultra-scroll-mode))

(use-package ligature
  :if (display-graphic-p)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
  :hook
  (after-init-hook . global-ligature-mode))

(use-package nerd-icons
  :if +with-icons
  :config
  (when (and (display-graphic-p)
             (not (member "Symbols Nerd Font Mono" (font-family-list))))
    (nerd-icons-install-fonts)))

(use-package bindings
  :ensure nil
  :init
  ;; (setq mode-line-right-align-edge 'right-fringe)
  (setq mode-line-collapse-minor-modes t)
  (setq mode-line-percent-position nil) ;; move percent to `mode-line-position-column-line-format'
  ;; (setq mode-line-position-column-line-format '("%l:%c:%p"))
  ;; (setq mode-line-position-column-line-format '("%l:%c:" (-3 "%p")))
  (setq mode-line-position-column-line-format '("%l:%c"))
  ;; (setq mode-line-compact t) ;; breaks right-align spacing
  (setq mode-line-modes-delimiters nil)
  )

(use-package doom-modeline
  :init
  (setq doom-modeline-icon +with-icons)
  (setq doom-modeline-modal-icon +with-icons)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-check 'simple)
  (setq doom-modeline-unicode-number nil)
  (setq doom-modeline-workspace-name nil)
  ;; :hook
  ;; (after-init-hook . doom-modeline-mode)
  )

(use-package modeline-x
  :ensure nil
  :demand t
  :init
  (setq-default mode-line-format
                '("%e"
                  " "
                  ;; (winum-mode modeline-x-winum)
                  (winum-mode modeline-x-winum-icon)
                  " "
                  ;; (evil-mode modeline-x-evil-state-tag)
                  (evil-mode modeline-x-evil-state-icon)
                  " "
                  modeline-x-buffer-identification
                  " "
                  modeline-x-position
                  " "
                  mode-line-format-right-align
                  modeline-x-misc-info
                  (flymake-mode modeline-x-flymake)
                  " "
                  (vc-mode modeline-x-vc)
                  " "
                  modeline-x-major-mode-icon
                  " "
                  mode-line-modes))
  :config
  (modeline-x-reset))

(use-package emacs
  :ensure nil
  :preface
  (defun +format-input-method ()
    (when-let* ((im (cond
                     (current-input-method
                      current-input-method-title)
                     ((and (bound-and-true-p evil-local-mode)
                           (bound-and-true-p evil-input-method))
                      ;; (INPUT-METHOD LANGUAGE-ENV ACTIVATE-FUNC TITLE DESCRIPTION ARGS...)
                      ;; (0            1            2             3     4           5   ...)
                      (nth 3 (assoc evil-input-method input-method-alist))))))
      (concat " " (propertize im 'face 'bold) " ")))
  :config
  (add-to-list 'global-mode-string '(:eval (+format-input-method))))

(use-package breadcrumb
  :hook
  (after-init-hook . breadcrumb-mode))

(use-package spacious-padding
  :if (display-graphic-p))

(defun +custom-faces (&rest _)
  "Reapply custom face attributes after theme load."
  (custom-set-faces
   `(mode-line
     ((t (:height 0.9 :box (:line-width 3 :style nil :color ,(face-attribute 'mode-line :background nil 'default))))))
   `(mode-line-active
     ((t (:height 0.9 :box (:line-width 3 :style nil :color ,(face-attribute 'mode-line-active :background nil 'default))))))
   `(mode-line-inactive
     ((t (:height 0.9 :box (:line-width 3 :style nil :color ,(face-attribute 'mode-line-inactive :background nil 'default))))))
   ;; do not change header-line height because it breaks alignment in proced, profile-report, etc.
   ;; '(header-line                    ((t (:height 0.9))))
   ;; '(header-line-inactive           ((t (:height 0.9))))
   '(evil-ex-substitute-matches     ((t (:inherit diff-removed :foreground unspecified :background unspecified :strike-through t))))
   '(evil-ex-substitute-replacement ((t (:inherit diff-added   :foreground unspecified :background unspecified :underline nil))))
   '(diredfl-dir-name               ((t (:bold t))))
   '(vundo-highlight                ((t (:inherit success :foreground unspecified))))
   '(vundo-last-saved               ((t (:inherit error   :foreground unspecified))))
   '(vundo-saved                    ((t (:inherit warning :foreground unspecified))))
   '(eros-result-overlay-face       ((t (:inherit shadow :box t))))
   '(cider-result-overlay-face      ((t (:inherit shadow :box t))))
   '(org-tag                        ((t (:inherit shadow :foreground unspecified :background unspecified :bold nil))))
   ;; '(org-ellipsis                   ((t (:underline nil))))
   ;; '(org-block-begin-line           ((t (:underline nil))))
   ;; '(org-block-end-line             ((t (:overline nil))))
   '(dape-breakpoint-face           ((t (:inherit error))))
   '(hs-ellipsis                    ((t (:inherit shadow))))
   ))

(add-hook 'enable-theme-functions #'+custom-faces)

(use-package modus-themes
  ;; :ensure nil
  :pin melpa-stable
  :init
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
        '(;; (bg-region bg-cyan-intense)
          ;; (cursor fg-main)
          (fg-region unspecified)
          (bg-prose-block-delimiter bg-inactive)
          (fg-prose-block-delimiter fg-dim)
          (bg-prose-block-contents bg-dim)
          (fringe unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          )))

(use-package modus-catppuccin
  :vc (:url "https://gitlab.com/magus/modus-catppuccin.git" :rev :newest))

(use-package ef-themes)

(use-package doric-themes)

(use-package doom-themes
  :init
  (setq doom-themes-enable-italic t)
  :config
  (doom-themes-org-config))

(setq +theme-alist '((default . modus-operandi)
                     (light   . modus-operandi)
                     (dark    . ef-dream)))

(defun +theme-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (when-let* ((theme (alist-get appearance +theme-alist)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme :no-confirm)))

(defun +theme-toggle ()
  "Toggle between light and dark themes."
  (interactive)
  (if (memq (alist-get 'dark +theme-alist) custom-enabled-themes)
      (+theme-change 'light)
    (+theme-change 'dark)))

(if (and (display-graphic-p)
         (eq window-system 'ns)
         (boundp 'ns-system-appearance-change-functions))
    (add-hook 'ns-system-appearance-change-functions #'+theme-change)
  (load-theme (alist-get 'default +theme-alist) :no-confirm))

(use-package frame
  :ensure nil
  :preface
  (defun +reset-frame-parameters ()
    "Reset frame parameters to default values."
    (interactive)
    (modify-frame-parameters
     nil
     (seq-filter (lambda (param)
                   (memq (car param) '(left top width height)))
                 default-frame-alist)))
  :config
  (blink-cursor-mode -1))

(use-package fringe
  :if (display-graphic-p)
  :ensure nil
  :init
  (setf (cdr (assq 'continuation fringe-indicator-alist))
        '(nil nil) ;; no continuation indicators
        ;; '(nil right-curly-arrow) ;; right indicator only
        ;; '(left-curly-arrow nil) ;; left indicator only
        ;; '(left-curly-arrow right-curly-arrow) ;; default
        ))

(use-package tab-bar
  :ensure nil
  :general
  (tab-prefix-map
   "TAB" 'tab-recent
   "0" 'tab-recent
   "1" 'tab-bar-select-tab
   "2" 'tab-bar-select-tab
   "3" 'tab-bar-select-tab
   "4" 'tab-bar-select-tab
   "5" 'tab-bar-select-tab
   "6" 'tab-bar-select-tab
   "7" 'tab-bar-select-tab
   "8" 'tab-bar-select-tab
   "9" 'tab-last
   "." 'tab-switch
   "n" 'tab-new
   "[" 'tab-previous
   "]" 'tab-next
   ">" 'tab-move
   "<" 'tab-bar-move-tab-backward
   "c" 'tab-close
   "C" 'tab-close-other)
  (evil-window-map
   "u" 'tab-bar-history-back
   "U" 'tab-bar-history-forward)
  :init
  (setq tab-bar-format '(tab-bar-separator
                         tab-bar-format-menu-bar
                         tab-bar-format-tabs-groups
                         tab-bar-format-align-right
                         tab-bar-format-global))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-tab-hints t)
  (setq tab-bar-separator " ")
  (setq tab-bar-auto-width nil)
  :config
  (add-to-list 'tab-bar-tab-name-format-functions
               #'tab-bar-tab-name-format-truncated)
  :hook
  (after-init-hook . tab-bar-mode)
  (after-init-hook . tab-bar-history-mode))

(use-package project-tab-groups
  :hook
  (after-init-hook . project-tab-groups-mode))

(use-package tab-bar-theme
  :ensure nil
  :init
  (setq tab-bar-theme-height 0.9)
  :hook
  (after-init-hook . tab-bar-theme-mode))

(use-package per-tab-group-theme
  :disabled
  :ensure nil
  :hook
  (after-init-hook . per-tab-group-theme-mode))

(use-package tab-line
  :ensure nil
  :init
  (setq tab-line-close-button-show nil)
  (setq tab-line-new-button-show nil)
  (setq tab-line-tab-name-function #'tab-line-tab-name-truncated-buffer)
  (setq tab-line-tabs-buffer-group-function #'tab-line-tabs-buffer-group-by-project)
  (setq tab-line-separator " ")
  ;; :hook
  ;; (after-init-hook . global-tab-line-mode)
  )

(use-package tab-line-theme
  :ensure nil
  :init
  (setq tab-line-theme-height 0.9)
  (setq tab-line-theme-tab-name-padding "")
  :hook
  (after-init-hook . tab-line-theme-mode))

(use-package tab-line-nerd-icons
  :if +with-icons
  :hook
  (after-init-hook . tab-line-nerd-icons-global-mode))

(use-package window
  :ensure nil
  :general
  (evil-window-map
   "m" 'maximize-window
   "M" 'minimize-window))

(use-package winum
  :init
  (setq winum-scope 'frame-local)
  (setq winum-auto-setup-mode-line nil)
  :hook
  (after-init-hook . winum-mode))

(use-package zoom
  :general
  (evil-window-map
   "z" 'zoom-mode)
  :init
  (setq zoom-size '(0.618 . 0.618)) ;; golden ratio
  (setq zoom-ignored-major-modes '(vundo-mode vundo-diff-mode))
  (setq zoom-ignored-buffer-names '("COMMIT_EDITMSG" " *vundo tree*"))
  (setq zoom-ignored-buffer-name-regexps '("^magit.*" "^\\*dape.*")))

(use-package shackle
  :init
  (setq shackle-default-size 0.4)
  (setq shackle-rules
        '((help-mode :align below :select t)
          (helpful-mode :align below)
          (cider-repl-mode :align below)
          (ansible-doc-module-mode :align below)
          ("\\*Async Shell Command\\*.*" :regexp t :ignore t)
          (Man-mode :align below :select t)
          ("\\*Man.*\\*" :regexp t :align below :select t)
          ;; ("*Warnings*" :align below)
          ("*Compile-Log*" :align below)
          (compilation-mode :align below)
          ("\\*vc-git :.*" :regexp t :align below :ignore t :select t)
          ("\\*docker-compose .*\\*" :regexp t :align below)
          (comint-mode :align below)
          ("*compilation*" :align below)
          (ghostel-compile-view-mode :align below)))
  :hook
  (after-init-hook . shackle-mode))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package buff-menu ;; see list-buffers
  :ensure nil
  :init
  (setq Buffer-menu-group-by '(Buffer-menu-group-by-root))
  (setq Buffer-menu-group-sort-by 'Buffer-menu-group-sort-alphabetically)
  (setq Buffer-menu-human-readable-sizes t))

(use-package ibuffer
  :ensure nil
  :general
  ([remap list-buffers] 'ibuffer)
  :init
  (setq ibuffer-human-readable-size t))

(use-package nerd-icons-ibuffer
  :if +with-icons
  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package persistent-scratch
  :init
  (setq persistent-scratch-backup-file-name-format "%Y-%m-%d")
  (setq persistent-scratch-backup-directory
        (expand-file-name "persistent-scratch" user-emacs-directory))
  :hook
  (after-init-hook . persistent-scratch-setup-default))

(use-package savehist
  :ensure nil
  :hook
  (after-init-hook . savehist-mode))

(use-package saveplace
  :ensure nil
  :hook
  (after-init-hook . save-place-mode))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 300)
  :hook
  (after-init-hook . recentf-mode))

(use-package desktop
  :ensure nil
  :init
  (setq desktop-path `(,user-emacs-directory))
  :config
  (dolist (mode '(git-commit-mode))
    (add-to-list 'desktop-modes-not-to-save mode)))

(use-package emacs
  :ensure nil
  :init
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq enable-recursive-minibuffers t)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (setq text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

(use-package mouse
  :ensure nil
  :if (display-graphic-p)
  :hook
  (after-init-hook . context-menu-mode))

(use-package nerd-icons-completion
  :if +with-icons
  :hook
  (vertico-mode-hook    . nerd-icons-completion-mode)
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package consult
  :general
  ([remap bookmark-jump]                 'consult-bookmark)
  ([remap goto-line]                     'consult-goto-line)
  ([remap imenu]                         'consult-imenu)
  ([remap locate]                        'consult-locate)
  ([remap load-theme]                    'consult-theme)
  ([remap man]                           'consult-man)
  ([remap recentf-open-files]            'consult-recent-file)
  ([remap switch-to-buffer]              'consult-buffer)
  ([remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  'consult-buffer-other-frame)
  ([remap yank-pop]                      'consult-yank-pop)
  ([remap project-find-regexp]           'consult-ripgrep)
  :init
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :hook
  (completion-list-mode-hook . consult-preview-at-point-mode))

(use-package consult-xref
  :ensure consult
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(use-package nerd-icons-xref
  :if +with-icons
  :hook
  (after-init-hook . nerd-icons-xref-mode))

(use-package consult-dir
  :general
  ([remap list-directory] 'consult-dir))

(use-package marginalia
  :general
  (minibuffer-local-map
   "M-A" 'marginalia-cycle)
  :hook
  (after-init-hook . marginalia-mode))

(use-package vertico
  :general
  (vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous)
  :init
  (setq vertico-cycle t)
  :hook
  (after-init-hook . vertico-mode))

(use-package vertico-directory
  :ensure vertico
  :general
  (vertico-map
   "DEL" 'vertico-directory-delete-char)
  :hook
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  (setq orderless-matching-styles '(orderless-literal
                                    ;; orderless-flex
                                    orderless-prefixes
                                    orderless-regexp))
  (setq completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :general
  ("M-S-SPC" 'completion-at-point)
  :init
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-min-width 40)
  :hook
  (after-init-hook . global-corfu-mode))

(use-package corfu-echo
  :ensure corfu
  :hook
  (corfu-mode-hook . corfu-echo-mode))

(use-package corfu-info
  :ensure corfu
  :unless (display-graphic-p)
  :after corfu
  :general
  (corfu-map
   "C-h" 'corfu-info-documentation))

(use-package corfu-popupinfo
  :ensure corfu
  :if (display-graphic-p)
  :general
  (corfu-map
   "C-h" 'corfu-popupinfo-documentation)
  :init
  (setq corfu-popupinfo-delay nil)
  :hook
  (corfu-mode-hook . corfu-popupinfo-mode))

(use-package corfu-history
  :ensure corfu
  :hook
  (corfu-mode-hook . corfu-history-mode))

(use-package kind-icon
  :unless +with-icons
  :after corfu
  :demand
  :preface
  (defun +kind-icon-reset-cache (theme)
    (call-interactively 'kind-icon-reset-cache))
  :init
  (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-blend-background t)
  (setq kind-icon-use-icons nil)
  (setq kind-icon-extra-space nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (advice-add #'disable-theme :before #'+kind-icon-reset-cache))

(use-package nerd-icons-corfu
  :if +with-icons
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :general
  ("C-c p" 'cape-prefix-map)
  :hook
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file)
  (completion-at-point-functions . cape-elisp-block))

(use-package embark
  :general
  ("C-;" #'embark-act)
  (help-map
   "B" #'embark-bindings)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package tempel
  :preface
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  :general
  (tempel-map
   "TAB" 'tempel-next)
  :hook
  (conf-mode-hook . tempel-setup-capf)
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (after-init-hook . global-tempel-abbrev-mode))

(use-package tempel-collection)

(use-package files
  :ensure nil
  :init
  (setq require-final-newline t)
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(use-package autorevert
  :ensure nil
  :init
  (setq auto-revert-avoid-polling t)
  (setq global-auto-revert-non-file-buffers t)
  :hook
  (after-init-hook . global-auto-revert-mode))

(use-package iqa
  :preface
  ;; for integration with project-tab-groups
  (defun +iqa-find-file-project (file)
    (let* ((dir (file-name-directory file))
           (default-directory dir))
      (project-current t)
      (find-file file)))
  :init
  (setq iqa-find-file-function #'+iqa-find-file-project)
  (setq iqa-user-init-file (locate-user-emacs-file "config.org")))

(use-package epg-config
  :ensure nil
  :init
  (setq epg-pinentry-mode 'loopback))

(use-package project
  :ensure nil
  :general
  (project-prefix-map
   "m" 'magit-project-status
   "b" 'consult-project-buffer)
  :init
  (setq project-buffers-viewer 'project-list-buffers-ibuffer)
  (setq project-kill-buffers-display-buffer-list t)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (magit-project-status "Magit")))
  (setq project-vc-extra-root-markers '(".project")))

(use-package disproject
  :general
  (project-prefix-map
   "." 'disproject-dispatch))

(use-package projection
  :general
  (project-prefix-map
   "P" '(:keymap projection-map :package projection-map :wk "projection"))
  :config
  (put 'projection-commands-configure-project 'safe-local-variable #'stringp)
  (put 'projection-commands-build-project     'safe-local-variable #'stringp)
  (put 'projection-commands-test-project      'safe-local-variable #'stringp)
  (put 'projection-commands-run-project       'safe-local-variable #'stringp)
  (put 'projection-commands-package-project   'safe-local-variable #'stringp)
  (put 'projection-commands-install-project   'safe-local-variable #'stringp)
  :hook
  (after-init-hook . global-projection-hook-mode))

(use-package projection-multi
  :general
  (project-prefix-map
   "RET" 'projection-multi-compile))

(use-package projection-multi-embark
  :after embark
  :after projection-multi
  :demand t
  :config
  (projection-multi-embark-setup-command-map))

(use-package project-x
  :ensure nil
  :demand t
  :general
  (project-prefix-map
   "l" #'project-x-open-layout)
  :init
  (setq project-x-layout '(dired-side-toggle magit-project-status))
  :config
  (add-to-list 'project-switch-commands '(project-x-open-layout "Layout") t))

(use-package dired
  :ensure nil
  :init
  ;; -l     use a long listing format
  ;;
  ;; -A, --almost-all
  ;;        do not list implied . and ..
  ;;
  ;; -G, --no-group
  ;;        in a long listing, don't print group names
  ;;
  ;; -F, --classify[=WHEN]
  ;;        append indicator (one of */=>@|) to entries WHEN
  ;;
  ;; -h, --human-readable
  ;;        with -l and -s, print sizes like 1K 234M 2G etc.
  ;;
  ;; --sort=WORD
  ;;        change default 'name' sort to WORD: none (-U), size (-S), time
  ;;        (-t), version (-v), extension (-X), name, width
  ;;
  ;; --group-directories-first
  ;;        group directories before files
  ;;
  ;; --time-style=TIME_STYLE
  ;;        time/date format with -l; see TIME_STYLE below
  ;;
  (setq dired-listing-switches "-lAGFhv --group-directories-first --time-style=long-iso")
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  (setq dired-free-space nil)
  (setq dired-hide-details-hide-absolute-location t)
  (setq dired-create-destination-dirs-on-trailing-dirsep t)
  :config
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))
  :hook
  (dired-mode-hook . dired-hide-details-mode))

(use-package dired-aux
  :ensure nil
  :init
  (setq dired-vc-rename-file t)
  (setq dired-create-destination-dirs 'ask))

(use-package dired-x
  :ensure nil
  :after dired
  :defer nil
  :general
  ( :keymaps 'dired-mode-map :states 'normal
    "M-." 'dired-omit-mode)
  :init
  (setq dired-omit-extensions nil)
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

(use-package dired-subtree
  :demand t
  :after dired
  :general
  (dired-mode-map
   "<backtab>" 'dired-subtree-cycle)
  :init
  (setq dired-subtree-use-backgrounds nil)
  ;; Tabs scale with `text-scale-adjust', plain spaces don't.
  ;; Avoid artifacts when `dired-subtree' is used together with `nerd-icons-dired' and scaled fonts.
  (when +with-icons
    (setq dired-subtree-line-prefix "\t\t")))

(use-package diredfl
  :hook
  (dired-mode-hook . diredfl-mode))

(use-package nerd-icons-dired
  :if +with-icons
  :hook
  (dired-mode-hook . nerd-icons-dired-mode)
  (dired-subtree-after-insert-hook . nerd-icons-dired--refresh))

(use-package nerd-icons-multimodal
  :disabled ;; conflicts with dired-sidebar
  :if +with-icons
  :vc (:url "https://github.com/abougouffa/nerd-icons-multimodal" :rev :newest)
  :hook
  (dired-mode-hook   . nerd-icons-multimodal-mode)
  (archive-mode-hook . nerd-icons-multimodal-mode)
  (tar-mode-hook     . nerd-icons-multimodal-mode))

(use-package dired-git-info
  :general
  ( :keymaps 'dired-mode-map :states 'normal
    ")" 'dired-git-info-mode)
  :init
  (setq dgi-auto-hide-details-p nil))

(use-package dired-side
  :ensure nil
  :preface
  (defun +dired-side-custom-face ()
    (face-remap-add-relative 'default :height 0.9))
  (defun +dired-side-point-at-file (file)
    "Reveal FILE in sidebar by expanding subtrees along the path."
    (when (and file (fboundp 'dired-subtree--is-expanded-p))
      ;; HACK: suppress diff-hl per-insert updates to avoid concurrent
      ;; git processes (index.lock conflict); call diff-hl-dired-update
      ;; once at the end instead
      (goto-char (point-min))
      (let* ((dired-subtree-after-insert-hook
              (remq '+diff-hl-dired-update dired-subtree-after-insert-hook))
             (root (dired-current-directory))
             (relative (file-relative-name file root))
             (parts (split-string relative "/" t))
             (path root))
        (dolist (part parts)
          (setq path (file-name-concat path part))
          (when (dired-goto-file-1 part (expand-file-name path) nil)
            (let ((pos (point)))
              (when (and (file-directory-p path)
                         (not (dired-subtree--is-expanded-p)))
                (dired-subtree-cycle))
              (setq path (file-name-as-directory path))
              (goto-char pos)))))
      (when (bound-and-true-p diff-hl-dired-mode)
        (diff-hl-dired-update))
      (when (bound-and-true-p nerd-icons-dired-mode)
        (nerd-icons-dired--refresh))))
  :init
  (setq dired-side-follow-file-function #'+dired-side-point-at-file)
  :config
  (with-eval-after-load 'winum
    (defun +winum-assign-0-to-dired-side ()
      (when (and (bound-and-true-p dired-side-mode)
                 (window-parameter (selected-window) 'window-side))
        0))
    (add-to-list 'winum-assign-functions #'+winum-assign-0-to-dired-side))
  :hook
  (dired-side-mode-hook . dired-hide-details-mode)
  (dired-side-mode-hook . mode-line-invisible-mode)
  (dired-side-mode-hook . +dired-side-custom-face))

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x)) (daemonp))
  :demand
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package with-editor
  :general
  ([remap shell-command]       'with-editor-shell-command)
  ([remap async-shell-command] 'with-editor-async-shell-command)
  :hook
  (shell-mode-hook   . with-editor-export-editor)
  (term-exec-hook    . with-editor-export-editor)
  (eshell-mode-hook  . with-editor-export-editor))

(use-package help
  :ensure nil
  :general
  (help-map
   "F" 'describe-face))

(use-package helpful
  :general
  ([remap describe-command]             'helpful-command)
  ([remap describe-key]                 'helpful-key)
  ([remap describe-variable]            'helpful-variable)
  ([remap describe-function]            'helpful-callable)
  ([remap Info-goto-emacs-command-node] 'helpful-function)
  (help-map
   "." 'helpful-at-point))

(use-package emacs
  :ensure nil
  :init
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil))

(use-package delsel
  :ensure nil
  :general
  ("C-c C-g" 'minibuffer-keyboard-quit)
  :hook
  (after-init-hook . delete-selection-mode))

(use-package simple
  :ensure nil
  :hook
  (after-init-hook . column-number-mode))

(use-package prog-mode
  :ensure nil
  :hook
  (after-init-hook . global-prettify-symbols-mode))

(use-package so-long
  :ensure nil
  :hook
  (after-init-hook . global-so-long-mode))

(use-package hungry-delete
  :hook
  (after-init-hook . global-hungry-delete-mode))

(use-package elec-pair
  :ensure nil
  :hook
  (after-init-hook . electric-pair-mode))

(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer-hook . outline-show-all)
  (ediff-quit-hook . tab-bar-history-back))

(use-package undo-fu-session
  :init
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :hook
  (after-init-hook . undo-fu-session-global-mode))

(use-package vundo
  :general
  ("C-x u" 'vundo)
  :config
  (setq vundo-compact-display t)
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package hl-line
  :ensure nil
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  :hook
  (after-init-hook . show-paren-mode))

(use-package paren-face
  :hook
  (after-init-hook . global-paren-face-mode))

(use-package colorful-mode)

(use-package page-break-lines
  :hook
  (after-init-hook . global-page-break-lines-mode))

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

(use-package hl-todo
  :init
  (setq hl-todo-highlight-punctuation ":")
  ;; stolen from doom-emacs
  (setq hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold)))
  :hook
  (after-init-hook . global-hl-todo-mode))

(use-package display-line-numbers
  :ensure nil
  :init
  (setq display-line-numbers-width-start t))

(use-package anzu
  ;; :init
  ;; (setq anzu-cons-mode-line-p nil)
  :hook
  (after-init-hook . global-anzu-mode))

(use-package evil-anzu
  :if +with-evil
  :demand
  :after evil anzu)

(use-package hideshow
  :ensure nil
  :init
  (setq hs-show-indicators t)
  (setq hs-allow-nesting t)
  :hook
  (prog-mode-hook . hs-minor-mode)
  (conf-mode-hook . hs-minor-mode)
  (yaml-ts-mode-hook . hs-minor-mode))

(use-package jinx
  :general
  ([remap ispell-word] 'jinx-correct)
  :init
  (setq jinx-languages "ru en es")
  :hook
  (text-mode-hook       . jinx-mode)
  (org-mode-hook        . jinx-mode)
  ;; (prog-mode-hook       . jinx-mode)
  (git-commit-mode-hook . jinx-mode))

(use-package flymake
  :ensure nil
  :general
  (project-prefix-map
   "D" 'flymake-show-project-diagnostics)
  :init
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-margin-indicator-position 'right-margin)
  (setq flymake-suppress-zero-counters t)
  :hook
  (prog-mode-hook . flymake-mode))

(use-package sideline
  :init
  (setq sideline-backends-right '(sideline-flymake))
  (setq sideline-display-backend-name t))

(use-package sideline-flymake
  :init
  (setq sideline-flymake-display-mode 'point)
  :hook
  (flymake-mode-hook . sideline-mode))

(use-package eros
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package quickrun)

(use-package apheleia
  :hook
  (after-init-hook . apheleia-global-mode))

(use-package xref
  :ensure nil
  :init
  (setq xref-search-program 'ripgrep)
  :hook
  (after-init-hook . global-xref-mouse-mode))

(use-package avy
  :init
  (setq avy-background t))

(use-package link-hint)

(use-package ghostel
  :preface
  (defun +ghostel-notify (title body)
    (let* ((proj (when-let* ((p (project-current)))
                   (project-name p)))
           (summary (if (or (null title) (string-empty-p title))
                        (buffer-name)
                      title))
           (summary (if (or (null proj) (string-empty-p proj))
                        summary
                      (format "%s: %s" proj summary))))
      (if (fboundp 'do-applescript)
          (do-applescript
           (format "display notification \"%s\" with title \"%s\"" body summary))
        (message "%s: %s" summary body))
      (when (fboundp 'system-taskbar-attention)
        (system-taskbar-attention 'critical))))
  (defun +ghostel-setup ()
    (setq-local nobreak-char-display nil) ;; don't render as `_'
    )
  :general
  (project-prefix-map
   "t" 'ghostel-project)
  :init
  (setq ghostel-shell "/opt/homebrew/bin/fish")
  (setq ghostel-macos-login-shell nil)
  (setq ghostel-notification-function #'+ghostel-notify)
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . ghostel-mode))
  :hook
  (ghostel-mode-hook . +ghostel-setup)
  (after-init-hook . ghostel-compile-global-mode)
  (after-init-hook . ghostel-comint-global-mode))

(use-package evil-ghostel
  :hook
  (ghostel-mode-hook . evil-ghostel-mode))

(use-package vc
  :ensure nil
  :init
  (setq vc-handled-backends '(Git))
  (setq vc-async-checkin t)
  (setq vc-allow-async-diff t)
  (setq vc-display-status 'no-backend))

(use-package magit
  :preface
  (defun +magit-worktree-switch-project (worktree)
    "Create .dir-locals.el with `project-vc-name' in WORKTREE.
Sets project-vc-name to \"project[worktree]\" so that `project.el'
can distinguish worktrees from the main checkout."
    (interactive
     (list (or (magit-section-value-if 'worktree)
               (magit-completing-read
                "Switch project in worktree"
                (magit-list-worktrees)
                nil t))))
    (let* ((path (if (listp worktree) (car worktree) worktree))
           (proj-orig (caar (magit-list-worktrees))))
      ;; Skip .dir-locals.el for primary worktree — it already has the
      ;; correct project name from its directory.
      (unless (string= (file-truename path) (file-truename proj-orig))
        (let* ((wt-name (file-name-nondirectory (directory-file-name path)))
               (proj-name (format "%s[%s]"
                                  (file-name-nondirectory (directory-file-name proj-orig))
                                  wt-name))
               (proj-new (project-current nil path)))
          (unless (and proj-new (equal (project-name proj-new) proj-name))
            (save-window-excursion
              (modify-dir-local-variable nil 'project-vc-name proj-name 'add-or-replace
                                         (expand-file-name ".dir-locals.el" path))
              (save-buffer)
              (kill-buffer)))))
      (project-remember-project (project-current nil path))
      (project-switch-project path)))
  :init
  (setq magit-define-global-key-bindings 'recommended)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories `((,user-emacs-directory . 0)
                                       ("~/Projects/"         . 2)
                                       ("~/Developer/"        . 2)))
  (setq magit-diff-refine-hunk t)
  (setq magit-process-apply-ansi-colors t)
  (when +with-icons
    (setq magit-format-file-function #'magit-format-file-nerd-icons))
  :config
  (transient-append-suffix 'magit-worktree "g"
    '("p" "Switch project" +magit-worktree-switch-project))
  ;; https://github.com/magit/magit/issues/3230#issuecomment-339900039
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace))

(use-package magit-blame
  :ensure magit
  :config
  (add-to-list
   'magit-blame-styles
   '(margin
     (margin-format    . ("%C %a%f"))
     (time-format      . "%F")
     (margin-width     . 32)
     (margin-face      . magit-blame-margin)
     (margin-body-face . (magit-blame-dimmed))
     (show-message     . t))))

(use-package git-modes
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package diff-hl
  :preface
  (defun +diff-hl-fringe-bmp-empty (_type _pos) 'diff-hl-bmp-empty)
  ;; https://github.com/dgutov/diff-hl/issues/116#issuecomment-1573253134
  (let* ((width 4)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap '+diff-hl-bmp-thin bitmap 1 width '(top t)))
  (defun +diff-hl-fringe-bmp-thin (_type _pos) '+diff-hl-bmp-thin)
  (defun +diff-hl-update-faces (&optional _theme)
    "Adapt diff-hl faces for fringe bitmap rendering.
Fringe bitmaps use the face foreground color, not background.
Move each face's background color to foreground and clear the
background so our thin bitmap displays the indicator color correctly.
Covers both working-tree faces and reference-revision faces."
    (when (display-graphic-p)
      (dolist (face '(diff-hl-insert
                      diff-hl-delete
                      diff-hl-change
                      diff-hl-reference-insert
                      diff-hl-reference-delete
                      diff-hl-reference-change))
        (when-let* ((bg (face-background face nil t)))
          (set-face-foreground face bg) ;; fg -> bg
          (set-face-background face nil) ;; bg -> nil (transparent)
          ))))
  :init
  (setq diff-hl-disable-on-remote t)
  (setq diff-hl-update-async 'thread)
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-margin-symbols-alist
        '((insert . " ") (delete . " ") (change . " ")
          (unknown . " ") (ignored . " ") (reference . " ")))
  ;; (setq diff-hl-fringe-bmp-function #'+diff-hl-fringe-bmp-empty)
  (setq diff-hl-fringe-bmp-function #'+diff-hl-fringe-bmp-thin)
  (setq diff-hl-fringe-flat-bmp '+diff-hl-bmp-thin)
  :hook
  (after-init-hook . global-diff-hl-mode)
  (after-init-hook . global-diff-hl-show-hunk-mouse-mode)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (diff-hl-mode-hook . +diff-hl-update-faces)
  (enable-theme-functions . +diff-hl-update-faces))

(use-package diff-hl-flydiff
  :disabled
  :ensure diff-hl
  :init
  (setq diff-hl-flydiff-delay 0.5)
  :hook
  (diff-hl-mode-hook . diff-hl-flydiff-mode))

(use-package diff-hl-dired
  :ensure diff-hl
  :preface
  (defun +diff-hl-dired-update ()
    (when (and (bound-and-true-p diff-hl-dired-mode)
               (memq this-command '(dired-subtree-insert
                                    dired-subtree-toggle
                                    dired-subtree-cycle)))
      (diff-hl-dired-update)))
  :init
  (setq diff-hl-dired-extra-indicators nil)
  ;; (setq diff-hl-dired-fringe-bmp-function #'+diff-hl-fringe-bmp-empty)
  (setq diff-hl-dired-fringe-bmp-function #'+diff-hl-fringe-bmp-thin)
  :config
  ;; diff-hl overlays are zero-width (point, point) and survive `delete-region'
  ;; — they leak onto the next visible line. Remove them before subtree deletion.
  (define-advice dired-subtree-remove (:before () clean-diff-hl-overlays)
    "Remove diff-hl overlays in subtree region before deletion."
    (when (bound-and-true-p diff-hl-dired-mode)
      (-when-let (ov (dired-subtree--get-ov))
        (diff-hl-remove-overlays (overlay-start ov) (overlay-end ov)))))
  :hook
  (dired-mode-hook . diff-hl-dired-mode-unless-remote)
  (dired-subtree-after-insert-hook . +diff-hl-dired-update)
  (diff-hl-dired-mode-hook . +diff-hl-update-faces))

(use-package git-link)

(use-package difftastic
  :hook
  (after-init-hook . difftastic-bindings-mode))

(use-package git-timemachine)

(use-package org
  :ensure nil
  :init
  (setq org-modules nil)

  (setq org-directory "~/Org")

  ;; (setq org-startup-folded 'overview)
  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content t)
  (setq org-hide-leading-stars t)

  (setq org-agenda-files '("todo.org"))
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-skip-unavailable-files t)

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

  (setq org-ellipsis "…")
  ;; (setq org-ellipsis " ⌄ ")
  (setq org-pretty-entities t)
  ;; (setq org-hide-emphasis-markers t)
  (setq org-use-sub-superscripts '{}) ;; allow _ and ^ characters to sub/super-script strings but only when string is wrapped in braces

  (setq org-use-fast-todo-selection 'expert)
  (setq org-todo-keywords '((sequence
                             "TODO(t)"
                             "STARTED(s)"
                             "NEXT(n)"
                             "WAITING(w)"
                             "HOLD(h)"
                             "|"
                             "DONE(d)"
                             "OBSOLETE(o)"
                             "CANCELLED(c)")))

  (setq org-log-done 'time)

  (setq org-startup-with-link-previews t)

  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline nil)

  (setq org-imenu-depth 6))

(use-package org-archive
  :ensure org
  :init
  (setq org-archive-location (concat org-directory "/archive.org::datetree/"))
  (setq org-archive-file-header-format nil))

(use-package org-refile
  :ensure org
  :preface
  ;; https://github.com/progfolio/.emacs.d#refile
  (defun +org-files-list ()
    "Returns a list of the file names for currently open Org files"
    (delq nil
          (mapcar (lambda (buffer)
                    (when-let* ((file-name (buffer-file-name buffer))
                                (directory (file-name-directory file-name)))
                      (unless (string-suffix-p "archives/" directory)
                        file-name)))
                  (org-buffer-list 'files t))))
  :init
  (setq org-refile-targets `((org-agenda-files :maxlevel . 3)
                             (+org-files-list  :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t))

(use-package ol
  :ensure org
  :preface
  ;; source: https://gist.github.com/kim366/8abe978cc295b027df636b218862758e
  (defun +org-link-get-title (link &optional description)
    (cond
     ((string-match-p "^http" link)
      (with-temp-buffer
        (url-insert-file-contents link)
        (goto-char (point-min))
        (search-forward-regexp (rx "<title" (* (not (any ">"))) ">" (group (*? anything)) "</title>"))
        (match-string 1)))))
  :init
  (setq org-link-make-description-function #'+org-link-get-title))

(use-package org-src
  :ensure org
  :init
  (setq org-src-window-setup 'current-window)
  (setq org-src-content-indentation 0))

(use-package org-agenda
  :ensure org
  :init
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-tags-column 0))

(use-package org-capture
  :ensure org
  :init
  (setq org-capture-templates
        `(("j" "Journal")
          ("jj" "Journal note" entry
           (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
           "* %<%H:%M> - %^{Title|untitled} :journal:\n\n%?\n\n"
           :empty-lines 1)
          ("jm" "Meeting note" entry
           (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
           "* %<%H:%M> - %^{Title|untitled} :meeting:\n\n%?\n\n"
           :empty-lines 1 :clock-in t :clock-resume t)
          )))

(use-package org-faces
  :ensure org
  :init
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-priority-faces
        '((?A . (:inherit (bold error)))
          (?B . (:inherit (bold warning)))
          (?C . (:inherit (bold success)))))
  ;; TODO: simplify
  (setq org-todo-keyword-faces
        '(("STARTED"   . (:inherit (bold font-lock-constant-face org-todo)))
          ("NEXT"      . (:inherit (bold font-lock-constant-face org-todo)))
          ("WAITING"   . (:inherit (bold warning org-todo)))
          ("HOLD"      . (:inherit (bold warning org-todo)))
          ("OBSOLETE"  . (:inherit (bold shadow org-todo)))
          ("CANCELLED" . (:inherit (bold shadow org-todo))))))

(use-package toc-org
  :init
  (setq toc-org-max-depth 6)
  :hook
  (org-mode-hook . toc-org-enable))

(use-package ob-core
  :ensure org
  :preface
  (defun +org-babel-add-lang (lang)
    "Enable LANG in `org-babel-load-languages'.
  Use instead of `org-babel-do-load-languages' to avoid
  overwriting `org-babel-load-languages'."
    (add-to-list 'org-babel-load-languages (cons lang t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
  :init
  ;; built-in languages, for external use `+org-babel-add-lang'
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (shell      . t)
          (plantuml   . t)))
  (setq org-babel-results-keyword "results")
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images))

(use-package ob-tangle
  :ensure org
  :init
  (add-to-list 'safe-local-variable-values '(after-save-hook . org-babel-tangle)))

(use-package ob-plantuml
  :ensure nil
  :init
  (setq org-plantuml-exec-mode 'plantuml))

(use-package verb
  :general
  (org-mode-map
   "C-c C-r" '(:keymap verb-command-map :package verb :wk "verb"))
  :init
  (setq verb-auto-kill-response-buffers t)
  (setq verb-json-use-mode 'json-ts-mode)
  :config
  (+org-babel-add-lang 'verb))

(use-package org-crypt
  :ensure org
  :init
  (setq org-tags-exclude-from-inheritance '("crypt"))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)
  :config
  (org-crypt-use-before-save-magic))

(use-package deft
  :general
  ( :keymaps 'deft-mode-map :states 'normal
    "gr" 'deft-refresh)
  :init
  (setq deft-directory (concat org-directory "/deft/"))
  (setq deft-extensions '("org"))
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0) ;; disable
  (setq deft-file-naming-rules ;; kebab-case
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase))))

(use-package eglot
  :ensure nil
  :general
  (+local-leader-def :keymaps 'eglot-mode-map
    "=" 'eglot-format
    "a" 'eglot-code-actions
    "R" 'eglot-rename
    "f" '(:ignore t :wk "find")
    "fd" 'eglot-find-declaration
    "ft" 'eglot-find-typeDefinition
    "fr" 'eglot-find-references
    "fi" 'eglot-find-implementation
    "h" '(:ignore t :wk "hierarchy")
    "hc" 'eglot-show-call-hierarchy
    "ht" 'eglot-show-type-hierarchy)
  :init
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)
  (setq eglot-events-buffer-config '(:size 0 :format full))
  :config
  ;; https://github.com/doomemacs/doomemacs/blob/master/modules/tools/lsp/%2Beglot.el
  (defvar +eglot-defer-shutdown 3
    "Seconds to defer `eglot-shutdown' triggered by `eglot-autoshutdown'.")
  (define-advice eglot--managed-mode
      (:around (fn &rest args) +defer-shutdown)
    "Defer auto-shutdown so buffer kills don't block on slow LSP shutdown,
  and reopening project files within the window avoids a restart."
    (cl-letf* ((orig (symbol-function #'eglot-shutdown))
               ((symbol-function #'eglot-shutdown)
                (lambda (server &rest _)
                  (run-with-idle-timer
                   +eglot-defer-shutdown nil
                   (lambda ()
                     (unless (eglot--managed-buffers server)
                       (funcall orig server)))))))
      (apply fn args))))

(use-package consult-eglot
  :general
  (+local-leader-def :keymaps 'eglot-mode-map
    "fs" 'consult-eglot-symbols))

(use-package consult-eglot-embark
  :config
  (consult-eglot-embark-mode))

(use-package dape
  :init
  (setq dape-key-prefix (kbd "C-x C-a"))
  (setq dape-inlay-hints t)
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-breakpoint-margin-string "●")
  :config
  (dape-breakpoint-global-mode)
  :hook
  (kill-emacs-hook . dape-breakpoint-save)
  (after-init-hook . dape-breakpoint-load))

(use-package mason
  :hook
  (after-init-hook . mason-ensure))

(use-package treesit
  :ensure nil
  :init
  (setq treesit-font-lock-level 4))

(use-package elisp-mode
  :ensure nil
  :init
  (setq elisp-fontify-semantically t))

(use-package highlight-defined
  :init
  (setq highlight-defined-face-use-itself t)
  :hook
  (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package macrostep
  :general
  (+local-leader-def :keymaps 'emacs-lisp-mode-map
    "m" 'macrostep-expand))

(use-package elisp-def
  :general
  (+local-leader-def :keymaps 'emacs-lisp-mode-map
    "d" 'elisp-def))

(use-package package-lint)

(use-package package-lint-flymake
  :hook
  (emacs-lisp-mode-hook . package-lint-flymake-setup))

(use-package clojure-ts-mode)

(use-package cider
  :general
  (+local-leader-def :keymaps 'clojure-ts-mode-map
    "c" '(:ignore t           :wk "connect")
    "cc" '(cider-jack-in      :wk "jack-in")
    "cj" '(cider-jack-in-clj  :wk "jack-in-clj")
    "cs" '(cider-jack-in-cljs :wk "jack-in-cljs")
    "cC" '(cider-connect      :wk "connect")
    "cR" '(cider-restart      :wk "restart")
    "cQ" '(cider-quit         :wk "quit")

    "b" '(:ignore t           :wk "buffer")
    "bs" 'cider-scratch

    "=" '(cider-format-buffer :wk "format"))
  :init
  (setq cider-eldoc-display-context-dependent-info t)
  :hook
  (clojure-ts-mode-hook . cider-mode))

(use-package clj-refactor
  :disabled
  :general
  (+local-leader-def :keymaps 'clojure-ts-mode-map
    "R" '(hydra-cljr-help-menu/body :wk "refactor"))
  :hook
  (clojure-ts-mode-hook . clj-refactor-mode))

(use-package go-ts-mode
  :ensure nil
  :init
  (setq go-ts-mode-indent-offset 4)
  :config
  (put 'go-ts-mode-build-tags 'safe-local-variable #'listp)
  :hook
  (go-ts-mode-hook . eglot-ensure))

(use-package makefile-executor
  :general
  (+local-leader-def :keymaps 'makefile-mode-map
    "e" 'makefile-executor-execute-target)
  :hook
  (makefile-mode-hook . makefile-executor-mode))

(use-package plantuml-mode
  :general
  (+local-leader-def :keymaps 'plantuml-mode-map
    "p" '(plantuml-preview :wk "preview"))
  :init
  (setq plantuml-output-type (if (display-images-p) "png" "txt"))
  (setq plantuml-default-exec-mode 'executable))

(use-package sql
  :ensure nil
  :general
  (+local-leader-def :keymaps 'sql-mode-map
    "c" '(:ignore t :wk "connect")
    "cc" '(sql-connect :wk "connect")

    "e" '(:ignore t :wk "eval")
    "ee" '(sql-send-paragraph :wk "paragraph")
    "el" '(sql-send-line-and-next :wk "line and next")
    "eb" '(sql-send-buffer :wk "buffer")
    "er" '(sql-send-region :wk "region")
    "es" '(sql-send-string :wk "string")

    "l" '(:ignore t :wk "list")
    "la" '(sql-list-all :wk "all")
    "lt" '(sql-list-table :wk "table"))
  :init
  (setq sql-connection-alist '((pg-local
                                (sql-product 'postgres)
                                (sql-port 5432)
                                (sql-server "localhost")
                                (sql-user "postgres")
                                (sql-password "postgres")
                                (sql-database "postgres")))))

(use-package markdown-ts-mode
  :ensure nil
  :init
  (setq markdown-ts-ellipsis "…")
  (setq markdown-ts-inline-images t))

(use-package markdown-ts-mode-x
  :ensure nil
  :hook
  (markdown-ts-mode-hook . markdown-ts-toc-update-before-save-mode))

(use-package grip-mode
  :general
  (+local-leader-def :keymaps 'markdown-ts-mode-map
    "p" 'grip-mode))

(use-package json-ts-mode
  :ensure nil
  :mode ("\\.json\\'" . json-ts-mode)
  :general
  (+local-leader-def :keymaps 'json-ts-mode-map
    "=" '(json-pretty-print-buffer :wk "format")))

(use-package yaml-ts-mode
  :ensure nil
  :preface
  (defun +yaml-ts-mode-set-evil-shift-width ()
    (setq-local evil-shift-width 2))
  :init
  (setq yaml-ts-mode-yamllint-options
        '("-d" "{extends: relaxed, rules: {line-length: {max: 120}}}"))
  :hook
  (yaml-ts-mode-hook . flymake-mode)
  (yaml-ts-mode-hook . +yaml-ts-mode-set-evil-shift-width))

(use-package lua-ts-mode
  :ensure nil
  :mode ("\\.lua\\'" . lua-ts-mode)
  :interpreter ("\\<lua\\(?:jit\\)?" . lua-ts-mode)
  :hook
  (lua-ts-mode-hook . eglot-ensure))

(use-package executable
  :ensure nil
  :hook
  (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package flymake-shellcheck
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

(use-package fish-mode)

(use-package vimrc-mode)

(use-package ssh-config-mode)

(use-package protobuf-mode)

(use-package dockerfile-ts-mode
  :ensure nil
  :mode ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode))

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")

(use-package editorconfig
  :ensure nil
  :hook
  (after-init-hook . editorconfig-mode))

(use-package docker)

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package ansible-vault-with-editor
  :vc (:url "https://github.com/rynffoll/ansible-vault-with-editor" :rev :newest)
  :general
  (+local-leader-def :keymaps 'yaml-ts-mode-map
    "e" '(ansible-vault-with-editor-edit :wk "edit")
    "E" '(ansible-vault-with-editor-encrypt :wk "encrypt")
    "D" '(ansible-vault-with-editor-decrypt :wk "decrypt")))

(use-package mise
  :hook
  (after-init-hook . global-mise-mode))

(use-package proced
  :ensure nil
  :init
  (setq proced-enable-color-flag t)
  (setq proced-auto-update-flag 'visible)
  (setq proced-format 'medium))

(use-package proced-narrow
  :general
  (proced-mode-map
   "M-n" 'proced-narrow))

(use-package keycast
  :init
  (setq keycast-tab-bar-location 'tab-bar-format-global)
  (setq keycast-tab-bar-format "%K")
  (setq keycast-tab-bar-minimal-width 2))

(use-package dothttp
  :ensure nil
  :mode ("\\.http\\'" . dothttp-mode))

(use-package dothttp-grpc
  :ensure nil
  :demand t
  :after dothttp)

(use-package flamegraph)

(use-package code-review
  :ensure nil
  :demand t
  :general
  (+leader-def
    "r"  '(:ignore t :wk "review")
    "ra" 'code-review-annotate
    "re" 'code-review-edit
    "rl" 'code-review-list
    "rc" 'code-review-send-to-claude)
  :config
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates code-review-capture-template t))
  (add-hook 'flymake-diagnostic-functions #'code-review-flymake)
  ;; review.org wires its own save/revert hooks via a -*- cookie; whitelist them
  (add-to-list 'safe-local-variable-values
               '(after-save-hook . code-review--after-review-change))
  (add-to-list 'safe-local-variable-values
               '(after-revert-hook . code-review--after-review-change)))

(use-package gptel
  :general
  (+local-leader-def :keymaps 'gptel-mode-map
    "." 'gptel-menu)
  (embark-general-map
   "." #'gptel-menu)
  :init
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'gpt-5-mini)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :hook
  (gptel-post-stream-hook . gptel-auto-scroll)
  (gptel-post-response-functions . gptel-end-of-response))

(use-package gptel-magit
  :hook
  (magit-mode-hook . gptel-magit-install))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :general
  (project-prefix-map
   "a" 'claude-code-ide-menu)
  :init
  (setq claude-code-ide-terminal-backend 'ghostel)
  (setq claude-code-ide-use-side-window nil)
  (setq claude-code-ide-show-claude-window-in-ediff nil)
  ;; (setq claude-code-ide-switch-tab-on-ediff nil) ;; it doesn't work (the same tab names)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package copilot
  :general
  (copilot-completion-map
   "<tab>"   'copilot-accept-completion
   "TAB"     'copilot-accept-completion
   "C-<tab>" 'copilot-accept-completion-by-word
   "C-TAB"   'copilot-accept-completion-by-word
   "C-j"     'copilot-next-completion
   "C-k"     'copilot-previous-completion
   "C-n"     'copilot-next-completion
   "C-p"     'copilot-previous-completion)
  :init
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 1000000)
  (setq copilot-max-char-warning-disable t)
  :hook
  ;; (after-init-hook . global-copilot-mode)
  (prog-mode-hook . copilot-mode)
  (conf-mode-hook . copilot-mode)
  (git-commit-mode-hook . copilot-mode)
  (yaml-ts-mode-hook . copilot-mode))

(use-package focus)

(use-package olivetti
  :init
  (setq olivetti-body-width 0.6))

(use-package crux)

(use-package try)

(use-package string-inflection)

(use-package show-font)

(use-package disk-usage)

(use-package list-environment)

(use-package daemons)

(use-package free-keys)

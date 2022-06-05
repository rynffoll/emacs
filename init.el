;;; -*- lexical-binding: t; -*-

(setq user-full-name "Ruslan Kamashev"
      user-login-name "rynffoll"
      user-mail-address "rynffoll@gmail.com")

(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-hook-name-suffix nil)
(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)

(eval-when-compile
  (require 'use-package))

(use-package quelpa-use-package
  :demand
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t))

(use-package gnu-elpa-keyring-update)

(use-package auto-compile
  :init
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-use-mode-line nil)
  :hook
  (emacs-lisp-mode-hook . auto-compile-on-load-mode)
  (emacs-lisp-mode-hook . auto-compile-on-save-mode))

(use-package gcmh
  :hook
  (after-init-hook . gcmh-mode))

(use-package emacs
  :ensure nil
  :init
  (setq load-prefer-newer t)
  (setq use-dialog-box nil)
  (setq enable-recursive-minibuffers t)
  (setq indent-tabs-mode nil)
  (setq create-lockfiles nil)
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise t)
  (setq inhibit-compacting-font-caches t)
  (setq scroll-step 1)
  (setq scroll-preserve-screen-position t)
  (setq scroll-margin 0)
  (setq scroll-conservatively 101)
  (setq ring-bell-function 'ignore)
  (setq delete-by-moving-to-trash t)
  (setq read-process-output-max (* 1024 1024))
  (setq bidi-inhibit-bpa t)
  (setq bidi-display-reordering 'left-to-right)
  (setq bidi-paragraph-direction 'left-to-right)
  (setq fast-but-imprecise-scrolling t)
  (setq native-comp-async-report-warnings-errors nil)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package mule
  :ensure nil
  :init
  (setq default-input-method 'russian-computer)
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package emacs
  :ensure nil
  :init
  (setq buffer-file-coding-system 'utf-8))

(use-package select
  :ensure nil
  :init
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package calendar
  :ensure nil
  :init
  (setq calendar-date-style 'iso)
  (setq calendar-week-start-day 1))

(use-package async
  :hook
  (after-init-hook . async-bytecomp-package-mode)
  (dired-mode-hook . dired-async-mode))

(use-package general
  :config
  (general-create-definer -leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (general-create-definer -local-leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-,")
  (-leader-def
    ""    '(nil :wk "leader")
    "o"   '(:ignore t :wk "open")
    "O"   '(:ignore t :wk "org")
    "p"   '(:ignore t :wk "project")
    "P"   '(:ignore t :wk "package")
    "F"   '(:ignore t :wk "frame")
    "TAB" '(:ignore t :wk "tab")
    "b"   '(:ignore t :wk "buffer")
    "f"   '(:ignore t :wk "file")
    "e"   '(:ignore t :wk "emacs")
    "g"   '(:ignore t :wk "git")
    "/"   '(:ignore t :wk "search")
    "j"   '(:ignore t :wk "jump")
    "h"   '(:ignore t :wk "help")
    "t"   '(:ignore t :wk "toggle")
    "i"   '(:ignore t :wk "insert")
    "q"   '(:ignore t :wk "quit"))
  (-local-leader-def
    ""    '(nil :wk "local leader")))

(use-package evil
  :demand
  :preface
  (defun -save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  (defun -disable-evil-cursor ()
    (setq-local evil-default-cursor '(nil)))
  :general
  (evil-insert-state-map
   "C-k" nil)
  (-leader-def
    "j[" 'evil-jump-backward
    "j]" 'evil-jump-forward)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-emacs-state-cursor 'hbar)
  (setq evil-mode-line-format nil)
  (setq evil-symbol-word-search t)
  (setq evil-move-beyond-eol nil)
  (setq evil-move-cursor-back t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" '-save-and-kill-buffer))

(use-package evil-collection
  :demand
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  (setq evil-collection-magit-want-horizontal-movement t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :hook
  (after-init-hook . evil-commentary-mode))

(use-package evil-surround
  :hook
  (after-init-hook . global-evil-surround-mode))

(use-package evil-matchit
  :hook
  (after-init-hook . global-evil-matchit-mode))

(use-package evil-org
  :init
  (setq evil-org-key-theme '(todo textobjects insert navigation heading))
  :hook
  (org-mode-hook . evil-org-mode))

(use-package evil-org-agenda
  :demand
  :ensure evil-org
  :after org-agenda
  :config
  (evil-org-agenda-set-keys))

(use-package evil-mc
  :hook
  (after-init-hook . global-evil-mc-mode))

(use-package evil-traces
  :hook
  (after-init-hook . evil-traces-mode)
  :config
  (evil-traces-use-diff-faces))

(use-package which-key
  :init
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 1.0)
  (setq which-key-idle-secondary-delay 0.05)
  :hook
  (after-init-hook . which-key-mode))

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

(use-package startup
  :ensure nil
  :init
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil))

(tooltip-mode -1)
(menu-bar-mode -1)

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(when (eq window-system 'ns)
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

(use-package ligature
  :ensure nil
  :if (display-graphic-p)
  :quelpa (ligature :fetcher github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures
   'prog-mode
   '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
     "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
     "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
     "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
     "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
     "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
     ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
     "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
     "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
     "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
     "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  :hook
  (after-init-hook . global-ligature-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t :inherit mode-line :box nil :underline nil :overline nil)))
  (mode-line-inactive ((t :inherit mode-line-inactive :box nil :underline nil :overline nil))))

(use-package hide-mode-line
  :hook
  (dired-sidebar-mode-hook . hide-mode-line-mode))

(use-package minions
  :hook
  (after-init-hook . minions-mode))

(use-package doom-modeline
  :init
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)
  :hook
  (after-init-hook . doom-modeline-mode)
  :config
  (dolist (name '("*Compile-Log*" "*Async-native-compile-log*"))
    (when-let ((buffer (get-buffer name)))
      (with-current-buffer buffer
        (doom-modeline-set-main-modeline)))))

(use-package solarized-theme
  :init
  (setq solarized-distinct-doc-face t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-scale-outline-headlines nil)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-gruvbox-dark t))

(use-package paradox
  :general
  (-leader-def
    "Pl" 'paradox-list-packages
    "PU" 'paradox-upgrade-packages)
  :init
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t))

(use-package frame
  :ensure nil
  :general
  (-leader-def
    "Ff" 'select-frame-by-name
    "Fn" 'make-frame-command
    "Fc" 'delete-frame
    "FC" 'delete-other-frames
    "Fo" 'other-frame
    "Fb" 'switch-to-buffer-other-frame
    "FM" 'toggle-frame-maximized
    "FF" 'toggle-frame-fullscreen)
  :config
  (blink-cursor-mode -1))

(use-package ns-win
  :if (eq window-system 'ns)
  :ensure nil
  :general
  (-leader-def
    "F[" 'ns-prev-frame
    "F]" 'ns-next-frame))

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

(use-package default-text-scale
  :hook
  (after-init-hook . default-text-scale-mode))

(use-package tab-bar
  :ensure nil
  :preface
  (defun -tab-bar-print-tabs (&optional ignore)
    (interactive)
    (let* ((separator (propertize "|" 'face '(shadow)))
           (tabs
            (mapconcat
             (lambda (tab)
               (let* ((type (car tab))
                      (index (1+ (tab-bar--tab-index tab)))
                      (name (alist-get 'name tab))
                      (face (if (equal type 'current-tab)
                                '(font-lock-constant-face :inverse-video t)
                              '(shadow))))
                 (propertize (format " %d:%s " index name) 'face face)))
             (tab-bar-tabs) separator)))
      (message tabs)))
  (defun -tab-bar-rename-or-close (name)
    (if name
        (tab-rename name)
      (progn
        (tab-close)
        (setq quit-flag nil))))
  (defun -tab-bar-post-open-rename (tab)
    (let* ((index (1+ (tab-bar--current-tab-index)))
           (prompt (format "%d:" index))
           (inhibit-quit t)
           (name (with-local-quit (read-string prompt))))
      (-tab-bar-rename-or-close name)))
  (defun -tab-bar-post-open-projectile (tab)
    (let* ((inhibit-quit t)
           (project (with-local-quit (projectile-switch-project)))
           (name (when project
                   (file-name-nondirectory
                    (directory-file-name project)))))
      (-tab-bar-rename-or-close name)))
  (defun -tab-bar-projectile ()
    (interactive)
    (let* ((tab-bar-tab-post-open-functions #'-tab-bar-post-open-projectile))
      (tab-new)))
  :general
  (-leader-def
    "TAB TAB" '-tab-bar-print-tabs
    "TAB ."   'tab-bar-select-tab-by-name
    "TAB n"   'tab-new
    "TAB p"   '-tab-bar-projectile
    "TAB ["   'tab-previous
    "TAB ]"   'tab-next
    "TAB c"   'tab-close
    "TAB C"   'tab-close-other
    "TAB r"   'tab-rename
    "TAB u"   'tab-undo)
  :init
  (setq tab-bar-tab-hints t)
  ;; (setq tab-bar-select-tab-modifiers '(meta))
  (setq tab-bar-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-new-tab-to 'rightmost)
  (setq tab-bar-tab-post-open-functions #'-tab-bar-post-open-rename)
  :config
  (mapcar
   (lambda (f) (advice-add f :after #'-tab-bar-print-tabs))
   '(tab-new
     tab-close
     tab-close-other
     tab-undo
     tab-select
     tab-next
     tab-previous
     tab-recent
     tab-move
     tab-move-to)))

(use-package window
  :ensure nil
  :general
  (evil-window-map
   "m" 'maximize-window
   "M" 'minimize-window))

(use-package winner
  :ensure nil
  :general
  (evil-window-map
   "u" 'winner-undo
   "U" 'winner-redo)
  :init
  (setq winner-dont-bind-my-keys t)
  :hook
  (after-init-hook . winner-mode))

(use-package winum
  :demand
  :general
  (-leader-def
    "0" 'winum-select-window-0-or-10
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9)
  :init
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local)
  :config
  (winum-mode))

(use-package emacs
  :ensure nil
  :preface
  (defun -switch-to-scratch () (interactive) (switch-to-buffer "*scratch*"))
  (defun -switch-to-messages () (interactive) (switch-to-buffer "*Messages*"))
  :general
  (-leader-def
    "bs" '(-switch-to-scratch :wk "open scratch")
    "bm" '(-switch-to-messages :wk "open messages")
    "bR" 'rename-buffer))

(use-package menu-bar
  :ensure nil
  :general
  (-leader-def
    "bk" 'kill-this-buffer

    "tde" 'toggle-debug-on-error
    "tdq" 'toggle-debug-on-quit))

(use-package window
  :ensure nil
  :general
  (-leader-def
    "bb" 'switch-to-buffer
    "bK" 'kill-buffer-and-window))

(use-package ibuffer
  :ensure nil
  :general
  ([remap list-buffers] 'ibuffer)
  (-leader-def
    "bi" 'ibuffer))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package evil-commands
  :ensure evil
  :after evil
  :general
  (-leader-def
    "bn" 'evil-buffer-new
    "b]" 'evil-next-buffer
    "b[" 'evil-prev-buffer))

(use-package ibuffer-vc
  :preface
  (defun -setup-ibuffer-vc ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  (ibuffer-hook . -setup-ibuffer-vc))

(use-package shackle
  :init
  (setq shackle-default-size 0.3)
  (setq shackle-rules '((help-mode :align below :select t)
                        (helpful-mode :align below)
                        (flycheck-error-list-mode :align below)
                        (cider-repl-mode :align below)
                        (ansible-doc-module-mode :align below)
                        ("*Pack*" :align below)
                        ("\\*Async Shell Command\\*.*" :regexp t :ignore t)
                        (Man-mode :align below :select t)
                        ("\\*Man.*\\*" :regexp t :align below :select t)
                        ("*lsp-help*" :align below)
                        ("*Warnings*" :align below)
                        ("*Compile-Log*" :align below)
                        (compilation-mode :align below)
                        ("*company-documentation*" :align below)
                        ("*Go REPL*" :align below)
                        ("\\*docker-compose .*\\*" :regexp t :align below)
                        (comint-mode :align below)))
  :hook
  (after-init-hook . shackle-mode))

(use-package consult
  :general
  ([remap apropos]                       'consult-apropos)
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
  ([remap imenu]                         'consult-imenu)
  (-leader-def
    "/." 'consult-ripgrep
    "/b" 'consult-line)
  :init
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :hook
  (completion-list-mode-hook . consult-preview-at-point-mode)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package consult-dir
  :general
  ([remap list-directory] 'consult-dir)
  ([remap dired-jump]     'consult-dir-jump-file)
  :init
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs))

(use-package marginalia
  :general
  (minibuffer-local-map
   "M-A" 'marginalia-cycle)
  :init
  (marginalia-mode))

(use-package vertico
  :general
  (vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous)
  :init
  (setq vertico-resize 'grow-only)
  (setq vertico-cycle t)
  :hook
  (after-init-hook . vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless))
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-flex
                                    orderless-regexp)))

(use-package embark
  :general
  ([remap describe-bindings] 'embark-bindings)
  (minibuffer-local-map
   "C-." 'embark-act)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after embark consult
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package files
  :ensure nil
  :general
  (-leader-def
    "." 'find-file
    "ff" 'find-file
    "br" 'revert-buffer)
  :init
  (setq require-final-newline t)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq enable-local-variables :all)
  (setq enable-local-eval t))

(use-package autorevert
  :ensure nil
  :init
  (setq auto-revert-verbose nil)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-check-vc-info t)
  :hook
  (after-init-hook . global-auto-revert-mode))

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
  :general
  (-leader-def
    "fr" 'recentf-open-files)
  :init
  (setq recentf-max-saved-items 300)
  :hook
  (after-init-hook . recentf-mode))

(use-package files
  :if (eq system-type 'darwin)
  :ensure nil
  :init
  (setq insert-directory-program "gls")
  (setq trash-directory "~/.Trash/emacs"))

(use-package iqa
  :general
  (-leader-def
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "er" 'iqa-reload-user-init-file)
  :init
  (setq iqa-user-init-file (concat user-emacs-directory "config.org")))

(use-package custom
  :ensure nil
  :general
  (-leader-def
    "tt" 'load-theme))

(use-package cus-edit
  :ensure nil
  :general
  (-leader-def
    "oc" 'customize-group)
  :init
  (setq custom-file null-device))

(use-package epg-config
  :ensure nil
  :init
  (setq epg-pinentry-mode 'loopback))

(use-package projectile
  :general
  (-leader-def
    "p" '(:keymap projectile-command-map :package projectile :wk "project"))
  :init
  (setq projectile-project-search-path '("~/Projects"))
  :hook
  (after-init-hook . projectile-mode))

(use-package consult-projectile
  :general
  ([remap projectile-switch-project] 'consult-projectile))

(use-package dired
  :ensure nil
  :init
  (setq dired-listing-switches "-lah --group-directories-first")
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-hide-details-hide-symlink-targets nil)
  :hook
  (dired-mode-hook . dired-hide-details-mode))

(use-package dired-hide-dotfiles
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "M-." 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :preface
  (defun -dired-subtree-revert ()
    (call-interactively 'revert-buffer)
    (recenter))
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "TAB" 'dired-subtree-toggle)
  :init
  (setq dired-subtree-use-backgrounds nil)
  :config
  ;; for treemacs-icons-dired
  (advice-add #'dired-subtree-toggle :after #'-dired-subtree-revert))

(use-package tramp
  :ensure nil
  :init
  (setq tramp-default-method "ssh"))

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
  (-leader-def
    "hx" 'describe-command
    "hk" 'describe-key
    "hv" 'describe-variable
    "hf" 'describe-function
    "hF" 'describe-face
    "hb" 'describe-bindings))

(use-package man
  :ensure nil
  :general
  (-leader-def
    "hM" 'man))

(use-package helpful
  :general
  ([remap describe-command]  'helpful-command)
  ([remap describe-key]      'helpful-key)
  ([remap describe-variable] 'helpful-variable)
  ([remap describe-function] 'helpful-function)
  (-leader-def
    "h." 'helpful-at-point))

(use-package find-func
  :ensure nil
  :general
  (-leader-def
    "hl" 'find-library))

(use-package delsel
  :ensure nil
  :general
  ("C-c C-g" 'minibuffer-keyboard-quit)
  :hook
  (after-init-hook . delete-selection-mode))

(use-package simple
  :ensure nil
  :general
  (-leader-def
    ":" 'execute-extended-command
    "tT" 'toggle-truncate-lines)
  :init
  (setq backward-delete-char-untabify-method 'hungry)
  (setq async-shell-command-buffer 'new-buffer)
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
  :preface
  (defun -disable-hungry-delete-mode ()
    (hungry-delete-mode -1))
  :hook
  (after-init-hook . global-hungry-delete-mode)
  (minibuffer-setup-hook . -disable-hungry-delete-mode))

(use-package ediff
  :ensure nil
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer-hook . show-all)
  (ediff-quit-hook . winner-undo))

(use-package undo-tree
  :disabled (not (eq evil-undo-system 'undo-tree))
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,temporary-file-directory)))
  :hook
  (after-init-hook . global-undo-tree-mode))

(use-package undo-fu
  :disabled (not (eq evil-undo-system 'undo-fu)))

(use-package vundo
  :general
  ("C-x u" 'vundo)
  :hook
  (vundo-mode-hook . -disable-global-hl-line-mode)
  (vundo-mode-hook . -disable-evil-cursor)
  :custom-face
  (vundo-highlight ((t :inherit (bold success))))
  :config
  (setq vundo-compact-display t)
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package ansi-color
  :preface
  ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  :hook
  (compilation-filter-hook . endless/colorize-compilation))

(use-package hl-line
  :ensure nil
  :preface
  (defun -disable-global-hl-line-mode ()
    (setq-local global-hl-line-mode nil))
  :general
  (-leader-def
    "tl" 'global-hl-line-mode)
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package paren
  :ensure nil
  :hook
  (after-init-hook . show-paren-mode))

(use-package elec-pair
  :ensure nil
  :hook
  (after-init-hook . electric-pair-mode))

(use-package rainbow-delimiters
  :disabled
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  (cider-repl-mode-hook . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook
  (prog-mode-hook . highlight-parentheses-mode)
  (cider-repl-mode-hook . highlight-parentheses-mode)
  (minibuffer-setup-hook . highlight-parentheses-minibuffer-setup))

(use-package rainbow-mode
  :general
  (-leader-def
    "tr" 'rainbow-mode)
  :hook
  (css-mode-hook . rainbow-mode))

(use-package whitespace
  :ensure nil
  :general
  (-leader-def
    "tw" 'whitespace-mode))

(use-package page-break-lines
  :hook
  (after-init-hook . global-page-break-lines-mode))

(use-package highlight-indent-guides
  :general
  (-leader-def
    "ti" 'highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive t))

(use-package hl-todo
  :init
  (setq hl-todo-highlight-punctuation ":")
  (setq hl-todo-keyword-faces '(("TODO"  . hl-todo)
                                ("FIXME" . hl-todo)))
  :hook
  (after-init-hook . global-hl-todo-mode))

(use-package hi-lock
  :ensure nil
  :general
  (-leader-def
    "/h" '(:ignore t :wh "highlight")
    "/h." 'highlight-symbol-at-point
    "/hp" 'highlight-phrase
    "/hr" 'highlight-regexp
    "/hl" 'highlight-lines-matching-regexp
    "/hu" 'unhighlight-regexp))

(use-package color-identifiers-mode
  :general
  (-leader-def
    "tc" 'color-identifiers-mode))

(use-package prism
  :general
  (-leader-def
    "tp" 'prism-mode))

(use-package display-line-numbers
  :ensure nil
  :general
  (-leader-def
    "tn" 'display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start t))

(use-package company
  :general
  ("M-S-SPC" 'company-complete)
  :custom-face
  (company-tooltip-selection ((t :inverse-video t)))
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.3)
  (setq company-selection-wrap-around t)
  :hook
  (after-init-hook . global-company-mode))

(use-package company-shell
  :after company
  :init
  (add-to-list 'company-backends 'company-shell))

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package anzu
  :init
  (setq anzu-cons-mode-line-p nil)
  :hook
  (after-init-hook . global-anzu-mode))

(use-package evil-anzu
  :demand
  :after anzu)

(use-package hideshow
  :ensure nil
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package ispell
  :if (executable-find "hunspell")
  :ensure nil
  :init
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-really-aspell nil)
  (setq ispell-really-hunspell t)
  (setq ispell-dictionary "ru_RU,en_US")
  :config
  (setq ispell-program-name "hunspell")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US"))

(use-package flyspell
  :general
  (-leader-def
    "ts" 'flyspell-mode)
  (flyspell-mode-map
   "C-," nil
   "C-." nil
   "C-c $" nil)
  :init
  (setq flyspell-delay 1)
  (setq flyspell-use-meta-tab nil)
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-prog-text-faces '(;; font-lock-string-face
                                   font-lock-comment-face
                                   font-lock-doc-face))
  :hook
  ;; (text-mode-hook . flyspell-mode)
  ;; (org-mode-hook . flyspell-mode)
  ;; (prog-mode-hook . flyspell-prog-mode)
  (git-commit-mode-hook . flyspell-mode))

(use-package flyspell-correct
  :general
  (flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper))

(use-package flycheck
  :init
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-temp-prefix ".flycheck")
  :hook
  (prog-mode-hook . flycheck-mode)
  :config
  (when (display-graphic-p)
    (define-fringe-bitmap '-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000100
              #b00001100
              #b00011100
              #b00111100
              #b00011100
              #b00001100
              #b00000100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000))

    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap '-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap '-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap '-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info)))

(use-package consult-flycheck
  :general
  (-leader-def
    "je" 'consult-flycheck))

(use-package imenu
  :ensure nil
  :general
  (-leader-def
    "ji" 'imenu))

(use-package avy
  :general
  (-leader-def
    "jc" 'avy-goto-char
    "jw" 'avy-goto-word-0
    "jW" 'avy-goto-word-1
    "jl" 'avy-goto-line
    "jL" 'avy-goto-end-of-line)
  :init
  (setq avy-background t))

(use-package link-hint
  :general
  (-leader-def
    "ol" 'link-hint-open-link))

(use-package dumb-jump
  :preface
  (defhydra hydra-dumb-jump
    (:color blue :columns 3)
    ("j" dumb-jump-go "go")
    ("o" dumb-jump-go-other-window "other window")
    ("e" dumb-jump-go-prefer-external "go external")
    ("x" dumb-jump-go-prefer-external-other-window "go external other window")
    ("i" dumb-jump-go-prompt "prompt")
    ("l" dumb-jump-quick-look "quick look")
    ("b" dumb-jump-back "back"))
  :general
  (-leader-def
    "jj" '(hydra-dumb-jump/body :wk "hydra-dumb-jump"))
  :init
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package treemacs
  :preface
  (defun -setup-treemacs-theme ()
    (treemacs-create-theme "Icons"
      :config
      (progn
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2))
         :extensions (root-open))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2))
         :extensions (root-closed))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-directory" :v-adjust 0))
         :extensions (dir-open))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-directory" :v-adjust 0))
         :extensions (dir-closed))

        (treemacs-create-icon
         :icon (format "  %s " (all-the-icons-octicon "tag" :v-adjust 0))
         :extensions (tag-leaf))
        (treemacs-create-icon
         :icon (format "%s %s "
                       (all-the-icons-octicon "chevron-down" :v-adjust 0)
                       (all-the-icons-octicon "tag" :v-adjust 0))
         :extensions (tag-open))
        (treemacs-create-icon
         :icon (format "%s %s "
                       (all-the-icons-octicon "chevron-right" :v-adjust 0)
                       (all-the-icons-octicon "tag" :v-adjust 0))
         :extensions (tag-closed))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "alert" :v-adjust 0 :face 'error))
         :extensions (error))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "stop"  :v-adjust 0 :face 'warning))
         :extensions (warning))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "info"  :v-adjust 0 :face 'success))
         :extensions (info))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-text" :v-adjust 0))
         :extensions ("md" "markdown" "rst" "log" "org" "txt"
                      "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-zip" :v-adjust 0))
         :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"
                      "xz" "dmg" "iso"))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-binary" :v-adjust 0))
         :extensions ("exe" "dll" "obj" "so" "o" "out" "elc"))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-pdf" :v-adjust 0))
         :extensions ("pdf"))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-media" :v-adjust 0))
         :extensions ("png" "jpg" "jpeg" "gif" "ico" "svg" "bmp"
                      "mov" "avi" "mp4" "webm" "mkv"
                      "wav" "mp3" "ogg" "midi"))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-octicon "file-code" :v-adjust 0))
         :extensions (fallback))))

    (treemacs-load-theme "Icons"))
  :general
  (-leader-def
    "0" 'treemacs-select-window
    "ft" 'treemacs)
  :custom-face
  (treemacs-root-face ((t :inherit font-lock-constant-face :bold t :height 1.1)))
  :init
  (setq treemacs-follow-after-init t)
  (setq treemacs-space-between-root-nodes nil)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  :hook
  (treemacs-mode-hook . hide-mode-line-mode)
  (treemacs-mode-hook . -disable-evil-cursor)
  :config
  (-setup-treemacs-theme))

(use-package treemacs-fringe-indicator
  :ensure treemacs
  :after treemacs
  :config
  (treemacs-fringe-indicator-mode -1))

(use-package treemacs-evil
  :demand
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode-hook . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-tab-bar
  :after treemacs tab-bar
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package em-smart
  :ensure nil
  :after eshell
  :config
  (eshell-smart-initialize))

(use-package eshell-fringe-status
  :hook
  (eshell-mode-hook . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :after eshell
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'epe-theme-lambda))

(use-package vterm
  :preface
  (defun -vterm ()
    (interactive)
    (let ((default-directory "~"))
      (if (get-buffer "vterm")
          (switch-to-buffer "vterm")
        (vterm))))
  :general
  (-leader-def
    "ot" '-vterm)
  :init
  (setq vterm-max-scrollback 10000)
  (setq vterm-clear-scrollback-when-clearing t)
  :hook
  (vterm-mode-hook . -disable-global-hl-line-mode))

(use-package eshell-toggle
  :preface
  (defun -eshell-toggle-init-vterm (dir)
    (let ((default-directory dir))
      (vterm)))
  :general
  ("§" 'eshell-toggle)
  :init
  (setq eshell-toggle-init-function '-eshell-toggle-init-vterm)
  (setq eshell-toggle-use-projectile-root t)
  (setq eshell-toggle-run-command nil))

(use-package magit
  :commands magit-blame
  :preface
  (defun -magit-status ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  :general
  (-leader-def
    "g." 'magit-dispatch
    "gI" 'magit-init
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit-status
    "gl" '-magit-status
    "gL" 'magit-log-buffer-file)
  :init
  (setq magit-clone-default-directory "~/Projects")
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories `((,user-emacs-directory . 0)
                                       (,magit-clone-default-directory . 1))))

(use-package magit-todos
  :init
  (setq magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")" ":")))
  :hook
  (magit-mode-hook . magit-todos-mode))

(use-package git-timemachine
  :general
  (-leader-def
    "gt" 'git-timemachine))

(use-package gitignore-templates
  :general
  (-leader-def
    "gi" 'gitignore-templates-new-file)
  (-local-leader-def :keymaps 'gitignore-mode-map
    "i" 'gitignore-templates-insert))

(use-package git-modes)

(use-package diff-hl
  :init
  (setq diff-hl-draw-borders nil)
  :hook
  (after-init-hook         . global-diff-hl-mode)
  (after-init-hook         . diff-hl-margin-mode)
  ;; (diff-hl-mode-hook       . diff-hl-flydiff-mode)
  (dired-mode-hook         . diff-hl-dired-mode)
  (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package org
  :ensure nil
  :preface
  (defun -open-org-directory  () (interactive) (find-file org-directory))
  (defun -open-org-inbox-file () (interactive) (find-file -org-inbox-file))
  (defun -open-org-todo-file  () (interactive) (find-file -org-todo-file))
  (defun -open-org-notes-file () (interactive) (find-file -org-notes-file))
  :general
  (-leader-def
    "O." '(-open-org-directory  :wk "open org-directory")
    "Oi" '(-open-org-inbox-file :wk "open inbox.org")
    "Ot" '(-open-org-todo-file  :wk "open todo.org")
    "On" '(-open-org-notes-file :wk "open notes.org"))
  (-local-leader-def
    "i" 'org-insert-structure-template)
  :init
  (setq org-directory "~/Org")
  (setq -org-inbox-file (concat org-directory "/inbox.org"))
  (setq -org-todo-file  (concat org-directory "/todo.org"))
  (setq -org-notes-file (concat org-directory "/notes.org"))

  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content t)
  (setq org-hide-leading-stars t)

  (setq org-agenda-files `(,-org-todo-file))
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-skip-unavailable-files t)

  (setq org-archive-location (concat org-directory "/archive.org::datetree/"))

  (setq org-tags-column 0)
  ;; (setq org-ellipsis "…")
  (setq org-ellipsis " ⌄ ")
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts '{})

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

  (setq org-startup-with-inline-images t)

  (setq org-catch-invisible-edits 'smart)

  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline nil))

(use-package org-archive
  :ensure org
  :init
  (setq org-archive-file-header-format nil))

(use-package org-refile
  :ensure org
  :init
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t))

(use-package org-src
  :ensure org
  :init
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-content-indentation 0))

(use-package org-list
  :ensure org
  :init
  (setq org-list-allow-alphabetical t)
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))))

(use-package org-agenda
  :ensure org
  :general
  (-leader-def
    "Oa" '(org-agenda :wk "agenda"))
  :init
  (setq org-agenda-window-setup 'current-window))

(use-package org-face
  :ensure org
  :custom-face
  (org-tag              ((t :inherit shadow)))
  (org-ellipsis         ((t :underline nil)))
  (org-block-begin-line ((t :underline nil)))
  (org-block-end-line   ((t :overline nil)))
  :init
  (setq org-priority-faces
        '((?A . (:inherit (bold error)))
          (?B . (:inherit (bold warning)))
          (?C . (:inherit (bold success)))))
  (setq org-todo-keyword-faces
        '(("STARTED"   . (:inherit (bold font-lock-constant-face org-todo)))
          ("NEXT"      . (:inherit (bold font-lock-constant-face org-todo)))
          ("WAITING"   . (:inherit (bold warning org-todo)))
          ("HOLD"      . (:inherit (bold warning org-todo)))
          ("OBSOLETE"  . (:inherit (bold shadow org-todo)))
          ("CANCELLED" . (:inherit (bold shadow org-todo))))))

(use-package org-bullets
  :after org
  :init
  (setq org-bullets-bullet-list '("•"))
  (setq org-bullets--keywords
        `(("^\\*+ "
           (0 (let* ((level (- (match-end 0) (match-beginning 0) 1)))
                (compose-region (- (match-end 0) 2)
                                (- (match-end 0) 1)
                                (org-bullets-level-char level))
                (dolist (n (number-sequence
                            (match-beginning 0)
                            (- (match-end 0) 3)))
                  (compose-region n (+ n 1) " "))
                (put-text-property (match-beginning 0)
                                   (- (match-end 0) 2)
                                   'face (list :inherit 'org-hide))
                nil)))))
  :hook
  (org-mode-hook . org-bullets-mode))

(use-package toc-org
  :hook
  (org-mode-hook . toc-org-enable))

(use-package ob-core
  :ensure org
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images))

(use-package ob-emacs-lisp
  :ensure org
  :commands
  org-babel-execute:emacs-lisp
  org-babel-expand-body:emacs-lisp)

(use-package ob-shell
  :ensure org
  :commands
  org-babel-execute:sh
  org-babel-expand-body:sh
  org-babel-execute:bash
  org-babel-expand-body:bash)

(use-package ob-async
  :demand
  :after ob-core)

(use-package lsp-mode
  :init
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration))

(use-package lsp-ui)

(use-package lsp-treemacs
  :config
  ;; https://github.com/emacs-lsp/lsp-treemacs/issues/89
  (with-eval-after-load 'lsp-treemacs
    (-setup-treemacs-theme)))

(use-package dap-mode
  :general
  (lsp-command-map
   "D" 'dap-hydra)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package tree-sitter
  :hook
  (after-init-hook . global-tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package ts-fold
  :ensure nil
  :quelpa (ts-fold :fetcher github :repo "jcs090218/ts-fold")
  :hook
  (tree-sitter-after-on-hook . ts-fold-mode))

(use-package highlight-defined
  :init
  (setq highlight-defined-face-use-itself t)
  :hook
  (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package erefactor
  :general
  (-local-leader-def :keymaps 'emacs-lisp-mode-map
    "R" '(:keymap erefactor-map :wk "refactor")))

(use-package eros
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(use-package clojure-mode-extra-font-locking)

(use-package clj-refactor
  :pin melpa-stable
  :general
  (-local-leader-def :keymaps 'clojure-mode-map
    "R" '(hydra-cljr-help-menu/body :wk "refactor"))
  :hook
  (clojure-mode-hook . clj-refactor-mode))

(use-package eldoc
  :ensure nil
  :hook
  (clojure-mode-hook . eldoc-mode)
  (cider-repl-mode-hook . eldoc-mode))

(use-package cider
  :general
  (-local-leader-def :keymaps 'clojure-mode-map
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
  (cider-mode-hook      . cider-company-enable-fuzzy-completion)
  (cider-repl-mode-hook . cider-company-enable-fuzzy-completion))

(use-package cider-hydra
  :general
  (-local-leader-def :keymaps 'clojure-mode-map
    "d" '(cider-hydra-doc/body  :wk "doc")
    "e" '(cider-hydra-eval/body :wk "eval")
    "t" '(cider-hydra-test/body :wk "test")
    "r" '(cider-hydra-repl/body :wk "repl"))
  :hook
  (clojure-mode-hook . cider-hydra-mode))

(use-package lsp-java
  :after cc-mode
  :hook
  (java-mode-hook . lsp-deferred))

(use-package lsp-java-boot
  :ensure lsp-java
  :hook
  (lsp-mode-hook . lsp-lens-mode)
  (java-mode-hook . lsp-java-boot-lens-mode))

(use-package dap-java
  :ensure nil
  :after lsp-java)

(use-package go-mode
  :preface
  (defun -setup-go-mode ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook
  (go-mode-hook . lsp-deferred)
  (go-mode-hook . -setup-go-mode))

(use-package makefile-executor
  :general
  (-local-leader-def :keymaps 'makefile-mode-map
    "e" '(:ignore t :wk "eval")
    "ee" '(makefile-executor-execute-target :wk "execute")
    "eb" '(makefile-executor-execute-target :wk "execute in dedicated buffer")
    "el" '(makefile-executor-execute-target :wk "execute last"))
  :hook
  (makefile-mode-hook . makefile-executor-mode))

(use-package js-mode
  :ensure nil
  :hook
  (js-mode-hook . lsp-deferred))

(use-package web-mode
  :mode "\\.html?\\'"
  :init
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-current-element-highlight t))

(use-package plantuml-mode
  :general
  (-local-leader-def :keymaps 'plantuml-mode-map
    "p" '(plantuml-preview :wk "preview"))
  :init
  (setq plantuml-output-type (if (display-images-p) "png" "txt"))
  (setq plantuml-default-exec-mode 'executable))

(use-package ob-plantuml
  :ensure nil
  :after ob-core
  :commands
  org-babel-execute:plantuml)

(use-package sql
  :ensure nil
  :general
  (-local-leader-def :keymaps 'sql-mode-map
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

(use-package groovy-mode)

(use-package markdown-mode
  :custom-face
  (markdown-code-face ((t :inherit default)))
  :general
  (-local-leader-def :keymaps 'markdown-mode-map
    "." '(:keymap markdown-mode-command-map))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package grip-mode
  :general
  (-local-leader-def :keymaps 'markdown-mode-map
    "g" 'grip-mode)
  :init
  (setq grip-update-after-change nil)
  (setq grip-preview-use-webkit t))

(use-package markdown-toc)

(use-package edit-indirect)

(use-package json-mode
  :preface
  (defun -setup-json-mode ()
    (setq flycheck-checker 'json-jq
          js-indent-level 2))
  :general
  (-local-leader-def :keymaps 'json-mode-map
    "=" '(json-pretty-print-buffer :wk "format"))
  :hook
  (json-mode-hook . -setup-json-mode))

(use-package yaml-mode
  :mode "Procfile\\'"
  :hook
  (yaml-mode-hook . flycheck-mode)
  (yaml-mode-hook . highlight-indent-guides-mode))

(use-package flycheck-yamllint
  :hook
  (yaml-mode-hook . flycheck-yamllint-setup))

(use-package lua-mode
  :init
  (setq lua-indent-level 2))

(use-package sh-script
  :preface
  (defun -setup-sh-mode ()
    (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p t t))
  :hook
  (sh-mode-hook . -setup-sh-mode))

(use-package flymake-shellcheck
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

(use-package vimrc-mode)

(use-package ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package editorconfig
  :hook
  (after-init-hook . editorconfig-mode))

(use-package docker
  :general
  (-leader-def
    "od" 'docker))

(use-package dockerfile-mode
  :general
  (-local-leader-def :keymaps 'dockerfile-mode-map
    "b" 'dockerfile-build-buffer
    "B" 'dockerfile-build-no-cache-buffer))

(use-package docker-compose-mode
  :general
  (-local-leader-def :keymaps 'docker-compose-mode-map
    "." 'docker-compose))

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package company-ansible
  :after company yaml-mode
  :init
  (add-to-list 'company-backends 'company-ansible))

(use-package ansible-vault-with-editor
  :ensure nil
  :quelpa
  (ansible-vault-with-editor
   :fetcher github
   :repo "rynffoll/ansible-vault-with-editor")
  :general
  (-local-leader-def :keymaps 'yaml-mode-map
    "e" '(ansible-vault-with-editor-edit :wk "edit")
    "E" '(ansible-vault-with-editor-encrypt :wk "encrypt")
    "D" '(ansible-vault-with-editor-decrypt :wk "decrypt")))

(use-package verb
  :general
  (org-mode-map
   "C-c C-r" '(:keymap verb-command-map :package verb :wk "verb"))
  :init
  (setq verb-auto-kill-response-buffers t)
  (setq verb-json-use-mode 'json-mode))

(use-package ob-verb
  :ensure verb
  :after ob-core
  :commands
  org-babel-execute:verb)

(use-package direnv
  :if (executable-find "direnv")
  :preface
  (defun -direnv-hook ()
    (add-hook
     'after-save-hook
     (lambda ()
       (call-interactively 'direnv-update-environment))
     nil t))
  :general
  (-local-leader-def :keymaps 'direnv-envrc-mode-map
    "a" 'direnv-allow
    "u" 'direnv-update-environment)
  :init
  (setq direnv-always-show-summary nil)
  :hook
  (after-init-hook . direnv-mode)
  (direnv-envrc-mode-hook . -direnv-hook))

(use-package olivetti
  :general
  (-leader-def
    "to" 'olivetti-mode))

(use-package crux
  :general
  (-leader-def
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer))

(use-package deadgrep
  :general
  (-leader-def
    "/D" 'deadgrep))

(use-package try
  :general
  (-leader-def
    "Pt" 'try))

(use-package password-generator)

(use-package string-inflection)

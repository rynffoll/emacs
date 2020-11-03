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
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance"))

(use-package auto-compile
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-use-mode-line nil)
  :hook
  (emacs-lisp-mode-hook . auto-compile-on-load-mode)
  (emacs-lisp-mode-hook . auto-compile-on-save-mode))

(use-package emacs
  :ensure nil
  :custom
  ;; https://github.com/emacs-evil/evil-surround/issues/107#issuecomment-306117178
  (major-mode 'text-mode)
  (load-prefer-newer t)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil "Don't use tabs")
  (create-lockfiles nil "Stop creating .# files")
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (inhibit-compacting-font-caches t)
  (scroll-step 1)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (ring-bell-function 'ignore)
  (delete-by-moving-to-trash t)
  (read-process-output-max (* 1024 1024))
  (bidi-inhibit-bpa t)
  (bidi-display-reordering 'left-to-right)
  (bidi-paragraph-direction 'left-to-right)
  (fast-but-imprecise-scrolling t)
  :hook
  (focus-out-hook . garbage-collect)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package mule
  :ensure nil
  :custom
  (default-input-method 'russian-computer)
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package emacs
  :ensure nil
  :custom
  (buffer-file-coding-system 'utf-8))

(use-package select
  :ensure nil
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package calendar
  :ensure nil
  :custom
  (calendar-date-style 'iso)
  (calendar-week-start-day 1))

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
  :general
  (evil-insert-state-map
   "C-k" nil)
  (-leader-def
    "j[" 'evil-jump-backward
    "j]" 'evil-jump-forward)
  :custom
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-emacs-state-cursor 'hbar)
  (evil-mode-line-format nil)
  (evil-symbol-word-search t)
  (evil-move-beyond-eol nil)
  (evil-move-cursor-back t)
  (evil-undo-system 'undo-tree)
  (evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" '-save-and-kill-buffer))

(use-package evil-collection
  :demand
  :after evil
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :hook
  (after-init-hook . evil-commentary-mode))

(use-package evil-magit
  :demand
  :after magit
  :custom
  (evil-magit-want-horizontal-movement t)
  (evil-magit-use-z-for-folds t))

(use-package evil-surround
  :hook
  (after-init-hook . global-evil-surround-mode))

(use-package evil-matchit
  :hook
  (after-init-hook . global-evil-matchit-mode))

(use-package evil-org
  :custom
  (evil-org-key-theme '(todo textobjects insert navigation heading))
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
  :custom
  ;; Allow C-h to trigger which-key before it is done automatically
  (which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :hook
  (after-init-hook . which-key-mode))

(use-package char-fold
  :ensure nil
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :general
  (evil-normal-state-map "C-х" 'evil-force-normal-state)
  (evil-insert-state-map "C-х" 'evil-normal-state)
  (evil-visual-state-map "C-х" 'evil-exit-visual-state)
  :custom
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-exclude)
  (reverse-im-input-methods '("russian-computer"))
  :hook
  (after-init-hook . reverse-im-mode))

(use-package xt-mouse
  :unless (display-graphic-p)
  :ensure nil
  :hook
  (after-init-hook . xterm-mouse-mode))

(use-package startup
  :ensure nil
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message nil))

(tooltip-mode -1)
(menu-bar-mode -1)

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

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
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-modal-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  :hook
  (after-init-hook . doom-modeline-mode)
  :config
  (dolist (name '("*Compile-Log*"))
    (when-let ((buffer (get-buffer name)))
      (with-current-buffer buffer
        (doom-modeline-set-main-modeline)))))

(use-package solarized-theme
  :custom
  (solarized-distinct-doc-face t)
  (solarized-use-variable-pitch nil)
  (solarized-scale-org-headlines nil)
  (solarized-scale-outline-headlines nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  :init
  (load-theme 'solarized-gruvbox-dark t))

(use-package paradox
  :general
  (-leader-def
    "Pl" 'paradox-list-packages
    "PU" 'paradox-upgrade-packages)
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t "Don't ask github token"))

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
    (let* ((current-tab-index (1+ (tab-bar--current-tab-index)))
           (tab-names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
           (separator (propertize "|" 'face '(shadow)))
           (active-face '(font-lock-constant-face :inverse-video t))
           (inactivate-face '(shadow))
           (tabs (mapconcat
                  (lambda (name)
                    (let* ((index (1+ (tab-bar--tab-index-by-name name)))
                           (name-with-index (format " %d:%s " index name))
                           (active? (= index current-tab-index))
                           (face (if active? active-face inactivate-face)))
                      (propertize name-with-index 'face face)))
                  tab-names separator)))
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
  :custom
  (tab-bar-tab-hints t)
  ;; (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-post-open-functions #'-tab-bar-post-open-rename)
  :config
  (advice-add #'tab-bar-select-tab :after #'-tab-bar-print-tabs)
  (advice-add #'tab-close          :after #'-tab-bar-print-tabs)
  (advice-add #'tab-close-other    :after #'-tab-bar-print-tabs))

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
  :custom
  (winner-dont-bind-my-keys t "Unbind C-right/C-left")
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
  :custom
  (winum-auto-setup-mode-line nil "For spaceline")
  (winum-scope 'frame-local)
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
  :defer 2
  :custom
  (uniquify-buffer-name-style 'forward))

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
  :custom
  (shackle-default-size 0.3)
  (shackle-rules '((help-mode :align below :select t)
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
                   ("\\*docker-compose .*\\*" :regexp t :align below)))
  :hook
  (after-init-hook . shackle-mode))

(use-package ivy
  :general
  (ivy-mode-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line)
  (-leader-def
    "bb" 'ivy-switch-buffer)
  :custom
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'full)
  (ivy-on-del-error-function nil)
  (ivy-use-selectable-prompt t)
  (ivy-initial-inputs-alist nil "Remove ^ from prompt")
  (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                           (swiper     . ivy--regex-plus)
                           (t          . ivy--regex-fuzzy)))
  :hook
  (after-init-hook . ivy-mode))

(use-package ivy-hydra)

(use-package ivy-rich
  :custom
  (ivy-rich-path-style 'abbrev)
  :hook
  (ivy-mode-hook . ivy-rich-mode))

(use-package counsel
  :general
  ([remap describe-face]            'counsel-describe-face)
  ([remap describe-function]        'counsel-describe-function)
  ([remap describe-variable]        'counsel-describe-variable)
  ([remap execute-extended-command] 'counsel-M-x)
  ([remap find-file]                'counsel-find-file)
  ([remap find-library]             'counsel-find-library)
  ([remap imenu]                    'counsel-imenu)
  (-leader-def
    "." 'counsel-find-file

    "oL" 'counsel-find-library
    "oh" 'counsel-command-history

    "Pp" 'counsel-package

    "ff" 'counsel-find-file
    "fr" 'counsel-recentf

    "/b" 'swiper
    "/d" 'counsel-rg

    "tt" 'counsel-load-theme

    "hF" '(:ignore t :wk "face")
    "hFf" 'counsel-faces
    "hFe" 'counsel-colors-emacs
    "hFw" 'counsel-colors-web)
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable))

(use-package counsel-projectile
  :general
  (-leader-def
    "/p" 'counsel-projectile-rg)
  :hook
  (after-init-hook . counsel-projectile-mode))

(use-package amx
  :custom
  (amx-backend 'ivy))

(use-package files
  :ensure nil
  :general
  (-leader-def
    "br" 'revert-buffer)
  :custom
  (require-final-newline t)
  (make-backup-files nil "Stop creating backup~ files")
  (auto-save-default nil "Stop creating #autosave# files")
  (enable-local-variables :all)
  (enable-local-eval t))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-check-vc-info t)
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
  :custom
  (recentf-max-saved-items 300)
  :hook
  (after-init-hook . recentf-mode))

(use-package files
  :if (eq system-type 'darwin)
  :ensure nil
  :custom
  (insert-directory-program "gls")
  (trash-directory "~/.Trash/emacs"))

(use-package iqa
  :general
  (-leader-def
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "er" 'iqa-reload-user-init-file)
  :custom
  (iqa-user-init-file (concat user-emacs-directory "config.org")))

(use-package cus-edit
  :ensure nil
  :general
  (-leader-def
    "oc" 'customize-group)
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package epg-config
  :ensure nil
  :custom
  (epg-pinentry-mode 'loopback))

(use-package projectile
  :general
  (-leader-def
    "p" '(:keymap projectile-command-map :package projectile :wk "project"))
  :custom
  (projectile-project-search-path '("~/Projects"))
  (projectile-auto-discover nil)
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always "Never prompt for recursive copies of a directory")
  (dired-recursive-deletes 'always "Never prompt for recursive deletes of a directory")
  (dired-hide-details-hide-symlink-targets nil)
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
  :custom
  (dired-subtree-use-backgrounds nil)
  :config
  ;; for treemacs-icons-dired
  (advice-add #'dired-subtree-toggle :after #'-dired-subtree-revert))

(use-package pack
  :general
  (:keymaps 'dired-mode-map :states 'normal
            "P" 'pack-dired-dwim)
  :custom
  (pack-dired-default-extension ".zip"))

(use-package dired-git-info
  :general
  (:keymaps 'dired-mode-map :states 'normal
            ")" 'dired-git-info-mode))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package exec-path-from-shell
  :demand
  :custom
  (exec-path-from-shell-arguments '("-l") "remove -i")
  (exec-path-from-shell-variables '("PATH" "MANPATH" "LANG"))
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
    "hd" 'describe-mode))

(use-package help-fns
  :ensure nil
  :general
  (-leader-def
    "hf" 'describe-function
    "hv" 'describe-variable))

(use-package man
  :ensure nil
  :general
  (-leader-def
    "hM" 'man))

(use-package helpful
  :general
  (-leader-def
    "h." 'helpful-at-point
    "hC" 'helpful-command
    "hc" 'helpful-callable
    "hk" 'helpful-key
    "hm" 'helpful-macro))

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
    "SPC" 'execute-extended-command
    ":" 'eval-expression
    "tT" 'toggle-truncate-lines)
  :custom
  (backward-delete-char-untabify-method 'hungry)
  (async-shell-command-buffer 'new-buffer)
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

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer-hook . show-all)
  (ediff-quit-hook . winner-undo))

(use-package undo-tree
  :if (eq evil-undo-system 'undo-tree)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,temporary-file-directory)))
  :hook
  (after-init-hook . global-undo-tree-mode))

(use-package undo-fu
  :if (eq evil-undo-system 'undo-fu))

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
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  (cider-repl-mode-hook . rainbow-delimiters-mode))

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
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive t))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces '(("TODO"  . hl-todo)
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

(use-package display-line-numbers
  :ensure nil
  :general
  (-leader-def
    "tn" 'display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start t))

(use-package company
  :general
  ("M-S-SPC" 'company-complete)
  :custom-face
  (company-tooltip-selection ((t :inverse-video t)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.3)
  (company-selection-wrap-around t)
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
  :custom
  (anzu-cons-mode-line-p nil)
  :hook
  (after-init-hook . global-anzu-mode))

(use-package evil-anzu
  :after anzu)

(use-package hideshow
  :ensure nil
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package ispell
  :if (executable-find "hunspell")
  :ensure nil
  :custom
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-dictionary "ru_RU,en_US")
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
  :custom
  (flyspell-delay 1)
  (flyspell-use-meta-tab nil)
  (flyspell-issue-message-flag nil)
  (flyspell-prog-text-faces '(;; font-lock-string-face
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
   "C-;" 'flyspell-correct-at-point))

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :custom
  (flyspell-correct-interface 'flyspell-correct-ivy))

(use-package flycheck
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-temp-prefix ".flycheck")
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
  :custom
  (avy-background t))

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
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg))

(use-package treemacs
  :preface
  (defun -hide-fringes ()
    (when (display-graphic-p)
      (set-window-fringes nil 0 0)))
  :general
  (-leader-def
    "0" 'treemacs-select-window
    "ft" 'treemacs)
  :custom-face
  (treemacs-root-face ((t :inherit font-lock-constant-face :bold t :height 1.1)))
  :custom
  (treemacs-collapse-dirs (if (executable-find "python") 3 0))
  (treemacs-follow-after-init t)
  (treemacs-no-delete-other-windows nil)
  (treemacs-space-between-root-nodes nil)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-after-tag-follow 'on-distance)
  :hook
  (treemacs-mode-hook . hide-mode-line-mode)
  (treemacs-mode-hook . -hide-fringes)
  :config
  (treemacs-create-theme "Icons"
    :config
    (progn
      (treemacs-create-icon
       :icon (format "%s " (all-the-icons-octicon "repo" :v-adjust -0.1 :height 1.2))
       :extensions (root))

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

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode-hook . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

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
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

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
  :custom
  (vterm-max-scrollback 10000)
  :hook
  (vterm-mode-hook . -disable-global-hl-line-mode))

(use-package eshell-toggle
  :preface
  (defun -eshell-toggle-init-vterm (dir)
    (let ((default-directory dir))
      (vterm)))
  :general
  ("§" 'eshell-toggle)
  :custom
  (eshell-toggle-init-function '-eshell-toggle-init-vterm)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil))

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
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-clone-default-directory "~/Projects")
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories `((,user-emacs-directory . 0)
                                  (,magit-clone-default-directory . 1))))

(use-package magit-todos
  :custom
  (magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")" ":")))
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

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package diff-hl
  :disabled
  :custom
  (diff-hl-draw-borders nil)
  :hook
  (after-init-hook         . global-diff-hl-mode)
  (after-init-hook         . diff-hl-margin-mode)
  (diff-hl-mode-hook       . diff-hl-flydiff-mode)
  (dired-mode-hook         . diff-hl-dired-mode)
  (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package alert :disabled)

(use-package alert
  :disabled
  :if (eq window-system 'ns)
  :preface
  (defun -osx-notification (info)
    (let* ((title   (substring-no-properties (plist-get info :title)))
           (message (substring-no-properties (plist-get info :message)))
           (script  (format "display notification %S with title %S"
                            message title)))
      (do-applescript script))
    (alert-message-notify info))
  :custom
  (alert-default-style 'osx-notification)
  :config
  (alert-define-style 'osx-notification
                      :title "AppleScript notification"
                      :notifier #'-osx-notification))

(use-package appt
  :disabled
  :ensure nil
  :preface
  (defun -appt-alert (min-to-app new-time appt-msg)
    (let ((min-to-app (if (listp min-to-app) min-to-app `(,min-to-app)))
          (appt-msg   (if (listp appt-msg)   appt-msg   `(,appt-msg))))
      (dotimes (i (length appt-msg))
        (let* ((min-to-app (nth i min-to-app))
               (appt-msg   (nth i appt-msg))
               (title (format "Appointment in %s minutes" min-to-app)))
          (alert appt-msg :title title)))))
  :custom
  (appt-time-msg-list nil)
  (appt-message-warning-time 15)
  (appt-display-interval 5)
  (appt-display-mode-line nil)
  (appt-disp-window-function (if (display-graphic-p)
                                 #'-appt-alert
                               #'appt-disp-window))
  (appt-audible nil)
  (appt-display-diary nil)
  :config
  (appt-activate t))

(use-package org
  :ensure nil
  :preface
  (defun -open-org-directory  () (interactive) (find-file org-directory))
  (defun -open-org-inbox-file () (interactive) (find-file -org-inbox-file))
  (defun -open-org-todo-file  () (interactive) (find-file -org-todo-file))
  (defun -open-org-work-file  () (interactive) (find-file -org-work-file))
  (defun -open-org-notes-file () (interactive) (find-file -org-notes-file))
  :general
  (-leader-def
    "O." '(-open-org-directory  :wk "open org-directory")
    "Oi" '(-open-org-inbox-file :wk "open inbox.org")
    "Ot" '(-open-org-todo-file  :wk "open todo.org")
    "Ow" '(-open-org-work-file  :wk "open work.org")
    "On" '(-open-org-notes-file :wk "open notes.org"))
  (-local-leader-def
    "i" 'org-insert-structure-template)
  :custom
  (org-directory "~/Org")
  (-org-inbox-file (concat org-directory "/inbox.org"))
  (-org-todo-file  (concat org-directory "/todo.org"))
  (-org-work-file  (concat org-directory "/work.org"))
  (-org-notes-file (concat org-directory "/notes.org"))

  (org-startup-indented t)
  (org-insert-heading-respect-content t)
  (org-hide-leading-stars t)
  (org-hide-leading-stars-before-indent-mode t)

  (org-agenda-files `(,-org-inbox-file ,-org-todo-file ,-org-work-file))
  (org-agenda-inhibit-startup t)
  (org-agenda-skip-unavailable-files t)

  (org-archive-location (concat org-directory "/old/archive.org" "::* From %s"))

  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-cache t)

  (org-tags-column 0)
  (org-ellipsis "…")
  (org-pretty-entities t)
  (org-use-sub-superscripts '{} "Require {} for sub/super scripts")

  (org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-log-done 'time)

  (org-startup-with-inline-images t)

  (org-catch-invisible-edits 'smart)

  (org-fontify-whole-heading-line t))

(use-package org-src
  :ensure nil
  :custom
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0))

(use-package org-list
  :ensure nil
  :custom
  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))))

(use-package org-agenda
  :ensure nil
  :general
  (-leader-def
    "Oa" '(org-agenda :wk "agenda"))
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-deadline-if-done (bound-and-true-p appt-timer))
  (org-agenda-skip-scheduled-if-done (bound-and-true-p appt-timer))
  :hook
  (org-agenda-finalize-hook . org-agenda-to-appt))

(when (bound-and-true-p appt-timer)
  (run-at-time "10 sec" (* 10 60) 'org-agenda-to-appt))

(use-package org-face
  :ensure nil
  :custom-face
  (org-tag ((t :inherit shadow)))
  (org-ellipsis ((t :underline nil)))
  (org-block-begin-line ((t :underline nil)))
  (org-block-end-line ((t :overline nil)))
  (org-level-1 ((t :weight bold)))
  (org-level-2 ((t :weight bold)))
  (org-level-3 ((t :weight bold)))
  (org-level-4 ((t :weight bold)))
  (org-level-5 ((t :weight bold)))
  (org-level-6 ((t :weight bold)))
  (org-level-7 ((t :weight bold)))
  (org-level-8 ((t :weight bold)))
  :custom
  (org-priority-faces '((?A . (:inherit error :weight bold))
                        (?B . (:inherit warning :weight bold))
                        (?C . (:inherit success :weight bold)))))

(use-package org-bullets
  :after org
  :custom
  (org-bullets-bullet-list '("•"))
  (org-bullets--keywords
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
  :ensure nil
  :custom
  (org-babel-load-languages
   '((emacs-lisp . t)
     (scheme     . t)
     (shell      . t)
     (plantuml   . t)))
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images))

(use-package ob-async
  :demand
  :after ob-core)

(use-package lsp-mode
  :custom
  (lsp-keep-workspace-alive nil)
  (lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration))

(use-package lsp-ui)

(use-package lsp-ivy
  :general
  (lsp-command-map
   "i" 'lsp-ivy-workspace-symbol
   "I" 'lsp-ivy-global-workspace-symbol))

(use-package dap-mode
  :general
  (lsp-command-map
   "D" 'dap-hydra)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package highlight-defined
  :custom
  (highlight-defined-face-use-itself t)
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
  :custom
  (web-mode-enable-block-face t)
  (web-mode-enable-part-face t)
  (web-mode-enable-comment-interpolation t)
  (web-mode-enable-current-element-highlight t))

(use-package plantuml-mode
  :general
  (-local-leader-def :keymaps 'plantuml-mode-map
    "p" '(plantuml-preview :wk "preview"))
  :custom
  (plantuml-output-type (if (display-images-p) "png" "txt"))
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path
   (car (last (file-expand-wildcards
               "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))))

(use-package flycheck-plantuml
  :hook
  (plantuml-mode-hook . flycheck-plantuml-setup))

(use-package ob-plantuml
  :ensure nil
  :after ob-core
  :custom
  (org-plantuml-jar-path
   (car (last (file-expand-wildcards
               "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))))

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
  :custom
  (sql-connection-alist '((pg-local
                           (sql-product 'postgres)
                           (sql-port 5432)
                           (sql-server "localhost")
                           (sql-user "postgres")
                           (sql-password "postgres")
                           (sql-database "postgres")))))

(use-package groovy-mode)

(use-package markdown-mode
  :general
  (-local-leader-def :keymaps 'markdown-mode-map
    "p" 'markdown-preview)
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package grip-mode
  :general
  (-local-leader-def :keymaps 'markdown-mode-map
    "g" 'grip-mode))

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
  :custom
  (lua-indent-level 2))

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

(use-package mu4e
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :general
  (-leader-def
    "om" 'mu4e)
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-drafts-folder "/[Gmail]/Drafts")
  (mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (mu4e-trash-folder  "/[Gmail]/Trash")
  (mu4e-refile-folder nil)
  ;; (mu4e-sent-messages-behavior 'delete)
  (mu4e-sent-messages-behavior 'sent)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir-shortcuts
   `( ("/INBOX"            . ?i)
      (,mu4e-drafts-folder . ?d)
      (,mu4e-sent-folder   . ?s)
      (,mu4e-trash-folder  . ?t)))
  (mu4e-compose-signature-auto-include nil)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-use-fancy-chars nil)
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses t)
  (mu4e-view-prefer-html t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-attachment-dir "~/Downloads")
  :config
  (add-to-list 'mu4e-view-actions
               '("browser" . mu4e-action-view-in-browser))
  (add-to-list 'mu4e-view-actions
               '("xwidget" . mu4e-action-view-with-xwidget)))

(use-package message
  :ensure nil
  :custom
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :ensure nil
  :custom
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-debug-info t))

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

(use-package ansible-doc
  :general
  (-local-leader-def :keymaps 'yaml-mode-map
    "h" '(ansible-doc :wh "doc"))
  :hook
  (yaml-mode-hook . ansible-doc-mode)
  :config
  (evil-set-initial-state 'ansible-doc-module-mode 'motion))

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
  :custom
  (verb-auto-kill-response-buffers t)
  (verb-json-use-mode 'json-mode))

(use-package ob-verb
  :ensure verb
  :after ob-core
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((verb . t)))))

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
  :custom
  (direnv-always-show-summary nil)
  :hook
  (after-init-hook . direnv-mode)
  (direnv-envrc-mode-hook . -direnv-hook))

(use-package password-generator
  :general
  (-leader-def
    "ip" '(:ignore t :wk "password-generator")
    "ips" 'password-generator-simple
    "ipS" 'password-generator-strong
    "ipp" 'password-generator-paranoid
    "ipn" 'password-generator-numeric
    "ipP" 'password-generator-phonetic))

(use-package google-translate
  :general
  (-leader-def
    "ht" 'google-translate-at-point
    "hT" 'google-translate-at-point-reverse)
  :custom
  (google-translate-default-target-language "ru")
  (google-translate-default-source-language "en")
  (google-translate-pop-up-buffer-set-focus t)
  (google-translate-backend-method 'curl))

(use-package olivetti
  :general
  (-leader-def
    "to" 'olivetti-mode)
  :custom
  (olivetti-body-width 100))

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

(use-package string-inflection)

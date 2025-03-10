;;; init.el --- Initialization -*- lexical-binding: t; no-byte-compile: t -*-

(setq user-full-name "Ruslan Kamashev"
      user-login-name "rynffoll"
      user-mail-address "rynffoll@gmail.com")

(setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "https://melpa.org/packages/")))

(package-initialize)

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-hook-name-suffix nil)
(setq use-package-enable-imenu-support t)
(setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)

(use-package gnu-elpa-keyring-update)

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

(use-package compile
  :ensure nil
  :init
  (setq compilation-scroll-output 'first-error))

(use-package ansi-color
  :ensure nil
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))

(use-package time
  :ensure nil
  :init
  (setq world-clock-time-format "%a %d %b %R %z")
  (setq world-clock-list
        '(("America/Mexico_City" "Mexico/Mexico City")
          ("UTC" "UTC")
          ("Europe/Madrid" "Spain/Madrid")
          ("Europe/Moscow" "Russia/Moscow")
          ("Asia/Nicosia" "Cyprus/Nicosia")
          ("Asia/Tbilisi" "Georgia/Tbilisi")
          ("Asia/Yerevan" "Armenia/Yerevan")
          ("Asia/Almaty" "Kazakhstan/Almaty"))))

(use-package calendar
  :ensure nil
  :init
  (setq calendar-date-style 'iso)
  (setq calendar-week-start-day 1))

(use-package gcmh
  :hook
  (emacs-startup-hook . gcmh-mode))

(use-package async
  :hook
  (after-init-hook . async-bytecomp-package-mode)
  (dired-mode-hook . dired-async-mode))

(use-package general
  :config
  (general-create-definer +leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-S-SPC")
  (general-create-definer +local-leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-,")
  (general-define-key
   :states '(normal visual)
   "," (general-simulate-key "SPC m" :which-key "local leader"))
  (+leader-def
    ""    '(nil :wk "leader")
    "a"   '(:ignore t :wk "assistant")
    "o"   '(:ignore t :wk "open")
    "O"   '(:ignore t :wk "org")
    "p"   '(:ignore t :wk "project")
    "P"   '(:ignore t :wk "package")
    "F"   '(:ignore t :wk "frame")
    "TAB" '(:ignore t :wk "tab")
    "b"   '(:ignore t :wk "buffer")
	"S"   '(:ignore t :wk "session")
    "f"   '(:ignore t :wk "file")
    "e"   '(:ignore t :wk "emacs")
    "g"   '(:ignore t :wk "git")
    "/"   '(:ignore t :wk "search")
    "j"   '(:ignore t :wk "jump")
    "h"   '(:ignore t :wk "help")
    "t"   '(:ignore t :wk "toggle")
    "i"   '(:ignore t :wk "insert")
    "q"   '(:ignore t :wk "quit"))
  (+local-leader-def
    ""    '(nil :wk "local leader")))

(use-package evil
  :demand
  :preface
  (defun +save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-buffer))
  (defun +disable-evil-cursor ()
    (setq-local evil-default-cursor    '(nil))
    (setq-local evil-motion-state-cursor nil)
    (setq-local evil-visual-state-cursor nil)
    (setq-local evil-normal-state-cursor nil)
    (setq-local evil-insert-state-cursor nil)
    (setq-local evil-emacs-state-cursor  nil))
  :general
  (evil-insert-state-map
   "C-k" nil)
  (+leader-def
    "j[" 'evil-jump-backward
    "j]" 'evil-jump-forward)
  :custom-face
  (evil-ex-substitute-matches
   ((t (:inherit diff-removed :foreground unspecified :background unspecified :strike-through t))))
  (evil-ex-substitute-replacement
   ((t (:inherit diff-added :foreground unspecified :background unspecified :underline nil))))
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
  (setq evil-want-C-i-jump nil)
  :config
  (evil-ex-define-cmd "q"  'kill-current-buffer)
  (evil-ex-define-cmd "wq" '+save-and-kill-buffer)
  (evil-mode t))

(use-package evil-collection
  :demand
  :after evil
  :init
  (setq evil-collection-magit-want-horizontal-movement t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :hook
  (after-init-hook . evil-commentary-mode))

(use-package evil-surround
  :hook
  (after-init-hook . global-evil-surround-mode))

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

(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :init
  (setq etcc-use-color t)
  (setq etcc-use-blink nil)
  :hook
  (after-init-hook . evil-terminal-cursor-changer-activate))

(use-package which-key
  :ensure nil
  :init
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-dont-use-unicode nil)
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

(use-package pixel-scroll
  :disabled
  :ensure nil
  :hook
  (after-init-hook . pixel-scroll-precision-mode))

(use-package ultra-scroll
  :if (display-graphic-p)
  :vc (:url "https://github.com/jdtsmith/ultra-scroll" :rev :newest)
  :init
  (setq scroll-conservatively 101) ;; important!
  (setq scroll-margin 0)
  :hook
  (after-init-hook . ultra-scroll-mode))

(use-package ligature
  :if (display-graphic-p)
  :config
  ;; https://github.com/mickeynp/ligature.el/wiki
  (cond
   ((s-contains? "JetBrains Mono" +font)
    (ligature-set-ligatures
     'prog-mode
     '("--" "---" "==" "===" "!=" "!==" "=!="
       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
       "<:<" ";;;")))
   ((s-contains? "Iosevka" +font)
    (ligature-set-ligatures
     'prog-mode
     '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))
   ((s-matches? "\\(Cascadia\\|Fira Code\\)" +font)
    (ligature-set-ligatures
     'prog-mode
     '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
       ;; =:= =!=
       ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
       ;; ;; ;;;
       (";" (rx (+ ";")))
       ;; && &&&
       ("&" (rx (+ "&")))
       ;; !! !!! !. !: !!. != !== !~
       ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
       ;; ?? ??? ?:  ?=  ?.
       ("?" (rx (or ":" "=" "\." (+ "?"))))
       ;; %% %%%
       ("%" (rx (+ "%")))
       ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
       ;; |->>-||-<<-| |- |== ||=||
       ;; |==>>==<<==<=>==//==/=!==:===>
       ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                       "-" "=" ))))
       ;; \\ \\\ \/
       ("\\" (rx (or "/" (+ "\\"))))
       ;; ++ +++ ++++ +>
       ("+" (rx (or ">" (+ "+"))))
       ;; :: ::: :::: :> :< := :// ::=
       (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
       ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
       ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                       "="))))
       ;; .. ... .... .= .- .? ..= ..<
       ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
       ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
       ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
       ;; *> */ *)  ** *** ****
       ("*" (rx (or ">" "/" ")" (+ "*"))))
       ;; www wwww
       ("w" (rx (+ "w")))
       ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
       ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
       ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
       ;; << <<< <<<<
       ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                       "-"  "/" "|" "="))))
       ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
       ;; >> >>> >>>>
       (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
       ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
       ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                    (+ "#"))))
       ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
       ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
       ;; __ ___ ____ _|_ __|____|_
       ("_" (rx (+ (or "_" "|"))))
       ;; Fira code: 0xFF 0x12
       ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
       ;; Fira code:
       "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
       ;; The few not covered by the regexps.
       "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))
   (t (message "No ligatures for %s" +font)))
  :hook
  (after-init-hook . global-ligature-mode))

(defvar +with-icons nil)

(use-package nerd-icons
  :if +with-icons
  :init
  (setq nerd-icons-color-icons t)
  :config
  (when (and (display-graphic-p)
             (not (member "Symbols Nerd Font Mono" (font-family-list))))
    (nerd-icons-install-fonts)))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t (:inherit mode-line :box nil :underline nil :overline nil))))
  (mode-line-inactive ((t (:inherit mode-line-inactive :box nil :underline nil :overline nil)))))

(use-package hide-mode-line)

(use-package minions
  :hook
  (after-init-hook . minions-mode))

(use-package doom-modeline
  :init
  ;; (setq doom-modeline-bar-width 2)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-icon +with-icons)
  ;; (setq doom-modeline-modal-icon t)
  (setq doom-modeline-buffer-encoding nil)
  ;; (setq doom-modeline-major-mode-icon t)
  ;; (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-workspace-name nil)
  (setq doom-modeline-check-icon nil)
  (setq doom-modeline-check-simple-format t)
  (setq doom-modeline-always-show-macro-register t)
  (setq doom-modeline-support-imenu t)
  :hook
  (after-init-hook . doom-modeline-mode))

(use-package custom
  :ensure nil
  :general
  (+leader-def
    "tt" 'load-theme))

(use-package modus-themes
  ;; :ensure nil
  :init
  (setq modus-themes-common-palette-overrides
        '(;; (bg-region bg-cyan-intense)
          (fg-region unspecified)
          (bg-prose-block-delimiter bg-inactive)
          (fg-prose-block-delimiter fg-dim)
          (bg-prose-block-contents bg-dim)
          (fringe unspecified))))

(use-package ef-themes)

(use-package standard-themes)

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
  (setq solarized-height-plus-4 1.0))

(use-package doom-themes
  :init
  (setq doom-themes-enable-italic t)
  :config
  (doom-themes-org-config))

(setq +theme 'modus-operandi)
;; (setq +theme 'ef-melissa-light)
;; (setq +theme 'solarized-gruvbox-dark)
;; (setq +theme 'doom-earl-grey)

(load-theme +theme :no-confirm)

(use-package frame
  :ensure nil
  :general
  (+leader-def
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
  (+leader-def
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
  (defun +tab-bar-select-tab-1 () (interactive) (tab-bar-select-tab 1))
  (defun +tab-bar-select-tab-2 () (interactive) (tab-bar-select-tab 2))
  (defun +tab-bar-select-tab-3 () (interactive) (tab-bar-select-tab 3))
  (defun +tab-bar-select-tab-4 () (interactive) (tab-bar-select-tab 4))
  (defun +tab-bar-select-tab-5 () (interactive) (tab-bar-select-tab 5))
  (defun +tab-bar-select-tab-6 () (interactive) (tab-bar-select-tab 6))
  (defun +tab-bar-select-tab-7 () (interactive) (tab-bar-select-tab 7))
  (defun +tab-bar-select-tab-8 () (interactive) (tab-bar-select-tab 8))
  (defun +tab-bar-select-tab-9 () (interactive) (tab-bar-switch-to-last-tab))
  :general
  (+leader-def
    "TAB" '(:keymap tab-prefix-map :wk "tab-bar"))
  (tab-prefix-map
   "TAB" 'tab-bar-switch-to-recent-tab
   "0" nil
   "1" '+tab-bar-select-tab-1
   "2" '+tab-bar-select-tab-2
   "3" '+tab-bar-select-tab-3
   "4" '+tab-bar-select-tab-4
   "5" '+tab-bar-select-tab-5
   "6" '+tab-bar-select-tab-6
   "7" '+tab-bar-select-tab-7
   "8" '+tab-bar-select-tab-8
   "9" '+tab-bar-select-tab-9
   "." 'tab-bar-select-tab-by-name
   "n" 'tab-new
   "[" 'tab-previous
   "]" 'tab-next
   ">" 'tab-bar-move-tab
   "<" 'tab-bar-move-tab-backward
   "c" 'tab-close
   "C" 'tab-close-other)
  :init
  ;; (setq tab-bar-show 1)
  (setq tab-bar-show t)
  (setq tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-separator))
  ;; (setq tab-bar-format '(tab-bar-format-menu-bar
  ;;                        tab-bar-format-tabs-groups
  ;;                        tab-bar-separator
  ;;                        tab-bar-format-add-tab))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-tab-hints t)
  (setq tab-bar-separator " ") ;; the same behavior in GUI and TUI
  ;; (setq tab-bar-separator (propertize "│" 'face '(vertical-border)))
  :hook
  (after-init-hook . tab-bar-mode)
  (after-init-hook . tab-bar-history-mode))

(use-package tab-bar-theme
  :ensure nil
  :load-path "site-lisp/tab-bar-theme"
  :init
  (setq tab-bar-theme-height 5)
  :hook
  (after-init-hook . tab-bar-theme-mode))

(use-package project-tab-groups
  :hook
  (after-init-hook . project-tab-groups-mode))

(use-package tab-line
  :ensure nil
  :init
  (setq tab-line-close-button-show nil)
  (setq tab-line-new-button-show nil))

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
  :general
  (+leader-def
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
  :hook
  (after-init-hook . winum-mode))

(use-package zoom
  :general
  (evil-window-map
   "z" 'zoom-mode))

(use-package shackle
  ;; :disabled
  :init
  (setq shackle-default-size 0.4)
  (setq shackle-rules
        '((help-mode :align below :select t)
          (helpful-mode :align below)
          (flycheck-error-list-mode :align below)
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
          (go-test-mode :align below)))
  :hook
  (after-init-hook . shackle-mode))

(use-package popper
  :disabled
  :general
  ("C-`"   'popper-toggle-latest)
  ("C-§"   'popper-toggle-latest)
  ;; ("M-`"   'popper-cycle)
  ;; ("M-~"   'popper-cycle-backwards)
  ("C-M-`" 'popper-toggle-type)
  ("C-M-§" 'popper-toggle-type)
  :init
  (setq popper-mode-line '(:eval (propertize " POP " 'face '(region bold))))
  (setq popper-display-control nil) ;; for shackle
  (setq popper-window-height 0.3)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*[Wo]Man.*\\*$"
          ;; "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "\\*vc-git : .*"
          
          help-mode
          helpful-mode
          
          compilation-mode
          comint-mode
          
          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode
          flycheck-verify-mode
          
          cider-repl-mode
          ansible-doc-module-mode))
  :hook
  (after-init-hook . popper-mode))

(use-package popper-echo
  :disabled
  :ensure popper
  :init
  (setq popper-echo-dispatch-actions t)
  (setq popper-echo-lines 3)
  :hook
  (after-init-hook . popper-echo-mode)
  ;; (after-init-hook . popper-tab-line-mode)
  )

(use-package emacs
  :ensure nil
  :preface
  (defun +switch-to-scratch  () (interactive) (switch-to-buffer "*scratch*"))
  (defun +switch-to-messages () (interactive) (switch-to-buffer "*Messages*"))
  :general
  (+leader-def
    "bs" '+switch-to-scratch
    "bm" '+switch-to-messages
    "bR" 'rename-buffer))

(use-package simple
  :ensure nil
  :general
  (+leader-def
    "bk" 'kill-current-buffer))

(use-package menu-bar
  :ensure nil
  :general
  (+leader-def
    "tde" 'toggle-debug-on-error
    "tdq" 'toggle-debug-on-quit))

(use-package window
  :ensure nil
  :general
  (+leader-def
    "bb" 'switch-to-buffer
    "bK" 'kill-buffer-and-window))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package evil-commands
  :ensure evil
  :after evil
  :general
  (+leader-def
    "bn" 'evil-buffer-new
    "b]" 'evil-next-buffer
    "b[" 'evil-prev-buffer))

(use-package ibuffer
  :ensure nil
  :general
  ([remap list-buffers] 'ibuffer)
  (+leader-def
    "bl" 'list-buffers
    "bi" 'ibuffer)
  :init
  (setq ibuffer-human-readable-size t) ;; emacs 31
  )

(use-package ibuffer-vc
  :disabled ;; replaced by projection-ibuffer
  :preface
  (defun +setup-ibuffer-vc ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  (ibuffer-hook . +setup-ibuffer-vc))

;; not only icons, but also other customizations (e.g. human-readable size, colors, etc.)
(use-package nerd-icons-ibuffer
  ;; :if +with-icons
  :init
  (setq nerd-icons-ibuffer-icon +with-icons)
  :hook
  (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package persistent-scratch
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
  :general
  (+leader-def
    "fr" 'recentf-open-files)
  :init
  (setq recentf-max-saved-items 300)
  :hook
  (after-init-hook . recentf-mode))

(use-package desktop
  ;; :disabled
  :ensure nil
  :general
  (+leader-def
    "Ss" 'desktop-save-in-desktop-dir
    "Sr" 'desktop-read)
  :init
  (setq desktop-path `(,user-emacs-directory))
  :config
  (dolist (mode '(git-commit-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  :hook
  (after-init-hook . desktop-save-mode))

(use-package easysession
  :disabled
  :preface
  (defun +easysession-load-ask ()
    (interactive)
    (when (y-or-n-p "Restore previous session?")
      (easysession-load)))
  ;; FIXME: hack to restore tab-bar
  (defun +easysession-restore-tab-bar ()
    (when (cdr (funcall tab-bar-tabs-function))
      (let ((tab-bar-show t))
        (tab-bar-mode +1))))
  :general
  (+leader-def
    "Ss" 'easysession-save
    "Sr" 'easysession-load)
  :init
  (setq easysession-save-interval (* 10 60))
  ;; (add-hook 'emacs-startup-hook #'easysession-load 102)
  (add-hook 'emacs-startup-hook #'+easysession-load-ask 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 102)
  :hook
  (easysession-after-load-hook . +easysession-restore-tab-bar))

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

(use-package nerd-icons-completion
  :if +with-icons
  :hook
  (vertico-mode-hook    . nerd-icons-completion-mode)
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

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
  (+leader-def
    "/." 'consult-ripgrep
    "/b" 'consult-line)
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

(use-package consult-dir
  :general
  ([remap list-directory] 'consult-dir))

(use-package marginalia
  :general
  ( :keymaps 'minibuffer-local-map
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

(use-package vertico-posframe
  :disabled ;; by performance reasons (try again in emacs 31 with feature `tty-child-frames')
  :init
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  :hook
  (vertico-mode-hook . vertico-posframe-mode))

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

(use-package corfu-terminal
  :if (< emacs-major-version 31)
  :unless (featurep 'tty-child-frames)
  :unless (display-graphic-p)
  :hook
  (corfu-mode-hook . corfu-terminal-mode))

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
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; Complete word from current buffers.
  (add-to-list 'completion-at-point-functions #'cape-file) ;; Complete file name.
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;; Complete Elisp in Org or Markdown code block.
  )

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
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :general
  (+leader-def
    "it" 'tempel-insert)
  ( :keymaps 'tempel-map
    "TAB" 'tempel-next)
  :hook
  (conf-mode-hook . tempel-setup-capf)
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf))

(use-package tempel-collection)

(use-package files
  :ensure nil
  :preface
  (defun +find-file-in-dir (dir)
    "Open a file starting in DIR."
    (interactive "DDirectory: ")
    (let ((default-directory (file-name-as-directory dir)))
      (call-interactively #'find-file)))
  :general
  (+leader-def
    "."  'find-file
    "br" 'revert-buffer
    "eR" 'restart-emacs)
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

(use-package iqa
  :preface
  ;; for integration with project-tab-groups
  (defun +iqa-find-file-project (file)
    (let* ((dir (file-name-directory file))
           (default-directory dir))
      (project-current t)
      (find-file file)))
  :general
  (+leader-def
    "ed" 'iqa-find-user-init-directory
    "ee" 'iqa-find-user-init-file
    "ec" 'iqa-find-user-custom-file
    "er" 'iqa-reload-user-init-file)
  :init
  (setq iqa-find-file-function #'+iqa-find-file-project)
  (setq iqa-user-init-file (locate-user-emacs-file "config.org")))

(use-package cus-edit
  :ensure nil
  :general
  (+leader-def
    "oc" 'customize-group))

(use-package epg-config
  :ensure nil
  :init
  (setq epg-pinentry-mode 'loopback))

(use-package project
  :ensure nil
  :general
  (+leader-def
    "p" '(:keymap project-prefix-map :package project :wk "project"))
  ( :keymaps 'project-prefix-map
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

(use-package project-vterm
  :ensure nil
  :load-path "site-lisp/project-vterm"
  :general
  ( :keymaps 'project-prefix-map
    "t" 'project-vterm)
  :config
  (add-to-list 'project-switch-commands '(project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode)))

(use-package disproject
  :general
  ( :keymaps 'project-prefix-map
    "." 'disproject-dispatch))

(use-package projection
  :general
  ( :keymaps 'project-prefix-map
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

(use-package projection-ibuffer
  :ensure projection
  :after ibuffer
  :demand t
  :preface
  (defun +projection-ibuffer-setup ()
    (setq ibuffer-filter-groups (projection-ibuffer--filter-groups))
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  (ibuffer-hook . +projection-ibuffer-setup))

(use-package projection-multi
  :general
  ( :keymaps 'project-prefix-map
    "RET" 'projection-multi-compile))

(use-package projection-multi-embark
  :after embark
  :after projection-multi
  :demand t
  :config
  (projection-multi-embark-setup-command-map))

(use-package project-butler
  :after project
  :demand
  :general
  ( :keymaps 'project-prefix-map
    "K" 'project-butler-cleanup)
  :config
  (add-to-list
   'project-butler-projects-list
   `(,user-emacs-directory . ("" ("config.org"))))
  (add-to-list
   'project-butler-projects-list
   `(,(file-name-as-directory org-directory)
     . ("1|2" (,+org-notes-file ,+org-todo-file)))))

(use-package dired
  :ensure nil
  :init
  (setq dired-listing-switches
        (concat
         "-l "                        ;; long listing (dired requires this)
         "--almost-all "              ;; show hidden files, but not . or ..
         "--no-group "                ;; do not show group, only owner
         ;; conflict w/ `dired-sidebar-follow-file'
         ;; "--classify "                ;; append indicator (one of */=>@|) to entries
         "--human-readable "          ;; print sizes in human readable format
         "--sort=version "            ;; sort by version number (netural order)
         "--group-directories-first " ;; group directories first
         "--time-style=long-iso"      ;; use ISO 8601 date format (YYYY-MM-DD HH:MM)
         ))
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)
  (setq dired-free-space nil)
  (setq dired-hide-details-hide-absolute-location t) ;; emacs 31
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
  :general
  ( :keymaps 'dired-mode-map :states 'normal
    "TAB" 'dired-subtree-toggle)
  :init
  (setq dired-subtree-use-backgrounds nil))

(use-package diredfl
  :custom-face
  (diredfl-dir-name ((t (:bold t))))
  :hook
  (dired-mode-hook . diredfl-mode))

(use-package nerd-icons-dired
  ;; :disabled  ;; replaced by nerd-icons-multimodal
  :if +with-icons
  :config
  ;; WORKAROUND: display transparent background of icons
  ;; https://github.com/rainstormstudio/nerd-icons-dired/issues/1#issuecomment-2628680359
  (defun +nerd-icons-dired--add-overlay (pos string)
    "Add overlay to display STRING at POS."
    (let ((ov (make-overlay (1- pos) pos)))
      (overlay-put ov 'nerd-icons-dired-overlay t)
      (overlay-put ov 'after-string
                   (propertize "_" 'display string))))
  (advice-add #'nerd-icons-dired--add-overlay :override #'+nerd-icons-dired--add-overlay)
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

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

(use-package dired-sidebar
  :autoload dired-sidebar-showing-sidebar-p
  :preface
  (defun +dired-sidebar-follow-file ()
    (interactive)
    (if (dired-sidebar-showing-sidebar-p)
        (dired-sidebar-follow-file)
      (dired-sidebar-jump-to-sidebar)))
  :general
  (+leader-def
    "0" 'dired-sidebar-jump-to-sidebar
    "ft" 'dired-sidebar-toggle-sidebar
    "ff" '+dired-sidebar-follow-file)
  :init
  (setq dired-sidebar-theme (if +with-icons 'nerd-icons 'none))
  ;; (setq dired-sidebar-use-custom-modeline nil)
  (setq dired-sidebar-use-custom-modeline t)
  (setq dired-sidebar-mode-line-format nil) ;; hide mode-line
  (setq dired-sidebar-no-delete-other-windows t)
  (setq dired-sidebar-toggle-hidden-commands nil) ;; don't hide on `balance-windows'
  ;; (setq dired-sidebar-window-fixed nil)
  (setq dired-sidebar-use-custom-font t) ;; to custom `dired-sidebar-face'
  (setq dired-sidebar-face '(:height 0.9))
  (setq dired-sidebar-refresh-on-project-switch nil)
  :config
  (with-eval-after-load 'winum
    (defun winum-assign-0-to-dired-sidebar ()
      (when (and (eq major-mode 'dired-sidebar-mode)
                 (eq (selected-window) (frame-first-window)))
        0))
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-dired-sidebar)))

;; Back to `quelpa' because `package-vc' doesn't support keyword like `:files'
(use-package quelpa-use-package
  :disabled
  :demand
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t))

(use-package dirvish
  :disabled
  ;; :vc (:url "https://github.com/hlissner/dirvish" :rev :newest)
  :ensure nil
  :quelpa (dirvish
           :fetcher github
           :repo "hlissner/dirvish"
           :files ("*.el" "extensions/*.el"))
  :preface
  (defun winum-assign-0-to-dirvish-side ()
    (when (and (functionp 'dirvish-side--session-visible-p)
               (eq (selected-window) (dirvish-side--session-visible-p))
               (eq (selected-window) (frame-first-window)))
      0))
  (defun +dired--init-fringes (dir buffer setup)
    (when (bound-and-true-p diff-hl-dired-mode)
      (set-window-fringes nil 8 1)))
  :general
  (+leader-def
    "0" 'dirvish-side
    "ft" 'dirvish-side
    "fd" 'drivish)
  ;; TODO: + evil-collection
  ( :keymaps 'dirvish-mode-map :states 'normal
    "?"   'dirvish-dispatch
    "q"   'dirvish-quit
    "b"   'dirvish-quick-access
    "f"   'dirvish-file-info-menu
    "p"   'dirvish-yank
    "S"   'dirvish-quicksort
    "F"   'dirvish-layout-toggle
    "z"   'dirvish-history-jump
    "gh"  'dirvish-subtree-up
    "gl"  'dirvish-subtree-toggle
    "TAB" 'dirvish-subtree-toggle
    "h"   'dired-up-directory
    "l"   'dired-find-file
    "[h"  'dirvish-history-go-backward
    "]h"  'dirvish-history-go-forward
    "[e"  'dirvish-emerge-next-group
    "]e"  'dirvish-emerge-previous-group
    "M-e" 'dirvish-emerge-menu
    "M-n" 'dirvish-narrow
    "M-m" 'dirvish-mark-menu
    "M-s" 'dirvish-setup-menu
    "y"    '(:ignore t :wk "yank")
    "yl"   'dirvish-copy-file-true-path
    "yn"   'dirvish-copy-file-name
    "yp"   'dirvish-copy-file-path
    "yr"   'dirvish-copy-remote-path
    "yy"   'dired-do-copy
    "s"    '(:ignore t :wk "symlinks")
    "ss"   'dirvish-symlink
    "sS"   'dirvish-relative-symlink
    "sh"   'dirvish-hardlink)
  :custom-face
  (dirvish-hl-line ((t (:inherit hl-line))))
  :init
  ;; (setq dirvish-mode-line-height   20) ;; see `doom-modeline-height'
  ;; (setq dirvish-header-line-height 20) ;; see `doom-modeline-height'
  ;; (setq dirvish-attributes '(vc-state)) ;; back to `diff-hl-dir-mode'
  (setq dirvish-attributes nil)
  (setq dirvish-path-separators '("  ~" "  " "/"))
  ;; (setq dirvish-reuse-session nil)
  (setq dirvish-subtree-prefix "  ")
  :config
  (with-eval-after-load 'doom-modeline
    (setq dirvish-mode-line-height   doom-modeline-height)
    (setq dirvish-header-line-height doom-modeline-height))
  (with-eval-after-load 'winum
    (add-to-list 'winum-assign-functions #'winum-assign-0-to-dirvish-side)
    ;; TODO: contribute to upstream
    (dirvish-define-mode-line winum
                              "A `winum-mode' indicator."
                              (and (bound-and-true-p winum-mode)
                                   (let ((num (winum-get-number-string)))
                                     (propertize (format " %s " num)
                                                 'face 'winum-face))))
    (setq dirvish-mode-line-format
          '( :left  (winum sort)
             :right (omit yank))))
  ;; https://github.com/doomemacs/doomemacs/blob/master/modules/emacs/dired/config.el#L109
  (advice-add 'dirvish-data-for-dir :before #'+dired--init-fringes)
  :hook
  (after-init-hook . dirvish-override-dired-mode))

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
  (+leader-def
    "h" '(:keymap help-map :package help :wk "help"))
  (help-map
   "F" 'describe-face))

(use-package helpful
  :general
  ([remap describe-command]             'helpful-command)
  ([remap describe-key]                 'helpful-key)
  ([remap describe-variable]            'helpful-variable)
  ([remap describe-function]            'helpful-callable)
  ([remap Info-goto-emacs-command-node] 'helpful-function)
  (+leader-def
    "h." 'helpful-at-point))

(use-package find-func
  :ensure nil
  :general
  (+leader-def
    "fl" 'find-library))

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
  :general
  (+leader-def
    ":" 'execute-extended-command
    "tT" 'toggle-truncate-lines)
  ;; :init ;; TODO: back to defaults
  ;; (setq backward-delete-char-untabify-method 'hungry)
  ;; (setq async-shell-command-buffer 'new-buffer)
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
  ;; :init ;; TODO: back to defaults
  ;; (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)
  :hook
  (after-init-hook . electric-pair-mode))

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
  :disabled
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `(("." . ,temporary-file-directory)))
  :hook
  (after-init-hook . global-undo-tree-mode))

(use-package undo-fu
  :disabled)

(use-package undo-fu-session
  :hook
  (org-mode-hook . undo-fu-session-mode))

(use-package vundo
  :general
  ("C-x u" 'vundo)
  :hook
  (vundo-mode-hook . +disable-global-hl-line-mode)
  (vundo-mode-hook . +disable-evil-cursor)
  :custom-face
  (vundo-highlight  ((t (:inherit success :foreground unspecified))))
  (vundo-last-saved ((t (:inherit error   :foreground unspecified))))
  (vundo-saved      ((t (:inherit warning :foreground unspecified))))
  :config
  (setq vundo-compact-display t)
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package hl-line
  :ensure nil
  :preface
  (defun +disable-global-hl-line-mode ()
    (setq-local global-hl-line-mode nil))
  :general
  (+leader-def
    "tl" 'global-hl-line-mode)
  :hook
  (after-init-hook . global-hl-line-mode))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  :hook
  (after-init-hook . show-paren-mode))

(use-package highlight-parentheses
  :disabled
  :hook
  (prog-mode-hook . highlight-parentheses-mode)
  (cider-repl-mode-hook . highlight-parentheses-mode)
  (minibuffer-setup-hook . highlight-parentheses-minibuffer-setup))

(use-package paren-face
  :hook
  (after-init-hook . global-paren-face-mode))

(use-package rainbow-mode
  :disabled
  :general
  (+leader-def
    "tr" 'rainbow-mode)
  :hook
  (css-mode-hook . rainbow-mode))

(use-package colorful-mode
  :general
  (+leader-def
    "tc" 'colorful-mode))

(use-package whitespace
  :ensure nil
  :general
  (+leader-def
    "tw" 'whitespace-mode))

(use-package page-break-lines
  :hook
  (after-init-hook . global-page-break-lines-mode))

(use-package highlight-indent-guides
  :general
  (+leader-def
    "ti" 'highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

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
  (+leader-def
    "/h" '(:ignore t :wh "highlight")
    "/h." 'highlight-symbol-at-point
    "/hp" 'highlight-phrase
    "/hr" 'highlight-regexp
    "/hl" 'highlight-lines-matching-regexp
    "/hu" 'unhighlight-regexp))

(use-package color-identifiers-mode
  :disabled
  :general
  (+leader-def
    "tc" 'color-identifiers-mode))

(use-package prism
  :disabled
  :general
  (+leader-def
    "tp" 'prism-mode))

(use-package display-line-numbers
  :ensure nil
  :general
  (+leader-def
    "tn" 'display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start t))

(use-package anzu
  :init
  (setq anzu-cons-mode-line-p nil)
  :hook
  (after-init-hook . global-anzu-mode))

(use-package evil-anzu
  :demand
  :after evil anzu)

(use-package outline
  :ensure nil
  ;; :init ;; TODO: back to defaults
  ;; (setq outline-blank-line t)
  )

(use-package hideshow
  :ensure nil
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package outline-indent
  :hook
  (yaml-ts-mode-hook . outline-indent-minor-mode))

(use-package ispell
  :disabled
  :if (executable-find "hunspell")
  :ensure nil
  :after flyspell
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
  :disabled
  :general
  (+leader-def
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
  :disabled
  :general
  (flyspell-mode-map
   "C-;" 'flyspell-correct-wrapper))

(use-package jinx
  :general
  (+leader-def
    "ts" 'jinx-mode)
  ([remap ispell-word] 'jinx-correct)
  :init
  (setq jinx-languages "ru en es")
  :hook
  (text-mode-hook       . jinx-mode)
  (org-mode-hook        . jinx-mode)
  ;; (prog-mode-hook       . jinx-mode)
  (git-commit-mode-hook . jinx-mode))

(use-package flycheck
  ;; :disabled
  :preface
  ;; https://www.flycheck.org/en/latest/user/error-reports.html#fringe-and-margin-icons
  (defun +flycheck-set-indication-mode ()
    (pcase flycheck-indication-mode
      (`left-margin
       (setq left-margin-width (max 1 left-margin-width)))
      (`right-margin
       (setq right-margin-width (max 1 right-margin-width))))
    (flycheck-refresh-fringes-and-margins))
  :init
  (setq flycheck-indication-mode (if (display-graphic-p)
                                     'right-fringe
                                   'right-margin))
  (setq flycheck-temp-prefix ".flycheck")
  :hook
  (after-init-hook . global-flycheck-mode)
  (flycheck-mode-hook . +flycheck-set-indication-mode)
  :config
  ;; (when (display-graphic-p)
  ;;   (define-fringe-bitmap '+flycheck-fringe-indicator
  ;;     (vector #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000100
  ;;             #b00001100
  ;;             #b00011100
  ;;             #b00111100
  ;;             #b00011100
  ;;             #b00001100
  ;;             #b00000100
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000
  ;;             #b00000000))

  ;;   (flycheck-define-error-level 'error
  ;;     :severity 2
  ;;     :overlay-category 'flycheck-error-overlay
  ;;     :fringe-bitmap '+flycheck-fringe-indicator
  ;;     :fringe-face 'flycheck-fringe-error)

  ;;   (flycheck-define-error-level 'warning
  ;;     :severity 1
  ;;     :overlay-category 'flycheck-warning-overlay
  ;;     :fringe-bitmap '+flycheck-fringe-indicator
  ;;     :fringe-face 'flycheck-fringe-warning)

  ;;   (flycheck-define-error-level 'info
  ;;     :severity 0
  ;;     :overlay-category 'flycheck-info-overlay
  ;;     :fringe-bitmap '+flycheck-fringe-indicator
  ;;     :fringe-face 'flycheck-fringe-info))
  (flycheck-redefine-standard-error-levels "!" 'exclamation-mark))

(use-package consult-flycheck
  :requires flycheck
  :general
  (+leader-def
    "je" 'consult-flycheck))

(use-package flymake
  :disabled ;; too slowly
  :ensure nil
  :init
  (setq flymake-fringe-indicator-position 'right-fringe)
  :hook
  (prog-mode-hook . flymake-mode))

(use-package flymake-collection
  :hook
  (after-init-hook . flymake-collection-hook-setup))

(use-package imenu
  :ensure nil
  :general
  (+leader-def
    "ji" 'imenu))

(use-package avy
  :general
  (+leader-def
    "jc" 'avy-goto-char
    "jw" 'avy-goto-word-0
    "jW" 'avy-goto-word-1
    "jl" 'avy-goto-line
    "jL" 'avy-goto-end-of-line)
  :init
  (setq avy-background t))

(use-package link-hint
  :general
  (+leader-def
    "ol" 'link-hint-open-link))

(use-package treemacs
  :disabled
  :general
  (+leader-def
    "0" 'treemacs-select-window
    "ft" 'treemacs)
  :init
  (setq treemacs-show-cursor t)
  (setq treemacs-follow-after-init t)
  (setq treemacs-space-between-root-nodes nil)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  (setq treemacs-no-png-images (not +with-icons))
  :hook
  (treemacs-mode-hook . hide-mode-line-mode)
  (treemacs-mode-hook . +disable-evil-cursor))

(use-package treemacs-theme
  :disabled
  :after treemacs
  :demand
  :ensure nil
  :load-path "site-lisp/treemacs-theme"
  :config
  (treemacs-theme-setup))

(use-package treemacs-fringe-indicator
  :disabled
  :ensure treemacs
  :after treemacs
  :config
  (treemacs-fringe-indicator-mode -1))

(use-package treemacs-evil
  :disabled
  :after treemacs evil)

(use-package treemacs-icons-dired
  :disabled
  :if (and +with-icons (display-graphic-p))
  :hook
  (dired-mode-hook . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :disabled
  :after treemacs magit)

(use-package treemacs-tab-bar
  :disabled
  :after treemacs tab-bar
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package treemacs-nerd-icons
  :disabled
  :if +with-icons
  :after treemacs
  :demand
  :config
  (treemacs-modify-theme "nerd-icons"
    :config
    (treemacs-create-icon
     :icon (format "%s%s%s%s"
                   treemacs-nerd-icons-tab
                   treemacs-nerd-icons-tab
                   (nerd-icons-faicon "nf-fa-folder"  :face 'treemacs-nerd-icons-file-face)
                   treemacs-nerd-icons-tab)
     :extensions (dir-closed dir-open)
     :fallback 'same-as-icon))
  (treemacs-load-theme "nerd-icons"))

(use-package vterm
  :preface
  (defun +vterm ()
    (interactive)
    (let ((default-directory "~"))
      (if (get-buffer "vterm")
          (switch-to-buffer "vterm")
        (vterm))))
  :general
  (+leader-def
    "ot" '+vterm)
  :init
  (setq vterm-shell "/opt/homebrew/bin/fish")
  (setq vterm-max-scrollback 10000)
  (setq vterm-set-bold-hightbright t)
  :config
  ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
  (advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))
  :hook
  (vterm-mode-hook . +disable-global-hl-line-mode)
  ;; (vterm-mode-hook . hide-mode-line-mode)
  )

(use-package magit
  :commands magit-blame
  :preface
  (defun +magit-status ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  :general
  (+leader-def
    "g." 'magit-dispatch
    "gI" 'magit-init
    "gb" 'magit-blame
    "gc" 'magit-clone
    "gg" 'magit-status
    "gl" '+magit-status
    "gL" 'magit-log-buffer-file)
  :init
  (setq magit-clone-default-directory "~/Projects/src/")
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-repository-directories `((,user-emacs-directory . 0)
                                       ("~/Projects/" . 2)))
  (setq magit-diff-refine-hunk t))

(use-package magit
  :if +with-icons
  :init
  (setq magit-format-file-function #'magit-format-file-nerd-icons))

(use-package magit-todos
  :init
  (setq magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")" ":")))
  :hook
  (magit-mode-hook . magit-todos-mode))

(use-package git-timemachine
  :general
  (+leader-def
    "gt" 'git-timemachine))

(use-package git-modes)

(use-package diff-hl
  :init
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-update-async t)
  :hook
  (after-init-hook         . global-diff-hl-mode)
  ;; (diff-hl-mode-hook       . diff-hl-flydiff-mode) ;; disabled by perf issues
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package diff-hl-dired
  ;; :disabled
  :ensure diff-hl
  :init
  (setq diff-hl-dired-extra-indicators nil)
  :config
  ;; FIXME: dirty hack to override bitmap functions (w/o icons inside)
  (defun diff-hl-dired-highlight-items (alist)
    "Highlight ALIST containing (FILE . TYPE) elements."
    (dolist (pair alist)
      (let ((file (car pair))
            (type (cdr pair)))
        (save-excursion
          (goto-char (point-min))
          (when (and type (dired-goto-file-1
                           file (expand-file-name file) nil))
            (let* (;; (diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
                   ;; (diff-hl-fringe-face-function 'diff-hl-dired-face-from-type)
                   (diff-hl-fringe-bmp-function '(lambda (_type pos) 'diff-hl-bmp-empty))
                   (o (diff-hl-add-highlighting type 'single)))
              (overlay-put o 'modification-hooks '(diff-hl-overlay-modified))
              (overlay-put o 'diff-hl-dired-type type)
              ))))))
  :hook
  (dired-mode-hook . diff-hl-dired-mode))

(use-package diff-hl-margin
  ;; :disabled
  :ensure diff-hl
  :unless (display-graphic-p)
  :init
  (setq diff-hl-margin-symbols-alist '((insert . " ")
                                       (delete . " ")
                                       (change . " ")
                                       (unknown . " ")))
  :hook
  (after-init-hook . diff-hl-margin-mode))

(use-package git-link
  :general
  (+leader-def
    "gL" 'git-link-dispatch))

(use-package org
  :ensure nil
  :preface
  (defun +find-file-in-org-directory ()
    (interactive)
    (+find-file-in-dir org-directory))
  (defun +open-org-inbox-file () (interactive) (find-file +org-inbox-file))
  (defun +open-org-todo-file  () (interactive) (find-file +org-todo-file))
  (defun +open-org-notes-file () (interactive) (find-file +org-notes-file))
  :general
  (+leader-def
    "O." '+find-file-in-org-directory
    "Oi" '+open-org-inbox-file
    "Ot" '+open-org-todo-file
    "On" '+open-org-notes-file)
  :init
  (setq org-directory "~/Org")
  (setq +org-inbox-file (concat org-directory "/inbox.org"))
  (setq +org-todo-file  (concat org-directory "/todo.org"))
  (setq +org-notes-file (concat org-directory "/notes.org"))

  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content t)
  (setq org-hide-leading-stars t)

  (setq org-agenda-files `(,+org-todo-file))
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-skip-unavailable-files t)

  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)

  (setq org-ellipsis "…")
  ;; (setq org-ellipsis " ⌄ ")
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
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

  (setq org-startup-with-inline-images t)

  (setq org-fold-catch-invisible-edits 'smart)

  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline nil)

  (setq org-imenu-depth 4))

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
  (setq org-refile-targets `((org-agenda-files  :maxlevel . 3)
                             (+org-files-list :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t))

(use-package org-link
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
  (setq org-edit-src-content-indentation 0))

(use-package org-list
  :ensure org
  :init
  (setq org-list-allow-alphabetical t)
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))))

(use-package org-agenda
  :ensure org
  :general
  (+leader-def
    "Oa" '(org-agenda :wk "agenda"))
  :init
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-tags-column 0))

(use-package org-faces
  :ensure org
  :custom-face
  (org-tag              ((t (:inherit shadow :foreground unspecified :background unspecified :bold nil))))
  (org-ellipsis         ((t (:underline nil))))
  (org-block-begin-line ((t (:underline nil))))
  (org-block-end-line   ((t (:overline nil))))
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

(use-package org-bullets
  :disabled
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

(use-package org-modern
  :disabled
  :init
  (setq org-modern-progress nil)
  (setq org-modern-tag nil)
  (setq org-modern-checkbox nil)
  (setq org-modern-keyword nil)
  (setq org-modern-todo nil)
  (setq org-modern-priority nil)
  (setq org-modern-list nil)
  (setq org-modern-table-horizontal 0.1)
  (setq org-modern-table-vertical 2)
  :hook
  (org-mode-hook . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda))

(use-package toc-org
  :init
  (setq toc-org-max-depth 4)
  :hook
  (org-mode-hook . toc-org-enable))

(use-package ob-core
  :ensure org
  :init
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (shell      . t)
          (plantuml   . t)))
  :hook
  (org-babel-after-execute-hook . org-redisplay-inline-images))

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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t))))

(use-package ob-chatgpt-shell
  :commands (org-babel-execute:chatgpt-shell)
  :config
  (ob-chatgpt-shell-setup))

(use-package org-crypt
  :ensure org
  :init
  (setq org-tags-exclude-from-inheritance '("crypt"))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)
  :config
  (org-crypt-use-before-save-magic))

(use-package org-appear
  :hook
  (org-mode-hook . org-appear-mode))

(use-package deft
  :general
  (+leader-def
    "Od" 'deft)
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

(use-package treesit
  :ensure nil
  :init
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :disabled
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  :hook
  (after-init-hook . global-treesit-auto-mode))

(use-package eglot
  :ensure nil
  :general
  (+local-leader-def :keymaps 'eglot-mode-map
    "=" 'eglot-format
    "a" '(:ignore t :wk "code")
    "a." 'eglot-code-actions
    "ao" 'eglot-code-action-organize-imports
    "ae" 'eglot-code-action-extract
    "ai" 'eglot-code-action-inline
    "aq" 'eglot-code-action-quickfix
    "R" '(:ignore t :wk "refactor")
    "Rr" 'eglot-rename
    "Re" 'eglot-code-action-extract
    "Ri" 'eglot-code-action-inline
    "F" '(:ignore t :wk "find")
    "Fd" 'eglot-find-declaration
    "Ft" 'eglot-find-typeDefinition
    "Fr" 'eglot-find-references
    "Fi" 'eglot-find-implementation)
  :init
  (setq eglot-autoshutdown t))

(use-package eglot-booster
  :if (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  ;; :after eglot
  :init
  (setq eglot-booster-no-remote-boost t)
  :hook
  (after-init-hook . eglot-booster-mode))

(use-package eglot-hierarchy
  :vc (:url "https://github.com/dolmens/eglot-hierarchy" :rev :newest)
  :general
  (+local-leader-def :keymaps 'eglot-mode-map
    "H" '(:ignore t :wk "hierarchy")
    "Hc" 'eglot-hierarchy-call-hierarchy
    "Ht" 'eglot-hierarchy-type-hierarchy))

(use-package consult-eglot
  :general
  (+local-leader-def :keymaps 'eglot-mode-map
    "Fs" 'consult-eglot-symbols))

(use-package consult-eglot-embark
  :config
  (consult-eglot-embark-mode))

(use-package flycheck-eglot
  :demand
  :after flycheck eglot
  :init
  (setq flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode))

(use-package dape
  :custom-face
  (dape-breakpoint-face ((t (:inherit error))))
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

;; TODO: move to different section
(use-package repeat
  :ensure nil
  :hook
  (after-init-hook . repeat-mode))

(use-package highlight-defined
  :init
  (setq highlight-defined-face-use-itself t)
  :hook
  (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode-hook . highlight-quoted-mode))

(use-package eros
  :custom-face
  (eros-result-overlay-face ((t (:inherit shadow :box t))))
  :hook
  (emacs-lisp-mode-hook . eros-mode))

(use-package package-lint)

(use-package flycheck-package
  :disabled ;; broken after flycheck update
  :after flycheck
  :demand
  :config
  (flycheck-package-setup))

(use-package clojure-ts-mode)

(use-package flycheck-clj-kondo
  :preface
  (defun +setup-flycheck-clj-kondo ()
    (require 'flycheck-clj-kondo))
  :hook
  (clojure-ts-mode-hook . +setup-flycheck-clj-kondo))

(use-package cider
  :custom-face
  (cider-result-overlay-face ((t (:inherit shadow :box t))))
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

(use-package cider-hydra
  :general
  (+local-leader-def :keymaps 'clojure-ts-mode-map
    "d" '(cider-hydra-doc/body  :wk "doc")
    "e" '(cider-hydra-eval/body :wk "eval")
    "t" '(cider-hydra-test/body :wk "test")
    "r" '(cider-hydra-repl/body :wk "repl"))
  :hook
  (clojure-ts-mode-hook . cider-hydra-mode))

(use-package clj-refactor
  :general
  (+local-leader-def :keymaps 'clojure-ts-mode-map
    "R" '(hydra-cljr-help-menu/body :wk "refactor"))
  :hook
  (clojure-ts-mode-hook . clj-refactor-mode))

(use-package go-ts-mode
  :ensure nil
  :general
  (+local-leader-def :keymaps 'go-ts-mode-map
    "t"  '(:ignore t :wk "test"))
  :init
  (setq go-ts-mode-indent-offset 4)
  :hook
  (go-ts-mode-hook . eglot-ensure))

(use-package go-mod-ts-mode
  :ensure go-ts-mode)

(use-package go-gen-test
  :general
  (+local-leader-def :keymaps 'go-ts-mode-map
    "tg" 'go-gen-test-dwim))

(use-package gotest
  :general
  (+local-leader-def :keymaps 'go-ts-mode-map
    "tf" 'go-test-current-file
    "tT" 'go-test-current-test
    "tp" 'go-test-current-project
    "tb" 'go-test-current-benchmark
    "tc" 'go-test-current-coverage
    "tx" 'go-run))

(use-package gotest-ts
  :general
  (+local-leader-def :keymaps 'go-ts-mode-map
    "tt" 'gotest-ts-run-dwim))

(use-package go-playground)

(use-package makefile-executor
  :general
  (+local-leader-def :keymaps 'makefile-mode-map
    "e" 'makefile-executor-execute-target)
  :hook
  (makefile-mode-hook . makefile-executor-mode))

(use-package just-mode) ;; TODO: just-ts-mode

(use-package justl
  :general
  ( :keymaps 'justl-mode-map :states 'normal
    "?" 'justl-help-popup
    "e" 'justl-exec-recipe
    "E" 'justl-exec-eshell
    "w" 'justl--exec-recipe-with-args
    "W" 'justl-no-exec-eshell)
  (+local-leader-def :keymaps 'just-mode-map
    "e" '(:ignore t :wk "eval")
    "e." 'justl
    "ee" 'justl-exec-recipe-in-dir))

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

(use-package markdown-mode
  :custom-face
  (markdown-code-face ((t (:inherit default))))
  :general
  (+local-leader-def :keymaps 'markdown-mode-map
    "." '(:keymap markdown-mode-command-map))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("clj" . clojure-mode)))

(use-package grip-mode
  :general
  (+local-leader-def :keymaps 'markdown-mode-map
    "g" 'grip-mode)
  :init
  (setq grip-update-after-change nil)
  (setq grip-preview-use-webkit t))

(use-package markdown-toc)

(use-package edit-indirect)

(use-package json-ts-mode
  :ensure nil
  :general
  (+local-leader-def :keymaps 'json-ts-mode-map
    "=" '(json-pretty-print-buffer :wk "format")))

(use-package yaml-ts-mode
  :ensure nil
  :hook
  (yaml-ts-mode-hook . flycheck-mode)
  ;; (yaml-ts-mode-hook . highlight-indent-guides-mode) ;; brakes scrolling
  )

(use-package yaml-pro
  :hook
  (yaml-ts-mode-hook . yaml-pro-ts-mode))

(use-package lua-ts-mode
  :ensure nil
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

(use-package ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package protobuf-ts-mode
  :mode "\\.proto\\'")

(use-package xwidget
  :if (display-graphic-p)
  :ensure nil
  :general
  (+leader-def
    "ow" 'xwidget-webkit-browse-url))

(use-package xwwp
  :if (display-graphic-p)
  :after xwidget
  :general
  ( :keymaps 'xwidget-webkit-mode-map :states 'normal
    "f" 'xwwp-follow-link))

(use-package editorconfig
  :ensure nil
  :hook
  (after-init-hook . editorconfig-mode))

(use-package docker
  :general
  (+leader-def
    "od" 'docker))

(use-package dockerfile-mode ;; TODO: dockerfile-ts-mode
  :general
  (+local-leader-def :keymaps 'dockerfile-mode-map
    "b" 'dockerfile-build-buffer
    "B" 'dockerfile-build-no-cache-buffer))

(use-package docker-compose-mode
  :general
  (+local-leader-def :keymaps 'docker-compose-mode-map
    "." 'docker-compose))

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package ansible-vault-with-editor
  :vc (:url "https://github.com/rynffoll/ansible-vault-with-editor" :rev :newest)
  :general
  (+local-leader-def :keymaps 'yaml-ts-mode-map
    "e" '(ansible-vault-with-editor-edit :wk "edit")
    "E" '(ansible-vault-with-editor-encrypt :wk "encrypt")
    "D" '(ansible-vault-with-editor-decrypt :wk "decrypt")))

(use-package direnv
  :if (executable-find "direnv")
  :preface
  (defun +direnv-hook ()
    (add-hook
     'after-save-hook
     (lambda ()
       (call-interactively 'direnv-update-environment))
     nil t))
  :general
  (+local-leader-def :keymaps 'direnv-envrc-mode-map
    "a" 'direnv-allow
    "u" 'direnv-update-environment)
  :init
  (setq direnv-always-show-summary nil)
  :hook
  (after-init-hook . direnv-mode)
  (direnv-envrc-mode-hook . +direnv-hook))

(use-package envrc
  :disabled
  :if (executable-find "direnv")
  :hook
  (after-init-hook . envrc-global-mode))

(use-package proced
  :ensure nil
  :general
  (+leader-def
    "op" 'proced)
  :init
  (setq proced-enable-color-flag t)
  ;; (setq proced-auto-update-flag t)
  (setq proced-format 'medium))

(use-package proced-narrow
  :general
  (proced-mode-map
   "M-n" 'proced-narrow))

(use-package recall
  :general
  (+leader-def
    "or" 'recall-list)
  :init
  (setq recall-completing-read-fn #'recall-consult-completing-read)
  :hook
  (after-init-hook . recall-mode))

(use-package gptel
  :preface
  (defun +gptel-send-back-evil-normal-state (&optional _result)
    "Switch to normal state after calling `gptel-send`."
    (evil-normal-state))
  :general
  (+leader-def
    "ag" '(:ignore t :wk "gptel")
    "ag." 'gptel-menu
    "agc" 'gptel)
  :init
  (setq gptel-default-mode 'org-mode)
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "### ")
          ;; (org-mode . "*** ")
          (org-mode . "* ")
          (text-mode . "### ")))
  (setq gptel-org-branching-context t)
  :config
  (advice-add 'gptel-send :after #'+gptel-send-back-evil-normal-state)
  :hook
  (gptel-mode-hook . toggle-truncate-lines)
  (gptel-mode-hook . toggle-word-wrap))

(use-package gptel-quick
  :vc ( :url "https://github.com/karthink/gptel-quick"
        :rev :newest)
  :general
  (+leader-def
    "ag?" 'gptel-quick))

(use-package chatgpt-shell
  :preface
  (defun +chatgpt-shell-openai-key ()
    (auth-source-pick-first-password :host "api.openai.com"))
  :general
  (+leader-def
    "as" '(:ignore t :wk "chatgpt-shell")
    "as." 'chatgpt-shell-prompt-compose
    "asc" 'chatgpt-shell
    "asq" 'chatgpt-shell-quick-insert)
  :init
  (setq chatgpt-shell-openai-key #'+chatgpt-shell-openai-key)
  (setq chatgpt-shell-model-version "gpt-4o-mini")
  :config
  (add-to-list
   'chatgpt-shell-models
   (chatgpt-shell-openai-make-model
    :version "gpt-4o-mini"
    :token-width 3
    ;; https://platform.openai.com/docs/models/gpt-4o#gpt-4o-mini
    :context-window 128000)))

(use-package copilot
  :vc ( :url "https://github.com/copilot-emacs/copilot.el"
        :rev :newest
        :branch "main")
  :general
  (+leader-def
    "ac" '(:ignore t :wk "copilot")
    "acm" 'copilot-mode
    "acD" 'copilot-diagnose)
  (copilot-completion-map
   "TAB" 'copilot-accept-completion
   "C-TAB" 'copilot-accept-completion-by-word
   "C-j" 'copilot-next-completion
   "C-k" 'copilot-previous-completion)
  :init
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 1000000)
  (setq copilot-max-char-warning-disable t)
  :hook
  (prog-mode-hook . copilot-mode)
  (git-commit-mode-hook . copilot-mode))

(use-package copilot-chat
  :general
  (+leader-def
    "acc" 'copilot-chat-display)
  (+local-leader-def :keymaps 'git-commit-mode-map
    "i" 'copilot-chat-insert-commit-message)
  :init
  (setq copilot-chat-frontend 'shell-maker))

(use-package focus
  :general
  (+leader-def
    "tf" 'focus-mode))

(use-package olivetti
  ;; :custom-face
  ;; (olivetti-fringe ((t (:background "unspecified-bg"))))
  :general
  (+leader-def
    "tz" 'olivetti-mode)
  :init
  (setq olivetti-body-width 0.6))

(use-package crux
  :general
  (+leader-def
    "fR" 'crux-rename-file-and-buffer
    "fD" 'crux-delete-file-and-buffer))

(use-package deadgrep
  :general
  (+leader-def
    "/D" 'deadgrep))

(use-package try)

(use-package string-inflection)

(use-package show-font)

(use-package disk-usage)

(use-package memory-usage)

(use-package list-environment)

(use-package daemons)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

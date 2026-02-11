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
  ;; :init
  ;; https://www.reddit.com/r/emacs/comments/1f8ok7c/comment/llhcdgy/
  ;; (setq package-install-upgrade-built-in t)
  :config
  ;; https://github.com/melpa/melpa
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize))

(use-package gnu-elpa-keyring-update)

(defvar +with-evil t)

(defvar +with-icons t)

(use-package emacs
  :ensure nil
  :init
  (setq confirm-kill-emacs 'y-or-n-p))

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
    :global-prefix "S-SPC")
  (general-create-definer +local-leader-def
    :states '(normal visual insert emacs motion)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "S-SPC m")
  (general-define-key
   :states '(normal visual)
   "," (general-simulate-key "SPC m" :which-key "local leader"))
  (+leader-def
    ""    '(nil :wk "leader")
    "l"   '(:ignore t :wk "llm")
    "lc"  '(:ignore t :wk "chats")
    "la"  '(:ignore t :wk "agents")
    "o"   '(:ignore t :wk "open")
    "O"   '(:ignore t :wk "org")
    "p"   '(:ignore t :wk "project") ;; TODO: project-prefix-map
    "F"   '(:ignore t :wk "frame")
    "TAB" '(:ignore t :wk "tab") ;; TODO: tab-prefix-map
    "b"   '(:ignore t :wk "buffer")
	"S"   '(:ignore t :wk "session")
    "f"   '(:ignore t :wk "file")
    "e"   '(:ignore t :wk "emacs")
    "g"   '(:ignore t :wk "git")
    "/"   '(:ignore t :wk "search") ;; TODO: search-map (M-s)
    "j"   '(:ignore t :wk "jump") ;; TODO: goto-map (M-g)
    "h"   '(:ignore t :wk "help") ;; TODO: help-map (C-h)
    "t"   '(:ignore t :wk "toggle")
    "i"   '(:ignore t :wk "insert")
    "q"   '(:ignore t :wk "quit"))
  (+local-leader-def
    ""    '(nil :wk "local leader")))

(use-package evil
  :if +with-evil
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
  :if +with-evil
  :demand
  :after evil
  :init
  (setq evil-collection-magit-want-horizontal-movement t)
  :config
  (evil-collection-init))

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

(use-package which-key
  :ensure nil
  :init
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-dont-use-unicode nil)
  :hook
  (after-init-hook . which-key-mode))

(use-package repeat
  :ensure nil
  :hook
  (after-init-hook . repeat-mode))

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

(use-package emacs
  :ensure nil
  :init
  (setq scroll-step 1)
  (setq scroll-preserve-screen-position t)
  (setq scroll-margin 0)
  (setq scroll-conservatively 101)
  (setq fast-but-imprecise-scrolling t)
  (setq redisplay-skip-fontification-on-input t))

(use-package pixel-scroll
  :ensure nil
  :hook
  (after-init-hook . pixel-scroll-precision-mode))

(use-package ultra-scroll
  :if (display-graphic-p)
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

(use-package nerd-icons
  :if +with-icons
  :init
  (setq nerd-icons-color-icons t)
  :config
  (when (and (display-graphic-p)
             (not (member "Symbols Nerd Font Mono" (font-family-list))))
    (nerd-icons-install-fonts)))

(use-package hide-mode-line)

(use-package minions
  :hook
  (after-init-hook . minions-mode))

(use-package doom-modeline
  :custom-face
  (mode-line ((t (:height 0.9))))
  (mode-line-active ((t (:height 0.9))))
  (mode-line-inactive ((t (:height 0.9))))
  :init
  (setq doom-modeline-icon +with-icons)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq doom-modeline-check 'simple)
  (setq doom-modeline-unicode-number nil)
  (setq doom-modeline-workspace-name nil)
  :hook
  (after-init-hook . doom-modeline-mode))

(use-package custom
  :ensure nil
  :general
  (+leader-def
    "tt" 'load-theme))

(use-package modus-themes
  ;; :ensure nil
  :pin melpa-stable
  :init
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
        '(;; (bg-region bg-cyan-intense)
          (fg-region unspecified)
          (bg-prose-block-delimiter bg-inactive)
          (fg-prose-block-delimiter fg-dim)
          (bg-prose-block-contents bg-dim)
          (fringe unspecified)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          )))

(use-package ef-themes)

(use-package doric-themes)

(use-package standard-themes)

(use-package doom-themes
  :init
  (setq doom-themes-enable-italic t)
  :config
  (doom-themes-org-config))

(setq +theme 'modus-operandi)
;; (setq +theme 'ef-melissa-light)
;; (setq +theme 'doom-earl-grey)

(load-theme +theme :no-confirm)

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
  (defun +tab-bar-bar-wrap (name &optional _tab _i) (concat " " name " "))
  (defun +tab-bar-tab-group-format-default (tab i &optional current-p)
    (propertize
     (+tab-bar-bar-wrap ;; patch start: wrap with spaces
      (concat (if (and tab-bar-tab-hints (not current-p)) (format "%d " i) "")
              (funcall tab-bar-tab-group-function tab))
      ) ;; patch end
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))
  :custom-face
  (tab-bar ((t (:height 0.9))))
  :general
  (+leader-def
    "TAB" '(:keymap tab-prefix-map :wk "tab-bar"))
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
  :init
  (setq tab-bar-show t)
  (setq tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-separator
                         tab-bar-format-align-right
                         tab-bar-format-global))
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-tab-hints t)
  ;; (setq tab-bar-separator " ") ;; the same behavior in GUI and TUI
  (setq tab-bar-separator (if (display-graphic-p) "\u200B" " "))
  (setq tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-hints
                                            ;; tab-bar-tab-name-format-close-button
                                            +tab-bar-bar-wrap
                                            tab-bar-tab-name-format-face))
  (setq tab-bar-tab-group-format-function #'+tab-bar-tab-group-format-default)
  :hook
  (after-init-hook . tab-bar-mode)
  (after-init-hook . tab-bar-history-mode))

(use-package tab-bar-theme
  :ensure nil
  :load-path "site-lisp/tab-bar-theme"
  :hook
  (after-init-hook . tab-bar-theme-mode))

(use-package project-tab-groups
  :hook
  (after-init-hook . project-tab-groups-mode))

(use-package per-tab-group-theme
  :ensure nil
  :load-path "site-lisp/per-tab-group-theme"
  :hook
  (after-init-hook . per-tab-group-theme-mode))

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
  :if +with-evil
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
  :ensure nil
  :general
  (+leader-def
    "Ss" 'desktop-save-in-desktop-dir
    "Sr" 'desktop-read)
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

(use-package consult-todo
  :general
  (+leader-def
    "jt" 'consult-todo))

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
  (+leader-def
    "it" 'tempel-insert)
  ( :keymaps 'tempel-map
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
  (setq auto-save-default nil))

(use-package autorevert
  :ensure nil
  :init
  (setq auto-revert-verbose t)
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-stop-on-user-input nil)
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
  :disabled
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
  :if +with-icons
  :preface
  (defun +nerd-icons-dired-refresh ()
    (when (bound-and-true-p nerd-icons-dired-mode)
      (nerd-icons-dired--refresh)))
  :hook
  (dired-mode-hook . nerd-icons-dired-mode)
  (dired-subtree-after-insert-hook . +nerd-icons-dired-refresh))

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
  ;; (setq dired-sidebar-theme (if +with-icons 'nerd-icons 'none))
  (setq dired-sidebar-theme 'none) ;; don't need dired-sidebar's customizations for nerd-icons, it's flickery and buggy, just use nerd-icons-dired
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
  (ediff-prepare-buffer-hook . show-all)
  (ediff-quit-hook . winner-undo))

(use-package undo-fu-session
  :init
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :hook
  (after-init-hook . undo-fu-session-global-mode))

(use-package vundo
  :general
  ("C-x u" 'vundo)
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

(use-package paren-face
  :hook
  (after-init-hook . global-paren-face-mode))

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
  :ensure nil)

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
  :if +with-evil
  :demand
  :after evil anzu)

(use-package hideshow
  :ensure nil
  :hook
  (prog-mode-hook . hs-minor-mode))

(use-package outline-indent
  :hook
  (yaml-ts-mode-hook . outline-indent-minor-mode))

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

(use-package flymake
  :ensure nil
  :general
  ( :keymaps 'project-prefix-map
    "D" 'flymake-show-project-diagnostics)
  :init
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-margin-indicator-position 'right-margin)
  :hook
  (prog-mode-hook . flymake-mode))

(use-package flyover
  :disabled ;; switch to sideline-flymake
  :custom
  (flyover-checkers '(flymake))
  (flyover-display-mode 'show-only-on-same-line)
  (flyover-show-at-eol t)
  (flyover-virtual-line-type nil)
  (flyover-error-icon   (car (alist-get 'error   flymake-margin-indicators-string)))
  (flyover-warning-icon (car (alist-get 'warning flymake-margin-indicators-string)))
  (flyover-info-icon    (car (alist-get 'note    flymake-margin-indicators-string)))
  (flyover-icon-left-padding 0.5)
  (flyover-icon-right-padding 0.5)
  :hook
  (flymake-mode-hook . flyover-mode))

(use-package sideline
  :init
  (setq sideline-backends-right '(sideline-flymake))
  (setq sideline-display-backend-name t))

(use-package sideline-flymake
  :init
  (setq sideline-flymake-display-mode 'point)
  :hook
  (flymake-mode-hook . sideline-mode))

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

(use-package vterm
  :general
  (+leader-def
    "ot" 'vterm)
  :init
  (setq vterm-shell "/opt/homebrew/bin/fish")
  (setq vterm-max-scrollback 10000)
  (setq vterm-set-bold-highbright t)
  :config
  ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
  (advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))
  :hook
  (vterm-mode-hook . +disable-global-hl-line-mode))

(use-package project-vterm
  :ensure nil
  :load-path "site-lisp/project-vterm"
  :general
  ( :keymaps 'project-prefix-map
    "t" 'project-vterm)
  :config
  (add-to-list 'project-switch-commands '(project-vterm "Vterm") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode)))

(use-package eat
  :general
  (+leader-def
    "oe" 'eat)
  :hook
  (eat-mode-hook . +disable-global-hl-line-mode))

(use-package project-eat
  :ensure nil
  :load-path "site-lisp/project-eat"
  :general
  ( :keymaps 'project-prefix-map
    "E" 'project-eat)
  :config
  (add-to-list 'project-switch-commands '(project-eat "Eat") t)
  (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode)))

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
                                       ("~/Projects/"         . 2)
                                       ("~/Developer/"        . 2)))
  (setq magit-diff-refine-hunk t)
  (setq magit-process-apply-ansi-colors t))

(use-package magit
  :if +with-icons
  :init
  (setq magit-format-file-function #'magit-format-file-nerd-icons))

(use-package magit-todos
  :init
  (setq magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")" ":")))
  (put 'magit-todos-exclude-globs 'safe-local-variable #'listp)
  :hook
  (magit-mode-hook . magit-todos-mode))

(use-package git-modes
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package diff-hl
  :vc (:url "https://github.com/rynffoll/diff-hl" :branch "dired-nested-paths" :rev :newest)
  :preface
  (defun +diff-hl-fringe-bmp-empty (_type _pos) 'diff-hl-bmp-empty)
  (defun +diff-hl-dired-update ()
    (when (bound-and-true-p diff-hl-dired-mode)
      (diff-hl-dired-update)))
  :init
  (setq diff-hl-update-async t)
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-margin-symbols-alist
        '((insert . " ") (delete . " ") (change . " ")
          (unknown . " ") (ignored . " ") (reference . " ")))
  (setq diff-hl-dired-extra-indicators nil)
  (setq diff-hl-dired-fringe-bmp-function #'+diff-hl-fringe-bmp-empty)
  :hook
  (after-init-hook . global-diff-hl-mode)
  (after-init-hook . global-diff-hl-show-hunk-mouse-mode)
  ;; (diff-hl-mode-hook . diff-hl-flydiff-mode) ;; disabled by perf issues
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (dired-mode-hook . diff-hl-dired-mode)
  (dired-subtree-after-insert-hook . +diff-hl-dired-update))

(use-package git-link
  :general
  (+leader-def
    "gL" 'git-link-dispatch))

(use-package consult-git-log-grep
  :general
  (+leader-def
    "gj" 'consult-git-log-grep)
  :init
  (setq consult-git-log-grep-open-function #'magit-show-commit))

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
  (org-mode-map
   "C-," nil ;; disable org-cycle-agenda-files
   "C-'" nil ;; disable org-cycle-agenda-files
   )
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
    "Fi" 'eglot-find-implementation
    "H" '(:ignore t :wk "hierarchy")
    "Hc" 'eglot-show-call-hierarchy
    "Ht" 'eglot-show-type-hierarchy)
  :init
  (setq eglot-autoshutdown t))

(use-package consult-eglot
  :general
  (+local-leader-def :keymaps 'eglot-mode-map
    "Fs" 'consult-eglot-symbols))

(use-package consult-eglot-embark
  :config
  (consult-eglot-embark-mode))

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

(use-package mason
  :preface
  (defun +mason-setup ()
    (mason-setup))
  :hook
  (after-init-hook . +mason-setup))

(use-package treesit
  :ensure nil
  :init
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (gosum . ("https://github.com/tree-sitter-grammars/tree-sitter-go-sum"))
          (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
          (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))))
  ;; TODO: emacs 31: treesit-enabled-modes
  (setq major-mode-remap-alist
        '((go-mode . go-ts-mode)
          (go-mod-mode . go-mod-ts-mode)
          (clojure-mode . clojure-ts-mode)
          (lua-mode . lua-ts-mode)
          (json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)))
  :config
  ;; TODO: emacs 31: treesit-auto-install-grammar
  (dolist (source treesit-language-source-alist)
    (unless (treesit-ready-p (car source) t) ;; `t' to quietly check
      (treesit-install-language-grammar (car source)))))

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

(use-package clj-refactor
  :general
  (+local-leader-def :keymaps 'clojure-ts-mode-map
    "R" '(hydra-cljr-help-menu/body :wk "refactor"))
  :hook
  (clojure-ts-mode-hook . clj-refactor-mode))

(use-package go-ts-mode
  :ensure nil
  :mode
  ("\\.go\\'" . go-ts-mode)
  ("go\\.mod\\'" . go-mod-ts-mode)
  :general
  (+local-leader-def :keymaps 'go-ts-mode-map
    "t"  '(:ignore t :wk "test"))
  :init
  (setq go-ts-mode-indent-offset 4)
  :hook
  (go-ts-mode-hook . eglot-ensure))

(use-package gotest-ts
  :general
  (+local-leader-def :keymaps 'go-ts-mode-map
    "tt" 'gotest-ts-run-dwim))

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
  :mode ("\\.json\\'" . json-ts-mode)
  :general
  (+local-leader-def :keymaps 'json-ts-mode-map
    "=" '(json-pretty-print-buffer :wk "format")))

(use-package yaml-ts-mode
  :ensure nil
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :preface
  (defun +yaml-ts-mode-set-evil-shift-width ()
    (setq-local evil-shift-width 2))
  :hook
  ;; (yaml-ts-mode-hook . flycheck-mode) ;; switch to flymake
  (yaml-ts-mode-hook . flymake-mode) 
  (yaml-ts-mode-hook . +yaml-ts-mode-set-evil-shift-width))

(use-package yaml-pro
  :hook
  (yaml-ts-mode-hook . yaml-pro-ts-mode))

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

(use-package ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package protobuf-ts-mode
  :mode "\\.proto\\'")

(use-package dockerfile-ts-mode
  :ensure nil
  :mode ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode))

(use-package editorconfig
  :ensure nil
  :hook
  (after-init-hook . editorconfig-mode))

(use-package docker)

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

(use-package mise
  :hook
  (after-init-hook . global-mise-mode))

(use-package proced
  :ensure nil
  :init
  (setq proced-enable-color-flag t)
  ;; (setq proced-auto-update-flag t)
  (setq proced-format 'medium))

(use-package proced-narrow
  :general
  (proced-mode-map
   "M-n" 'proced-narrow))

(use-package recall
  :init
  (setq recall-completing-read-fn #'recall-consult-completing-read)
  :hook
  (after-init-hook . recall-mode))

(use-package gptel
  :general
  (+leader-def
    "lcg" 'gptel)
  (+local-leader-def :keymaps 'gptel-mode-map
    "." 'gptel-menu)
  (embark-general-map
   "." #'gptel-menu)
  :init
  (setq gptel-default-mode 'org-mode)
  (setq gptel-prompt-prefix-alist
        '((markdown-mode . "### ")
          ;; (org-mode . "*** ")
          (org-mode . "* ")
          (text-mode . "### ")))
  (setq gptel-org-branching-context t)
  (setq gptel-model 'gpt-5)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :hook
  (gptel-post-stream-hook . gptel-auto-scroll)
  (gptel-post-response-functions . gptel-end-of-response))

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick" :rev :newest)
  :general
  (embark-general-map
   "?" #'gptel-quick))

(use-package gptel-magit
  :hook
  (magit-mode-hook . gptel-magit-install))

(use-package chatgpt-shell
  :preface
  (defun +chatgpt-shell-openai-key ()
    (auth-source-pick-first-password :host "api.openai.com"))
  :general
  (+leader-def
    "lcs" 'chatgpt-shell)
  :init
  (setq chatgpt-shell-openai-key #'+chatgpt-shell-openai-key))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :general
  (+leader-def
    "laC" 'claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(use-package mcp)

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el" :rev :newest))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell" :rev :newest)
  :preface
  (defun +agent-shell-diff-evil-setup ()
    (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
      ;; n - next conflict hunk
      ;; p - previous conflict hunk
      ;; q - kill buffer and exit
      (evil-local-set-key 'normal "n" #'diff-hunk-next)
      (evil-local-set-key 'normal "p" #'diff-hunk-prev)
      (evil-local-set-key 'normal "q" #'kill-current-buffer)))
  :general
  (+leader-def
    "las" 'agent-shell)
  (+local-leader-def :keymaps 'agent-shell-mode-map
    "." 'agent-shell-help-menu)
  ( :keymaps 'project-prefix-map
    "a" 'agent-shell)
  :hook
  (diff-mode-hook . +agent-shell-diff-evil-setup))

(use-package agent-shell-manager
  :vc (:url "https://github.com/jethrokuan/agent-shell-manager" :rev :newest))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest)
  :general
  (copilot-completion-map
   "TAB"   'copilot-accept-completion
   "C-TAB" 'copilot-accept-completion-by-word
   "C-j"   'copilot-next-completion
   "C-k"   'copilot-previous-completion)
  :init
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 1000000)
  (setq copilot-max-char-warning-disable t)
  :hook
  (prog-mode-hook . copilot-mode)
  (git-commit-mode-hook . copilot-mode))

(use-package focus
  :general
  (+leader-def
    "tf" 'focus-mode))

(use-package olivetti
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

(use-package try)

(use-package string-inflection)

(use-package show-font)

(use-package disk-usage)

(use-package list-environment)

(use-package daemons)

(use-package free-keys)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

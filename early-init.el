;;; early-init.el --- Early Initialization -*- lexical-binding: t; no-byte-compile: t -*-

(setq default-directory "~/")

(setq load-prefer-newer t)
(setq create-lockfiles nil)
(setq ring-bell-function 'ignore)
(setq delete-by-moving-to-trash t)
(setq read-process-output-max (* 1024 1024))
(setq native-comp-async-report-warnings-errors nil)
(setq use-short-answers t) ;; yes-or-no -> y-or-n

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (let ((init-time (float-time (time-subtract after-init-time before-init-time)))
                    (packages  (length package-activated-list))
                    (gc-time   (float-time gc-elapsed))
                    (gc-count  gcs-done))
                (message "Emacs ready (init time = %.2fs, packages = %d, gc time = %.2fs, gc count = %d)."
                         init-time packages gc-time gc-count))))

(defvar +file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq file-name-handler-alist +file-name-handler-alist)))

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(setq initial-scratch-message nil)

(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

(setq use-dialog-box nil)

(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(top  . 0.5))
(add-to-list 'default-frame-alist '(width  . 0.75))
(add-to-list 'default-frame-alist '(height . 0.9))

(setq inhibit-compacting-font-caches t)

;; (setq +font "JetBrains Mono:weight=medium:size=14")
;; (setq +font "Iosevka Term:weight=medium:size=14")
;; (setq +font "Iosevka Term:weight=medium:width=expanded:size=14") ;; Ioesevka Term Extended
;; (setq +font "Martian Mono Condensed 14")
;; (setq +font "Cascadia Code:size=16")
;; (setq +font "Cascadia Code NF:size=16")
;; (setq +font "Iosevka:size=16")
(setq +font "Iosevka Term:size=16")

(add-to-list 'default-frame-alist `(font . ,+font))

(when (featurep 'ns)
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)
  (add-to-list 'default-frame-alist '(undecorated-round . t))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; (setq custom-file null-device)
;; Fix: Error in post-command-hook (vertico--exhibit): (error "Maximum buffer size exceeded")
(setq custom-file (locate-user-emacs-file "custom.el"))

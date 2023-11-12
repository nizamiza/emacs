;; Personal configuration -*- lexical-binding: t -*-

;; Originally based on configuration generated via:
;; https://emacs.amodernist.com/

;; Modified and tailored for Niza Toshpulatov's <niza@hey.com>
;; needs by Niza himself. This config is primarily oriented
;; towards web development.

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;;; Options

;; Disable title bar
(menu-bar-mode -1)

;; Disable startup screen
(setq inhibit-startup-message t
      visible-bell nil)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Remember recent stuff
(recentf-mode t)
(setq history-length 25)

(savehist-mode t)
(save-place-mode t)

;; Set indentation settings
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set backup and auto-save files location
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))

(setq backup-directory-alist
      '((".*" . "~/.emacs.d/backup")))

;; Auto-sync files when they change on disk
(global-auto-revert-mode t)

;; Enable line and column numbering
(global-display-line-numbers-mode t)
(column-number-mode)

;; Automatically pair parentheses
(electric-pair-mode t)

;; Display time
(setq display-time-24hr-format t)
(display-time-mode t)

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;; Alias 'yes or no' confirmations to 'y or n'
(setq confirm-kill-emacs #'yes-or-no-p)
(defalias 'yes-or-no-p #'y-or-n-p)

;;; Initialize package manager

(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install use-package
;; use-package provides better interface over package management than
;; the default package manager that ships with Emacs. Thanks to its
;; non-blocking architecture, it also improves load times dramatically.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; always install packages if missing

;;; Set color theme

(use-package modus-themes
  :bind ("C-c t" . modus-themes-toggle))

;; Custom function for detecting macOS appearance
(defun set-color-theme-based-on-appearance ()
  (let ((macos-appearance (string-trim (shell-command-to-string "osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'"))))
    (if (string= macos-appearance "true")
        (load-theme 'modus-vivendi t)
      (load-theme 'modus-operandi t))))

;; Make sure that theme is installed
(unless (package-installed-p 'modus-themes)
  (package-install 'modus-themes))

;; Set theme based on appearance
(when (package-installed-p 'modus-themes)
  (set-color-theme-based-on-appearance))

;;; UI packages

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package which-key
  :diminish
  :init (which-key-mode))

;;; Other packages

(use-package format-all
  :commands format-all-mode
  :hook
  (prog-mode . format-all-mode)
  (format-all . format-all-ensure-formatter)
  :config
  (setq-default format-all-formatters '(("JavaScript" (prettierd))
                                        ("TypeScript" (prettierd))
                                        ("HTML" (prettierd))
                                        ("CSS" (prettierd))
                                        ("JSX" (prettierd))
                                        ("TSX" (prettierd))
                                        ("JSON" (prettierd))
                                        ("YAML" (prettierd))
                                        ("Python" (black))
                                        ("Markdown" (prettierd)))))

(use-package auto-complete
  :config
  (ac-config-default))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo"))))

(use-package vertico
  :config
  (vertico-mode t))

(use-package consult
  :bind
  ([rebind switch-to-buffer] . consult-buffer)
  ("C-c i" . consult-imenu)
  ("C-x b" . consult-buffer)
  :config
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t)))))

(use-package eglot
  :hook (prog-mode . eglot-ensure))

(use-package sweeprolog
  :mode ("\\.pl\\'" . sweeprolog-mode))

(use-package corfu
  :config
  (setq corfu-auto t)
  :hook (prog-mode . corfu-mode))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package diff-hl
  :hook prog-mode)

(use-package elixir-mode)

(use-package json-mode)

(use-package kotlin-mode)

(use-package lua-mode)

(use-package sly)

(use-package typescript-mode)

(use-package yaml-mode)

(use-package markdown-mode)

(use-package org
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :config
  (use-package org-contrib))

(use-package crdt)

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package avy
  :bind ("C-c z" . avy-goto-word-1)
  :config
  (setq avy-all-windows 'all-frames))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;;; Initialize package
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;;; Color theme
(unless (package-installed-p 'modus-themes)
  (package-install 'modus-themes))

(use-package modus-themes
  :bind ("C-c t" . modus-themes-toggle))

(load-theme 'modus-vivendi t)
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

;; Backup files
(setq auto-save-default nil)
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/backup")))

;; Auto-sync files
(global-auto-revert-mode t)

;; Enable line numbering by default
(global-display-line-numbers-mode t)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; Packages
(use-package prettier
  :ensure t
  :hook (after-init . global-prettier-mode))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo"))))

(use-package vertico
  :ensure t
  :config
  (vertico-mode t))

(use-package consult
  :ensure t
  :bind
  ([rebind switch-to-buffer] . consult-buffer)
  ("C-c i" . consult-imenu))

(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure))

(use-package sweeprolog
  :ensure t
  :mode ("\\.pl\\'" . sweeprolog-mode))

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t)
  :hook (prog-mode . corfu-mode))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package diff-hl
  :ensure t
  :hook prog-mode)

(use-package elixir-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package sly
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package org
  :ensure t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :config
  (use-package org-contrib
    :ensure t))

(use-package crdt
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode t))

(use-package avy
  :ensure t
  :bind ("C-c z" . avy-goto-word-1)
  :config
  (setq avy-all-windows 'all-frames))

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(defalias 'yes-or-no-p #'y-or-n-p)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

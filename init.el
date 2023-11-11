;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;;; Options

;; Disable title bar
(menu-bar-mode -1)

;; Set cursor style
(setq-default cursor-type 'hollow)

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

;; Display time
(setq display-time-24hr-format t)
(display-time-mode t)

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

;;; Initialize package

(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Color theme

(use-package modus-themes
  :bind ("C-c t" . modus-themes-toggle))

(unless (package-installed-p 'modus-themes)
  (package-install 'modus-themes))

(load-theme 'modus-vivendi t)

;;; Aesthetics

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;;; Packages

(use-package prettier
  :hook (after-init . global-prettier-mode))

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

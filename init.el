;;;;; Startup
;;;; basic-settings emacs
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(windmove-default-keybindings)
(setq mouse-autoselect-window t
      focus-follow-mouse t)
(setq use-dialog-box nil)
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq ns-alternate-modifier nil)

;;;; Packages

;;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;;; EXWM
;(use-package exwm
;:config
;(require 'exwm)
;(require 'exwm-config)
;(require 'exwm-systemtray)
;(exwm-config-example)
;(exwm-systemtray-enable)
;(setq exwm-workspace-number 9))

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;;; Undo
(use-package undo-fu)

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;;; All the icons
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

;;; Dirvish
(use-package dirvish
:init
(dirvish-override-dired-mode)
:config
(setq dired-mouse-drag-files t)
(setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
(setq dirvish-attributes
      '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
(setq delete-by-moving-to-trash t)
(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group"))

;;; Doom Bar
(use-package doom-modeline
  :config
  (doom-modeline-mode))

;;; Vertico
(use-package vertico
  :config
  (vertico-mode))

;;; Gruvbox Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

;;; cIRCe
(use-package circe)

;;; MPV
(use-package mpv)

;;; VTerm
(use-package vterm)

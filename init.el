;;; Startup

;;; Minimal-like emacs
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq use-dialog-box nil)
(setq auto-save-default nil)
(setq make-backup-files nil)

;;; Change custom-garbage output to custom.el rather than init.el
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

;;; PACKAGE LIST
(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

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

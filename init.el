;;;;; Startup
;;;; Basic-settings emacs
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

;;;; Binding Keys
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x w") 'kill-buffer-and-window)

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

;;; All the icons
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

;;; EMMS
(use-package emms
  :config
  (emms-minimalistic)
  (setq emms-player-list '(emms-player-mpv)
	emms-info-functions '(emms-info-native))
  (setq emms-source-file-default-directory "~/Music/")

  ;;; MPV volume fix
  (defvar emms-player-mpv-volume 100)

  (defun emms-player-mpv-get-volume ()
    "Sets `emms-player-mpv-volume' to the current volume value
and sends a message of the current volume status."
    (emms-player-mpv-cmd '(get_property volume)
                         #'(lambda (vol err)
                             (unless err
                               (let ((vol (truncate vol)))
                                 (setq emms-player-mpv-volume vol)
                                 (message "Music volume: %s%%"
                                           vol))))))

  (defun emms-player-mpv-raise-volume (&optional amount)
    (interactive)
    (let* ((amount (or amount 10))
           (new-volume (+ emms-player-mpv-volume amount)))
      (if (> new-volume 100)
          (emms-player-mpv-cmd '(set_property volume 100))
        (emms-player-mpv-cmd `(add volume ,amount))))
    (emms-player-mpv-get-volume))

  (defun emms-player-mpv-lower-volume (&optional amount)
    (interactive)
    (emms-player-mpv-cmd `(add volume ,(- (or amount '10))))
    (emms-player-mpv-get-volume))
  
  ;; Musikcube-like binding
  :bind
  ("C-c m" . emms-play-file)
  ("C-c C-SPC" . emms-pause)
  ("C-c C-o" . emms-seek-forward)
  ("C-c C-u" . emms-seek-backward)
  ("C-c C-l" . emms-next)
  ("C-c C-j" . emms-previous)
  ("C-c C-i" . emms-player-mpv-raise-volume)
  ("C-c C-k" . emms-player-mpv-lower-volume)
  ("C-c C-." . emms-toggle-repeat-track))

;;; Vertico
(use-package vertico
  :config
  (vertico-mode))

;;; Gruvbox Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

;;; Packages with no config
(use-package undo-fu)
(use-package unicode-fonts)
(use-package elpher)
(use-package circe)
(use-package mpv)
(use-package vterm)
(use-package elcord)

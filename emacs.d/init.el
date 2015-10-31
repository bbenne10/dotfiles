;;; Package -- summary
; Emacs customization files

;;; Commentary:
; Nothing to see here

;;; Code:
(load-file "~/.emacs.d/pkg.el")
(load-file "~/.emacs.d/keymap.el")

; change how emacs looks
(set-frame-font "Input-9")
;(load-theme 'gruvbox t)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-linum-mode 1)
(show-paren-mode 1)
(setq-default truncate-lines t)
(global-hl-line-mode 1)
(setq-default fill-column 80)

;; Don't care to see the startup message
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Audible bell and backup files are literally cancer
(setq visible-bell t)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Tab character is bad and it should feel bad
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 120 2))
(setq-default sgml-basic-offset 2)
(setq-default py-indent-offset 4)
(setq-default python-indent 4)

;; Don't make me type out the full word...even if it's important
(defalias 'yes-or-no-p 'y-or-n-p)

;; "Clipboard" is terribad.
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)
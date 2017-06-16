(load "~/.emacs.d/fira_init.el")

;; (set-frame-font "Fira Code:size=10")
(setq default-frame-alist '((font . "Fira Code:size=12")))
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

(global-linum-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

(setq-default fill-column 80)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq scroll-conservatively 101)
(setq scroll-margin 5)

(setq visible-bell t)

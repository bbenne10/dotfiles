;;; Package -- summary
; Emacs customization files

;;; Commentary:
; Nothing to see here

;;; Code:
(load-file "~/.emacs.d/pkg.el")
(load-file "~/.emacs.d/keymap.el")
(load-file "~/.emacs.d/langs.el")

; change how emacs looks
(set-frame-font "Knack-10")
(setq default-frame-alist '((font . "Knack-10")))

(if (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame) (with-selected-frame frame (load-theme 'material t))))
(load-theme 'material t))

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

;; Audible bell is literally cancer
(setq visible-bell t)

;; Set backup files in a sane dir
(setq backup-directory-alist `(("." . "~/.saves")))

;; Tab character is bad and it should feel bad
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 120 2))
(setq-default sgml-basic-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default py-indent-offset 4)
(setq-default python-indent 4)

;; Don't make me type out the full word...even if it's important
(defalias 'yes-or-no-p 'y-or-n-p)

;; Delete trailing whitespace when saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Copy to primary selection, not clipboard.
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#262626"))
 '(custom-safe-themes
   (quote
    ("870a63a25a2756074e53e5ee28f3f890332ddc21f9e87d583c5387285e882099" default)))
 '(fci-rule-color "#3a3a3a")
 '(hl-sexp-background-color "#121212")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

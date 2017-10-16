(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/languages.el")

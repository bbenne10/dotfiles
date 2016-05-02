(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load "~/.emacs.d/base.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/langs.el")

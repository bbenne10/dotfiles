(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)

(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package)
  (require 'diminish)
)

(use-package evil
  ;; We're first so we define the maps we override later
  :config
  (evil-mode 1)
)
(use-package all-the-icons)
(use-package spaceline-all-the-icons
  :after spaceline-config
  :config
    (setq spaceline-all-the-icons-separator-type 'cup)
    (spaceline-all-the-icons--setup-neotree)
    (spaceline-all-the-icons-theme)
)
(use-package ample-theme)
(use-package anaconda-mode
  :commands anaconda-mode
  :diminish anaconda-mode
  :config
    (defun python-insert-trace ()
      ;; insert a line that impots pdb and sets a trace just below the current line
      (interactive)
      (move-end-of-line 1)
      (insert "\n")
      (indent-according-to-mode)
      (insert "import pdb; pdb.set_trace()")
    )
    (add-hook 'python-mode-hook (function (lambda ()
        (setq evil-shift-width python-indent-offset)
        (define-key evil-normal-state-map (kbd "C-c g") 'anaconda-mode-find-definitions)
        (define-key evil-normal-state-map (kbd "C-c a") 'anaconda-mode-find-assignments)
        (define-key evil-normal-state-map (kbd "C-c r") 'anaconda-mode-find-references)
        (define-key evil-normal-state-map (kbd "C-c ?") 'anaconda-mode-show-doc)
        (define-key evil-normal-state-map (kbd "C-c t") 'python-insert-trace)
        'anaconda-mode)))
    )
(use-package base16-theme
  :config
  (load-theme 'base16-gruvbox-dark-medium)
)
(use-package company
  :diminish company-mode
  :config
    (setq company-tooltip-limit 20
          company-tooltip-align-annotations t)
    (global-company-mode 1)
)
(use-package company-anaconda
  :init (add-to-list 'company-backends 'company-anaconda)
)
(use-package counsel
  :init
    (defun counsel-ag-project-at-point ()
      (interactive)
      (counsel-ag (thing-at-point 'symbol) (projectile-project-root))
    )
  :config
    (define-key evil-normal-state-map (kbd "/") 'swiper)
    (define-key evil-normal-state-map (kbd "M-x") 'counsel-M-x)
    (define-key evil-normal-state-map (kbd "<SPC>f") 'counsel-ag-project-at-point)
    (define-key evil-normal-state-map (kbd "<SPC>B") 'ivy-switch-buffer)
    (define-key evil-normal-state-map (kbd "<SPC>p") 'counsel-find-file)
)
(use-package counsel-dash
  :init
    (setq counsel-dash-docsets-path "~/.emacs/dash-docsets"
          counsel-dash-min-length 3
          counsel-dash-browser-func 'browse-url
    )

    (defun counsel-dash-at-point ()
      (interactive)
      (counsel-dash (thing-at-point 'symbol))
    )
  :config
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
    (add-hook 'js-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "MomentJS "))))
    (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python3" "Flask" "Jinja"))))
    (add-hook 'scala-mode-hook (lambda () (setq-local counsel-dash-docsets '("Scala"))))
    (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C++"))))
    (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
    (add-hook 'sh-mode-hook (lambda () (setq-local counsel-dash-docsets '("Bash"))))
    (define-key evil-normal-state-map (kbd "<SPC>d") 'counsel-dash-at-point)
    (define-key evil-normal-state-map (kbd "<SPC>D") 'counsel-dash)
)
(use-package editorconfig)
(use-package ensime
  :init
    (setq ensime-startup-notification nil)
    (setq ensime-startup-snapshot-notification nil)
)
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1)
)
(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
)
(use-package evil-visual-mark-mode
  :config
  (evil-visual-mark-mode 1)
)
(use-package fic-mode
  :config
    (add-hook 'prog-mode-hook (function (lambda () (fic-mode 1))))
)
(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side . bottom)
                 (window-height . 0.2)))
  :config
    (define-key evil-normal-state-map (kbd "<SPC>lo") 'flycheck-list-errors)
    (define-key evil-normal-state-map (kbd "<SPC>lc") 'delete-flycheck-errors-list)
    (define-key evil-normal-state-map (kbd "<SPC>ln") 'flycheck-next-error)
    (define-key evil-normal-state-map (kbd "<SPC>lp") 'flycheck-previous-error)
    (global-flycheck-mode)
)
(use-package flycheck-pos-tip
  :ensure t
  :config
  (flycheck-pos-tip-mode)
)
(use-package hideshow
  :diminish hs-minor-mode
  :config
    (setq hs-allow-nesting t)
    (add-hook 'prog-mode-hook (function (lambda() (hs-minor-mode))))
)
(use-package magit
  :diminish magit-auto-revert-mode
)
(use-package neotree
  :init
    (setq neo-theme 'icons)
  :commands (neotree-toggle neotree-hide neotree-show)
  :config
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (define-key evil-normal-state-map (kbd "<SPC>t") 'neotree-toggle)
)
(use-package projectile
  :diminish projectile-mode
  :init
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)
)
(use-package counsel-projectile
  :config
    ;; (counsel-projectile-on)
    (define-key evil-normal-state-map (kbd "<SPC>b") 'counsel-projectile-switch-to-buffer)
    (define-key evil-normal-state-map (kbd "<SPC>e") 'counsel-projectile-find-file)
)
(use-package puppet-mode)
(use-package spaceline-config
  :ensure spaceline
  :config
  (setq evil-insert-state-message nil
        evil-visual-state-message nil
        evil-visual-state-tag "V"
        evil-insert-state-tag "I"
        evil-normal-state-tag "N"
  )
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
)
(use-package pyenv-mode)
(use-package pyenv-mode-auto)
(use-package rainbow-delimiters
  :config
    (add-hook 'c-mode-common-hook (function (lambda () (
      rainbow-delimiters-mode-enable))))
)
(use-package rainbow-mode)
(use-package undo-tree
  :diminish undo-tree-mode)
(use-package web-mode)
(use-package yaml-mode)

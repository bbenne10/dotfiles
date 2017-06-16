(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  :config (spaceline-all-the-icons-theme)
)
(use-package ample-theme)
(use-package anaconda-mode
  :defer t
  :commands anaconda-mode
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
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
  :config
  (define-key evil-normal-state-map (kbd "/") 'swiper)
  (define-key evil-normal-state-map (kbd "<SPC>f") 'counsel-ag-project-at-point)
  (define-key evil-normal-state-map (kbd "M-x") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "<SPC>B") 'ivy-switch-buffer)
  (define-key evil-normal-state-map (kbd "<SPC>p") 'counsel-find-file)
  )
(use-package editorconfig)
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
(use-package fic-mode)
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
)
(use-package magit
  :diminish magit-auto-revert-mode
)
(use-package neotree
  :init
  (setq neo-theme 'icons)

  :config
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (define-key evil-normal-state-map (kbd "<SPC>t") 'neotree-toggle)
)
(use-package projectile)
(use-package counsel-projectile
  :config
  (counsel-projectile-on)
  (define-key evil-normal-state-map (kbd "<SPC>b") 'counsel-projectile-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "<SPC>e") 'counsel-projectile-find-file)
)
(use-package puppet-mode)
(use-package spaceline-config
  :ensure spaceline
  :config
  (setq evil-normal-state-tag "N")
  (setq evil-insert-state-tag "I")
  (setq evil-visual-state-tag "V")
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-default-separator 'curve)
)
(use-package pyenv-mode)
(use-package pyenv-mode-auto)
(use-package rainbow-delimiters)
(use-package rainbow-mode)
(use-package undo-tree
  :diminish undo-tree-mode)
(use-package web-mode)
(use-package yaml-mode)

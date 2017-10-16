(setq package-user-dir "~/.emacs.d/elpa"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(defvar use-package-always-ensure t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'delight))
(require 'use-package)
(require 'bind-key)
(require 'delight)

(defvar default-leader-key "<XF86TouchpadOff>")
(if (string= (system-name) "Bryan-Laptop")
  (setq default-leader-key "<SPC>")
)

(use-package evil
  ;; We're first so we define the maps we override later
  :config
    (evil-mode 1)
    (evil-set-initial-state 'term-mode 'emacs)
)
(use-package general
  :config
    (global-unset-key (kbd "C-h h"))
)
(use-package all-the-icons)
(use-package zerodark-theme
  :config
    ;; (load-theme 'zerodark)
    (zerodark-setup-modeline-format)
  )
(use-package nord-theme
  :config
    (load-theme 'nord t)
)
(use-package anaconda-mode
   :after (general pyenv-mode-auto)
   :commands anaconda-mode
   :delight anaconda-mode
   :init
     (defun python-insert-trace ()
       ;; insert a line that impots pdb and sets a trace just below the current line
       (interactive)
       (move-end-of-line 1)
       (insert "\n")
       (indent-according-to-mode)
       (insert "import pdb; pdb.set_trace()"))

     (add-hook 'python-mode-hook
               (function
                (lambda () (setq evil-shift-width python-indent-offset)
                  (anaconda-mode 1))))

     (general-define-key :keymaps 'anaconda-mode-map
                         :states '(normal)
                         :prefix "C-c"
                         "g" 'anaconda-mode-find-definitions
                         "a" 'anaconda-mode-find-assignments
                         "r" 'anaconda-mode-find-references
                         "?" 'anaconda-mode-show-doc
                         "t" 'python-insert-trace)
)
(use-package company
  :delight company-mode
  :config
    (setq company-tooltip-limit 20
          company-tooltip-align-annotations t)
    (global-company-mode 1)
)
(use-package company-anaconda
  :commands (anaconda-mode)
  :init (add-to-list 'company-backends 'company-anaconda)
)
(use-package counsel
  :after (general)
  :init
  (defun counsel-ag-project-at-point ()
    (interactive)
    (counsel-ag (thing-at-point 'symbol) (projectile-project-root)))
  :config
    (general-define-key :states '(normal)
                        "/" 'swiper
                        "M-x" 'counsel-M-x)
    (general-define-key :states '(normal)
                        :prefix default-leader-key
                        "f" 'counsel-ag-project-at-point
                        "B" 'ivy-switch-buffer
                        "E" 'counsel-find-file)
  )
(use-package counsel-dash
  :after (general)
  :init
    (setq counsel-dash-docsets-path "~/.emacs.d/dash-docsets"
          counsel-dash-min-length 3
          counsel-dash-browser-func 'browse-url)

    (defun counsel-dash-at-point ()
      (interactive)
      (counsel-dash (thing-at-point 'symbol)))
  :commands (counsel-dash-activate-docset counsel-dash counsel-dash-at-point counsel-dash)
  :config
    (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
    (add-hook 'js-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript" "MomentJS "))))
    (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python3" "Flask" "Jinja"))))
    (add-hook 'scala-mode-hook (lambda () (setq-local counsel-dash-docsets '("Scala"))))
    (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C++"))))
    (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
    (add-hook 'sh-mode-hook (lambda () (setq-local counsel-dash-docsets '("Bash"))))

    (general-define-key :states '(normal)
                        :prefix default-leader-key
                        "d" 'counsel-dash-at-point
                        "D" 'counsel-dash)
  )
(use-package editorconfig)
(use-package ensime
  :commands (ensime ensime-mode)
  :init
    (setq ensime-startup-notification nil
          ensime-startup-snapshot-notification nil)

    (add-hook 'scala-mode-hook #'ensime-mode)
  :config
    (set-face-attribute 'ensime-implicit-highlight nil
                        :underline nil
                        :slant 'italic)

)
(use-package evil-matchit
  :config (global-evil-matchit-mode 1)
)
(use-package evil-numbers
  :config
    (general-define-key :states '(normal)
                        "C-a" 'evil-numbers/inc-at-pt
                        "C-x" 'evil-numbers/dec-at-pt))
(use-package evil-surround
  :config (global-evil-surround-mode 1))
(use-package evil-visual-mark-mode
  :config (evil-visual-mark-mode 1))
(use-package fic-mode
  :config (add-hook 'prog-mode-hook (function (lambda () (fic-mode 1)))))
(use-package flycheck
  :after (general)
  :delight flycheck-mode
  :commands (flycheck-mode)
  :init
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side . bottom)
                   (window-height . 0.2)))
    (defun delete-flycheck-errors-list ()
      (interactive)
      (if (get-buffer-window "*Flycheck errors*" "visible") (delete-window (get-buffer-window "*Flycheck errors*" "visible"))))
    (defun flycheck-verify-ensime ()
      "Verify the Ensime syntax checker."
      (list
       (flycheck-verification-result-new
        :label "Ensime Mode"
        :message (if ensime-mode "Enabled" "Disabled")
        :face (if ensime-mode 'success '(bold warning)))
       (flycheck-verification-result-new
        :label "Ensime connection"
        :message (if (ensime-connected-p) "open" "closed")
        :face (if (ensime-connected-p) 'success '(bold warning)))))
    (defun flycheck-ensime-parse-note (note checker)
      "Parse a single Ensime NOTE for CHECKER into an error."
      (let ((severity (plist-get note :severity)))
        (unless (symbolp severity)
          (setq severity (intern severity)))
        (flycheck-error-new-at
         (plist-get note :line)
         (plist-get note :col)
         severity (plist-get note :msg)
         :checker checker
         :filename (plist-get note :file)
         :buffer (current-buffer))))
    (defun flycheck-ensime-parse-notes (notes checker)
      "Parse Ensime NOTES for CHECKER into Flycheck errors."
      (mapcar (lambda (n) (flycheck-ensime-parse-note n checker)) notes))
    (defun flycheck-ensime-start (checker callback)
      "Start a syntax CHECKER with Ensime."
      (condition-case err
          (let* ((notes (ensime-scala-compiler-notes (ensime-connection)))
                 (errors (flycheck-ensime-parse-notes notes checker)))
            (funcall callback 'finished errors))
        (error (funcall callback 'errored (error-message-string err)))))
    (defun flycheck-ensime-setup ()
      "Setup Flycheck for Ensime."
      (interactive)
      (add-to-list 'flycheck-checkers 'scala-ensime)
      (advice-add 'ensime-make-note-overlays :override #'ignore
                  '((name . flycheck-ensime-disable-ensime-overlays))))
  :config
    (flycheck-define-generic-checker 'scala-ensime
      "A Scala syntax checker using Ensime."
      :start #'flycheck-ensime-start
      :verify #'flycheck-verify-ensime
      :modes '(scala-mode)
      :predicate (lambda () (and ensime-mode (ensime-connection-or-nil)))
      :next-checkers '((warning . scala-scalastyle)))
    ;; TODO: figure out if we can "double up" the prefix key?
    (general-define-key :prefix default-leader-key
                        :states '(normal)
                        "lo" 'flycheck-list-errors
                        "lc" 'delete-flycheck-errors-list
                        "ln" 'flycheck-next-error
                        "lp" 'flycheck-previous-error)
    (add-hook 'prog-mode-hook  (function (lambda () (flycheck-mode))))
  )
(use-package flycheck-pos-tip
   :after flycheck
   :config (flycheck-pos-tip-mode))
(use-package hideshow
  :delight hs-minor-mode
  :config
    (setq hs-allow-nesting t)
    (add-hook 'prog-mode-hook (function (lambda() (hs-minor-mode))))
  )
(use-package lua-mode)
(use-package magit
  :delight magit-auto-revert-mode
  :config
    (setq magit-popup-show-common-commands nil)
)
(use-package evil-magit
  :after magit
  :init
    (setq evil-magit-want-horizontal-movement nil))
(use-package markdown-mode
  :commands (markdown-mode)
  :config
    (setq markdown-css-paths
                 '("https://markdowncss.github.io/modest/css/modest.css"))
)
(use-package multi-term
  :config
    (setq multi-term-program "/bin/zsh")
    (add-hook 'term-mode-hook (function
                               (lambda ()
                                 (goto-address-mode)
                                 (define-key term-raw-map (kbd "C-w h") 'evil-window-left)
                                 (define-key term-raw-map (kbd "C-w j") 'evil-window-down)
                                 (define-key term-raw-map (kbd "C-w k") 'evil-window-up)
                                 (define-key term-raw-map (kbd "C-w l") 'evil-window-right))))
    (add-hook 'term-exec-hook
              (function (lambda () (set-buffer-process-coding-system
                                'utf-8-unix 'utf-8-unix))))
)
(use-package projectile
  :delight projectile-mode
  :init
    (setq projectile-completion-system 'ivy)
  :config
    (projectile-global-mode))
(use-package counsel-projectile
  :after (projectile general)
  :config
    (general-define-key :states '(normal)
                        :prefix default-leader-key
                        ;; "b" 'counsel-projectile-switch-to-buffer
                        "e" 'counsel-projectile-find-file
                        "p" 'counsel-projectile-switch-project
                        "<SPC>" 'counsel-projectile)
)
(use-package spaceline-config
  :ensure spaceline
  :config
    (setq evil-insert-state-message nil
          evil-visual-state-message nil
          evil-visual-state-tag "V"
          evil-insert-state-tag "I"
          evil-normal-state-tag "N"
          spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
          )
)
(use-package pyenv-mode
  :init
   (add-hook 'python-mode-hook (function (lambda () (pyenv-mode))))
)
(use-package pyenv-mode-auto
  :after (pyenv-mode)
)
(use-package rainbow-delimiters
  :config
  (add-hook 'c-mode-common-hook (function (lambda () (rainbow-delimiters-mode-enable))))
  (add-hook 'scala-mode-hook (function (lambda () (rainbow-delimiters-mode-enable))))
  (add-hook 'elip-mode-common-hook (function (lambda () (rainbow-delimiters-mode-enable))))
  )
(use-package rainbow-mode)
(use-package undo-tree
  :delight undo-tree-mode)
(use-package yaml-mode
  :mode ("\\.yaml'" "\\.yml'"))
(use-package yasnippet
  :delight yas-minor-mode)

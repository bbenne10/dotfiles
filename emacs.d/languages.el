(defun python-insert-trace ()
  ;; insert a line that impots pdb and sets a trace just below the current line
  (interactive)
  (move-end-of-line 1)
  (insert "\n")
  (indent-according-to-mode)
  (insert "import pdb; pdb.set_trace()")
  ;;(back-to-indentation)
)

(defun setup-python-keybinds ()
  (define-key evil-normal-state-map (kbd "C-c g") 'anaconda-mode-find-definitions)
  (define-key evil-normal-state-map (kbd "C-c a") 'anaconda-mode-find-assignments)
  (define-key evil-normal-state-map (kbd "C-c r") 'anaconda-mode-find-references)
  (define-key evil-normal-state-map (kbd "C-c ?") 'anaconda-mode-show-doc)
  (define-key evil-normal-state-map (kbd "C-c t") 'python-insert-trace)
)

(add-hook 'prog-mode-hook
          (function (lambda()
                      (hs-minor-mode)
                      (fic-mode 1))))

(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (rainbow-delimiters-mode-enable))))

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq evil-shift-width python-indent)
                      (modify-syntax-entry ?_ "w")
                      (setup-python-keybinds)
                      )))

(add-hook 'js-mode-hook
          (function (lambda ()
                      (flycheck-select-checker 'javascript-standard)
                      (setq js-indent-level 2)
                      (setq evil-shift-width 2))))

(add-hook 'c++-mode-hook
          (function (lambda ()
                      (flycheck-select-checker 'c/c++-gcc))))

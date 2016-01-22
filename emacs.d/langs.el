(add-hook 'prog-mode-hook
  (function (lambda()
              (hs-minor-mode)
              (fic-mode 1))))

(add-hook 'python-mode-hook
  (function (lambda ()
              (setq evil-shift-width python-indent)
              (modify-syntax-entry ?_ "w")
              (pyenv-use-corresponding)
              (add-to-list 'company-backends 'company-jedi)
              )))

(add-hook 'js-mode-hook
  (function (lambda ()
          (flycheck-select-checker 'javascript-standard)
          (setq js-indent-level 2)
          (setq evil-shift-width 2))))

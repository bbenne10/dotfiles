(add-hook 'js-mode-hook
          (function (lambda ()
                      (flycheck-select-checker 'javascript-standard)
                      (setq js-indent-level 2)
                      (setq evil-shift-width 2))))

(add-hook 'c++-mode-hook
          (function (lambda ()
                      (flycheck-select-checker 'c/c++-gcc))))

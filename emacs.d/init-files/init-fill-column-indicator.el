(add-hook 'after-change-major-mode-hook
        (lambda () (if (string= major-mode "web-mode")
            (turn-off-fci-mode) (turn-on-fci-mode))))

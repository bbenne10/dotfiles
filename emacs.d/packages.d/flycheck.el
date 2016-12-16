(require 'flycheck)
(flycheck-define-checker python-pylama
  "A Python syntax and style checker using the pylama utility. See URL
  `http://pypi.python.org/pypi/pylama'."

  :command ("pylama" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

;; (add-to-list 'flycheck-checkers 'python-pylama)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side . bottom)
               (window-height . 0.2)))

(add-hook 'after-init-hook #'global-flycheck-mode)

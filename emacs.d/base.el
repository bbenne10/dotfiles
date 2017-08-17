(setq ad-redefinition-action 'accept)
(setq-default truncate-lines t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 120 2))

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq prettify-symbols-unprettify-at-point t)
(prettify-symbols-mode)

;; Enable yanking and pasting to and from both clipboards
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq mouse-drag-copy-region t)

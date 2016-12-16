(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")
        ("django" . "\\.css\\'")
       )
 )

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-comment-style 2)

(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-current-column-highlight t)

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-opening t)
(setq web-mode-enable-auto-indentation t)

;; Sort of like matchit
(evil-declare-key 'normal web-mode-map (kbd "%") 'web-mode-navigate)
(evil-declare-key 'visual web-mode-map (kbd "%") 'web-mode-navigate)
(evil-declare-key 'normal web-mode-map (kbd ",c") 'web-mode-comment-or-uncomment)
(evil-declare-key 'visual web-mode-map (kbd ",c") 'web-mode-comment-or-uncomment)

(require 'spaceline-config )
(spaceline-install
 `(
   '(evil-state
     :face spaceline-highlight-face)
   '(buffer-id \(buffer-modified\))
  )

 `(
   '(flycheck-error flycheck-warning flycheck-info)
   '(major-mode minor-modes)
   version-control
   )
 )

;;(spaceline-spacemacs-theme)
(spaceline-toggle-line-column-off)
(setq powerline-default-separator nil)

(defun delete-flycheck-errors-list ()
  (interactive)
  (cond (get-buffer-window "*Flycheck errors*" "visible")
        (delete-window (get-buffer-window "*Flycheck-errors*" "visible")))
  )

(define-key evil-normal-state-map (kbd "<SPC>lo") 'flycheck-list-errors)
(define-key evil-normal-state-map (kbd "<SPC>lc") 'delete-flycheck-errors-list)
(define-key evil-normal-state-map (kbd "<SPC>ln") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "<SPC>lp") 'flycheck-previous-error)

(define-key evil-normal-state-map (kbd "<SPC>w") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "<SPC>e") 'helm-projectile-find-file)

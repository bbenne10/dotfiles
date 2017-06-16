(defun counsel-ag-project-at-point ()
    "use counsel ag to search for the word at point in the project"
    (interactive)
    (counsel-ag (thing-at-point 'symbol) (projectile-project-root)))

(defun delete-flycheck-errors-list ()
  (interactive)
  (if (get-buffer-window "*Flycheck errors*" "visible")
        (delete-window (get-buffer-window "*Flycheck errors*" "visible")))
)

(define-key evil-normal-state-map (kbd "/") 'swiper)
(define-key evil-normal-state-map (kbd "<SPC>f") 'counsel-ag-project-at-point)
(define-key evil-normal-state-map (kbd "M-x") 'counsel-M-x)

;; handle opening and closing the "quick fix" window for flycheck
(define-key evil-normal-state-map (kbd "<SPC>lo") 'flycheck-list-errors)
(define-key evil-normal-state-map (kbd "<SPC>lc") 'delete-flycheck-errors-list)
(define-key evil-normal-state-map (kbd "<SPC>ln") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "<SPC>lp") 'flycheck-previous-error)

;; bindings for file/buffer/project management
(define-key evil-normal-state-map (kbd "<SPC>b") 'counsel-projectile-switch-to-buffer)
(define-key evil-normal-state-map (kbd "<SPC>B") 'ivy-switch-buffer)

(define-key evil-normal-state-map (kbd "<SPC>p") 'counsel-find-file)

(define-key evil-normal-state-map (kbd "<SPC>e") 'counsel-projectile-find-file)

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

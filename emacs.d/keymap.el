(defun delete-flycheck-errors-list ()
  (interactive)
  (if (get-buffer-window "*Flycheck errors*" "visible")
        (delete-window (get-buffer-window "*Flycheck errors*" "visible")))
)

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

(define-key evil-normal-state-map (kbd "/") 'swiper)
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

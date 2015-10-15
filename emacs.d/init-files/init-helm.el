(defun minibuffer-keyboard-quit()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; Make escape close most buffers
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)

; Now define some 'personal' keybinds
(define-key evil-normal-state-map (kbd "<SPC>w") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "<SPC>e") 'helm-find-files)

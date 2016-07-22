(require 'evil)
(evil-mode 1)

;; (define-key evil-normal-state-map "za" nil)
;; (define-key evil-normal-state-map "zA" nil)
;;
;; (define-key evil-normal-state-map "zo" #'evil-open-fold)
;; (evil-define-command evil-open-fold ()
;;   "Open one fold under the cursor."
;;   (outline-minor-mode)
;;   (outline-show-children))
;;
;; (define-key evil-normal-state-map "zc" #'evil-close-fold)
;; (evil-define-command evil-close-fold ()
;;   "Close one fold under the cursor."
;;   (outline-minor-mode)
;;   (outline-hide-subtree))
;;
;; (define-key evil-normal-state-map "zO" #'evil-open-folds-at-point)
;; (evil-define-command evil-open-folds-at-point ()
;;   "Open all folds under the cursor recursively."
;;   (outline-minor-mode)
;;   (outline-show-subtree))
;;
;; (define-key evil-normal-state-map "zC" #'evil-close-folds-at-point)
;; (evil-define-command evil-close-folds-at-point ()
;;   "Close all folds under the cursor recursively."
;;   (outline-minor-mode)
;;   (hide-subtree))
;;
;; (define-key evil-normal-state-map "zM" #'evil-close-all-folds)
;; (evil-define-command evil-close-all-folds ()
;;   "Close all folds."
;;   (outline-minor-mode)
;;   (hide-sublevels 1))
;;
;; (define-key evil-normal-state-map "zR" #'evil-open-all-folds)
;; (evil-define-command evil-open-all-folds ()
;;   "Open all folds."
;;   (outline-minor-mode)
;;   (show-all))
;;
;; (define-key evil-normal-state-map "zm" #'evil-fold-more)
;; (evil-define-command evil-fold-more ()
;;   "Fold more."
;;   (outline-minor-mode)
;;   (when (> evil-fold-level 0)
;;     (decf evil-fold-level)
;;     (hide-sublevels (+ evil-fold-level 1))))
;;
;; (define-key evil-normal-state-map "zr" #'evil-fold-less)
;; (evil-define-command evil-fold-less ()
;;   "Reduce folding."
;;   (outline-minor-mode)
;;   (incf evil-fold-level)
;;   (hide-sublevels (+ evil-fold-level 1)))

(setq default-frame-alist '((font . "Hasklig-10")))

(defun my-correct-symbol-bounds (pretty-alist)
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))
(defun my-ligature-list (ligatures codepoint-start)
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))
(setq my-hasklig-ligatures
      (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                     "==" "===" "==>" "=>" "=<<" "!!" ">>"
                     ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                     "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                     "<<" "<<<" "<+>" ".." "..." "++" "+++"
                     "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
        (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

(defun my-set-hasklig-ligatures ()
  (setq prettify-symbols-alist
        (append my-hasklig-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))

(add-hook 'prog-mode-hook (function (lambda ()
            (my-set-hasklig-ligatures)
            (setq-local display-line-numbers t))))

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

(show-paren-mode 1)
(global-hl-line-mode 1)

(set-face-attribute 'line-number-current-line nil
                    :font "Hasklig-10:weight=Bold"
                    :inverse-video nil)

(setq initial-scratch-message ""
      inhibit-startup-message t
      scroll-conservatively 101
      scroll-margin 5
      visible-bell t
      ad-redefinition-action 'accept)
(setq-default fill-column 80
              truncate-lines t
              indent-tabs-mode nil
              tab-width 2
              tab-stop-list (number-sequence 3 120 2))

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq prettify-symbols-unprettify-at-point t)
(prettify-symbols-mode)

;; Enable yanking and pasting to and from both clipboards
(setq select-enable-clipboard t
      select-enable-primary t
      mouse-drag-copy-region t)

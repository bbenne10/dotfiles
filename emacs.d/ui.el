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

(add-hook 'prog-mode-hook 'my-set-hasklig-ligatures)

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

(global-linum-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

(setq-default fill-column 80)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq scroll-conservatively 101)
(setq scroll-margin 5)

(setq visible-bell t)

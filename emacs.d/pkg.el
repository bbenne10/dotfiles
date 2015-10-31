(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-user-package-directory "~/.emacs.d/init-files")

(el-get-bundle "evil")
(el-get-bundle "evil-surround")
(el-get-bundle "helm")
(el-get-bundle "helm-projectile")
(el-get-bundle "projectile")
(el-get-bundle "emacs-theme-gruvbox"
  :type "git"
  :url "https://github.com/greduan/emacs-theme-gruvbox"
)
(el-get-bundle "fill-column-indicator")
(el-get-bundle "flycheck")
(el-get-bundle "smooth-scrolling")

(el-get-bundle "org-mode")
(el-get-bundle "evil-org-mode")

(el-get-bundle "web-mode")

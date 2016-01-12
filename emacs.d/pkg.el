(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-user-package-directory "~/.emacs.d/init-files")


(el-get-bundle "company-jedi")
(el-get-bundle "jedi")
(el-get-bundle "evil")
(el-get-bundle "evil-org-mode")
(el-get-bundle "evil-plugins"
  :features (evil-operator-comment)
)
(el-get-bundle "evil-surround")
(el-get-bundle "fic-mode")
(el-get-bundle "helm")
(el-get-bundle "gruvbox-theme"
  :type "git"
  :url "https://github.com/greduan/emacs-theme-gruvbox"
)
(el-get-bundle "material-theme"
  :type "git"
  :url "https://github.com/cpaulik/emacs-material-theme"
)
(el-get-bundle "fill-column-indicator")
(el-get-bundle "flycheck")
(el-get-bundle "org-mode")
(el-get-bundle "evil-org-mode")
(el-get-bundle "projectile")
(el-get-bundle "grizzl")
(el-get-bundle "puppet-mode")
(el-get-bundle "smooth-scrolling")
(el-get-bundle "org-mode")
(el-get-bundle "powerline")
(el-get-bundle "spaceline"
  :type "git"
  :url "https://github.com/TheBB/spaceline.git"
  :features "spaceline-config"
  :depends s
)
(el-get-bundle "diminish")

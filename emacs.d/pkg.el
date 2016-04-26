(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-user-package-directory "~/.emacs.d/packages.d")

(el-get-bundle "ample-theme"
  :type "git"
  :url "https://github.com/jordonbiondo/ample-theme"
)
(el-get-bundle "anaconda-mode")
(el-get-bundle "diminish")
(el-get-bundle "dired+")
(el-get-bundle "evil")
(el-get-bundle "evil-matchit")
(el-get-bundle "evil-org-mode")
(el-get-bundle "evil-plugins"
  :features (evil-operator-comment)
)
(el-get-bundle "evil-surround")
(el-get-bundle "fic-mode")
(el-get-bundle "flycheck")
(el-get-bundle "flx-ido")
(el-get-bundle "origami"
  :type "github"
  :pkgname "gregsexton/origami.el"
  :depends (dash s)
)
(el-get-bundle "org-mode")
(el-get-bundle "php-mode")
(el-get-bundle "projectile")
(el-get-bundle "puppet-mode")
(el-get-bundle "smooth-scrolling")
(el-get-bundle "spaceline"
  :type "github"
  :pkgname "TheBB/spaceline"
  :features "spaceline-config"
  :depends (s dash powerline)
)

(el-get-bundle "company-mode")
(el-get-bundle "company-jedi"
  :type "github"
  :pkgname "syohex/emacs-company-jedi"
  :depends (jedi-core)
)
(el-get-bundle "pyenv"
  :type "github"
  :pkgname "cyberved/pyenv.el"
)


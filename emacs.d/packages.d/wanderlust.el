(setq wl-folders-file "~/.emacs.d/packages.d/wanderlust_folders")
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(setq wl-message-ignored-field-list '("^.*:")
      wl-message-sort-field-list '("^From:" "^Subject:" "^Date:" "^To:" "^Cc:"))
      wl-message-visible-field-list '("^To:" "^Cc:" "^From:" "^Subject:" "^Date:"))
      wl-default-spec "%"
      wl-draft-folder "%Drafts"
      wl-trash-folder "%Trash"
      wl-folder-check-async t
      mime-edit-split-message nil
      elmo-message-fetch-confirm nil
      wl-summary-line-format "%n%T%P %W %M/%D %h:%m %t%[%17(%c %f%) %] %s"
      wl-summary-search-parent-by-subject-regexp nil
      mime-view-buttons-visible nil
      wl-fcc-force-as-read t
      wl-folder-check-async t
      wl-folder-window-width 25
      wl-summary-width 150
      ;; wl-icon-directory "~/.emacs.d/el-get/wanderlust/icons"
      wl-insert-message-id-domain nil
      wl-default-spec "%"
      wl-summary-showto-folder-regexp ".*Sent.*"
      ;;wl-user-mail-address-list  '("tmearnest@gmail.com"
      ;;                             "earnest3@illinois.edu"
      ;;                             "earnest3@uiuc.edu")
      wl-stay-folder-window t)

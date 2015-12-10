(defun start-jedi()
  (add-to-list 'company-backends 'company-jedi)
)

(add-hook 'python-mode-hook 'start-jedi)

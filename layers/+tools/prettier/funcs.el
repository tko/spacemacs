(defun spacemacs/prettier-setup-command ()
  (when-let ((found (spacemacs/node-executable-find "prettier")))
    (setq-local prettier-js-command found)))

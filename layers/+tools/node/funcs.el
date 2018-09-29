;;; funcs.el --- node  Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Tommi Komulainen <tommi.komulainen@iki.fi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/node-executable-find (command &rest extra-modules)
  "Search for an executable named COMMAND and return the absolute file name of
the executable. This function searches directories \"node_modules/.bin\",
\"node_modules/MODULE/node_modules/.bin\" for each extra module in
EXTRA-MODULES, and the directories searched by `executable-find'."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (node_modules (expand-file-name "node_modules" root))
         (bindirs (nconc
                   (list
                    ;; node_modules/.bin/{command}
                    ".bin"
                    ;; node_modules/{command}/bin/{command}
                    ;; (format "%s/bin" command)
                    )
                   ;; node_modules/{moduleN}/node_modules/.bin/{command}
                   (--map (f-join it "node_modules" ".bin") extra-modules))))
    (or
     (dolist (bindir bindirs)
       (let ((path (f-join node_modules bindir command)))
         (when (file-executable-p path) (return path))))
     (executable-find command))))

(defun spacemacs/node-use-eslint-from-node-modules ()
  (let ((eslint (spacemacs/node-executable-find "eslint")))
    (when eslint (setq-local flycheck-javascript-eslint-executable eslint))))

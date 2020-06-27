;;; funcs.el --- Go Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//go-backend ()
  "Returns selected backend."
  (if go-backend
      go-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'go-mode))))

(defun spacemacs//go-setup-backend ()
  "Conditionally setup go backend"
  (pcase (spacemacs//go-backend)
    ('lsp (spacemacs//go-setup-backend-lsp))))

(defun spacemacs//go-setup-company ()
  "Conditionally setup go company based on backend"
  (pcase (spacemacs//go-backend)
    ('go-mode (spacemacs//go-setup-company-go))))

(defun spacemacs//go-setup-eldoc ()
  "Conditionally setup go eldoc based on backend"
  (pcase (spacemacs//go-backend)
    ('go-mode (go-eldoc-setup))))


(defun spacemacs//go-setup-dap ()
  "Conditionally setup go DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//go-backend)
    (`lsp (spacemacs//go-setup-lsp-dap))))


;; go-mode

(defun spacemacs//go-setup-company-go ()
  (spacemacs|add-company-backends
    :backends company-go
    :modes go-mode
    :variables company-go-show-annotation t
    :append-hooks nil
    :call-hooks t)
  (company-mode))


;; lsp

(defun spacemacs//go-setup-backend-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; without setting lsp-diagnostic-package to :none
        ;; golangci-lint errors won't be reported
        (when go-use-golangci-lint
          (message "[go] Setting lsp-diagnostic-package :none to enable golangci-lint support.")
          (setq-local lsp-diagnostic-package :none))
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//go-setup-dap ()
  "Conditionally setup go DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//go-backend)
    (`lsp (spacemacs//go-setup-lsp-dap))))

(defun spacemacs//go-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-go)
  (dap-go-setup))


;; flycheck

(defun spacemacs//go-enable-flycheck-golangci-lint ()
  "Enable `flycheck-golangci-linter' and disable overlapping `flycheck' linters."
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     ;; go-build
                                     ;; go-test
                                     go-errcheck
                                     go-staticcheck
                                     go-unconvert))
  (flycheck-golangci-lint-setup)

  ;; Make sure to only run golangci after go-build
  ;; to ensure we show at least basic errors in the buffer
  ;; when golangci fails. Make also sure to run go-test if possible.
  ;; See #13580 for details
  (flycheck-add-next-checker 'go-build '(warning . golangci-lint) t)
  (flycheck-add-next-checker 'go-test '(warning . golangci-lint) t)

  ;; Set basic checkers explicitly as flycheck will
  ;; select the better golangci-lint automatically.
  ;; However if it fails we require these as fallbacks.
  (cond ((flycheck-may-use-checker 'go-test) (flycheck-select-checker 'go-test))
        ((flycheck-may-use-checker 'go-build) (flycheck-select-checker 'go-build))))


;; run

(defun spacemacs/go-run-tests (args)
  (interactive)
  (compilation-start (concat "go test " (when go-test-verbose "-v ") args " " go-use-test-args)
                     nil (lambda (n) go-test-buffer-name) nil))

(defun spacemacs/go-run-package-tests ()
  (interactive)
  (spacemacs/go-run-tests ""))

(defun spacemacs/go-run-package-tests-nested ()
  (interactive)
  (spacemacs/go-run-tests "./..."))

(defun spacemacs//go--function-regex (&optional test)
  ;; func TestIt
  ;; func (x X) TestIt
  ;; func (x *X) TestIt
  (rx-to-string
   '(: bol "func" (1+ " ")
       (? (seq
           "("
           (*? alnum) (? " ") (? "*")
           (group (1+ alnum)) ; group 1: receiver type / suite
           ")"
           (1+ " ")))
       (group ; group 2: function name
        (eval (if test "Test" 'upper))
        (1+ (any "_" alnum))
        ))))

(defun spacemacs//go--current-function-name (&optional test)
  (save-excursion
    (re-search-backward (spacemacs//go--function-regex test)))
  (list
   (match-string-no-properties 1) ; suite
   (if test
       (match-string-no-properties 2) ; filename starting with Test
     (concat "Test" (match-string-no-properties 2)) ; filename
   )))

(defun spacemacs//go--get-test-file-buffer ()
  (if (string-match "_test\\.go" buffer-file-name)
      (current-buffer)
    (let ((ff-always-try-to-create nil)
          (ff-quiet-mode t)
          (inhibit-message t))
      (when-let ((filename (ff-other-file-name)))
        (find-file-noselect filename t)))))

(defun spacemacs//go--list-functions ()
  (when-let ((buffer (spacemacs//go--get-test-file-buffer)))
    (with-current-buffer buffer
        (save-excursion

          (goto-char (point-min))

          (let ((regex (spacemacs//go--function-regex t))
                fns)
            (while (re-search-forward regex nil t)
              (let ((name (match-string-no-properties 2)))
                (push name fns)))
            fns)))))

(defun spacemacs/go-run-test-current-function ()
  (interactive)
  (let ((is-test (string-match "_test\\.go" buffer-file-name)))
    (pcase-let ((`(,suite ,filename) (spacemacs//go--current-function-name is-test)))
      (spacemacs/go-run-tests
       (cond (go-use-testify-for-testing (format "-run='Test%s' -testify.m='%s'" suite filename))
             (go-use-gocheck-for-testing (format "-check.f='%s'" filename))
             (t (format "-run='^%s$'" filename)))))))

(defun spacemacs/go-run-test-current-suite ()
  (interactive)
  (if (or go-use-gocheck-for-testing go-use-testify-for-testing)
      (let ((is-test (string-match "_test\\.go" buffer-file-name)))
        (pcase-let ((`(,suite ,filename) (spacemacs//go--current-function-name is-test)))
          (spacemacs/go-run-tests
           (cond (go-use-testify-for-testing (format "-run='Test%s'" suite))
                 (go-use-gocheck-for-testing (format "-check.f='%s'" suite))))))
    (message "Testify or Gocheck is needed to test the current suite")))

(defun spacemacs/go-run-test-current-file ()
  (interactive)
  (when-let ((fns (spacemacs//go--list-functions))
             (regex (format "^%s$" (string-join fns "|"))))
    (spacemacs/go-run-tests
     (cond (go-use-testify-for-testing (format "-run='%s' -testify.m='%s'" regex regex))
           (go-use-gocheck-for-testing (format "-check.f='%s'" regex))
           (t (format "-run='%s'" regex))))))

(defun spacemacs/go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s %s"
           (shell-quote-argument (or (file-remote-p (buffer-file-name (buffer-base-buffer)) 'localname)
                                     (buffer-file-name (buffer-base-buffer))))
           go-run-args)))


;; misc

(defun spacemacs/go-packages-gopkgs ()
  "Return a list of all Go packages, using `gopkgs'."
  (sort (process-lines "gopkgs") #'string<))

(defun spacemacs//go-set-tab-width ()
  "Set the tab width."
  (when go-tab-width
    (setq-local tab-width go-tab-width)))

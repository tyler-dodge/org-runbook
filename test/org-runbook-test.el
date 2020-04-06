;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org)
(require 'el-mock)

(when (require 'undercover nil t)
  (undercover "*.el"))
(require 'org-runbook (expand-file-name "org-runbook.el"))

(ert-deftest org-runbook-exists ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'org-runbook-execute)))

(ert-deftest org-runbook--validate-command ()
  "Tests to verify validation"
  (should (org-runbook--validate-command (org-runbook-command-create)))
  (should-error (org-runbook--validate-command nil))
  (should-error (org-runbook--validate-command "test")))

(ert-deftest org-runbook-execute-no-commands ()
  "org-runbook-execute should throw an error when no commands are available"
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "no-commands"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "no-commands"))
    (org-runbook--output-configuration)
    (should-error (org-runbook-execute))))

(ert-deftest org-runbook-execute-one-command ()
  "org-runbook-execute should execute the command referenced in the corresponding org file."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-execute-command-action #'org-runbook-command-execute-message)
    (org-runbook--output-configuration)
    (setq-local completing-read-function (lambda (_ collection &rest _) (-some-> collection ht-keys first)))
    (should (org-runbook-execute))
    (should (string= (org-runbook-command-full-command org-runbook-command-last-command) "echo test"))))

(ert-deftest org-runbook-view-one-command ()
  "org-runbook-execute should execute the command referenced in the corresponding org file."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-execute-command-action #'org-runbook-command-execute-message)
    (save-window-excursion
      (org-runbook-switch-to-major-mode-file))
    (org-runbook--output-configuration)
    (setq-local completing-read-function (lambda (_ collection &rest _) (-some-> collection ht-keys first)))
    (org-runbook-view)
    (should (eq (get-buffer org-runbook-view-mode-buffer) (current-buffer)))
    (goto-char (point-min))
    (should (re-search-forward "echo test" nil t))
    (org-runbook-view--open-at-point)
    (should (s-contains-p "fundamental-mode.org" (buffer-file-name)))
    (goto-char (point-min))
    (should (re-search-forward "echo test" nil t))))
(defun -message (&rest body)
  (if (eq (length body) 1)
      (prog1 (car body) (message "%s" (car body)))
    (prog1 (car (last body)) (apply 'message body))))

(ert-deftest org-runbook-execute-command-from-org-runbook-files ()
  "org-runbook-execute should execute the command referenced in the corresponding org file."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-execute-command-action #'org-runbook-command-execute-message)
    (setq-local org-runbook-files (list (relative-to-test-directory "test-runbook.org")))
    (org-runbook--output-configuration)
    (setq-local completing-read-function (lambda (_ collection &rest _)
                                           (-some->> collection (ht-keys)
                                                     (--first (string= it "Test Data 2 >> Test Data B")))))
    (should (org-runbook-execute))
    (should (string= (org-runbook-command-full-command org-runbook-command-last-command) "echo test-runbook-2-B"))))

(ert-deftest org-runbook-goto-one-command ()
  "org-runbook-execute should execute the command referenced in the corresponding org file."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-execute-command-action #'org-runbook-command-execute-message)
    (org-runbook--output-configuration)
    (setq-local completing-read-function (lambda (_ collection &rest _) (-some-> collection ht-keys first)))
    (org-runbook-goto)
    (should (s-contains-p "fundamental-mode.org" (buffer-file-name)))
    (goto-char (point-min))
    (should (re-search-forward "echo test" nil t))))

(ert-deftest org-runbook-switch-to-file-functions ()
  "org-runbook-switch-to-* functions should work correctly"
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (let ((expected-file-name (expand-file-name (f-join org-runbook-modes-directory "fundamental-mode.org"))))
      (org-runbook-switch-to-major-mode-file)
      (should (string= (buffer-file-name) expected-file-name)))
    (let ((expected-file-name (expand-file-name (f-join org-runbook-project-directory "project-file.org")))
          (projectile-project-function (symbol-function #'projectile-project-name)))
      (with-mock
        (stub projectile-project-name => "project-file")
        (org-runbook-switch-to-projectile-file)
        (should (string= (buffer-file-name) expected-file-name))))))

(ert-deftest org-runbook-execute-shell-functions ()
  "Test org-runbook-execute-eshell and org-runbook-execute-shell."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (org-runbook-command-execute-eshell nil))
    (should-error (org-runbook-command-execute-shell nil))
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-execute-command-action #'org-runbook-command-execute-shell)
    (org-runbook--output-configuration)
    (setq-local completing-read-function (lambda (_ collection &rest _) (-some-> collection ht-keys first)))
    (with-mock
      (mock (async-shell-command "echo test" "*Test*") => t :times 1)
      (should (org-runbook-execute)))

    (setq-local org-runbook-execute-command-action #'org-runbook-command-execute-eshell)
    (with-mock
      (mock (eshell-command "echo test") => t :times 1)
      (should (org-runbook-execute)))))

(provide 'org-runbook-test)

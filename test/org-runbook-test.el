;;; -*- lexical-binding: t -*-

(require 'org-runbook (expand-file-name "org-runbook.el") t)
(ert-deftest org-runbook-exists ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'org-runbook-execute)))

(defun relative-to-test-directory (file)
  (->
   (or (-some--> (and (f-exists-p "test") "test")
         (f-join it file))
       file)
   expand-file-name))

(ert-deftest org-runbook-execute-no-commands ()
  "org-runbook-execute should throw an error when no commands are available"
  (with-temp-buffer
    (setq-local org-runbook-modes-directory (relative-to-test-directory "no-commands"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "no-commands"))
    (should-error (org-runbook-execute))))

(ert-deftest org-runbook-execute-one-command ()
  "org-runbook-execute should execute the command referenced in the correspoding org file."
  (with-temp-buffer
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local
     completing-read-function
     (lambda (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
       (-> collection ht-keys first)))
    (should (org-runbook-execute))))

(ert-deftest org-runbook-view-one-command ()
  "org-runbook-execute should execute the command referenced in the correspoding org file."
  (with-temp-buffer
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local
     completing-read-function
     (lambda (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
       (-> collection ht-keys first)))
    (org-runbook-view)
    (should (eq (get-buffer org-runbook-view-mode-buffer) (current-buffer)))
    (goto-char (point-min))
    (should (re-search-forward "echo test" nil t))
    (org-runbook-view--open-at-point)
    (should (s-contains-p "fundamental-mode.org" (buffer-file-name)))
    (goto-char (point-min))
    (should (re-search-forward "echo test" nil t))))

(ert-deftest org-runbook-goto-one-command ()
  "org-runbook-execute should execute the command referenced in the correspoding org file."
  (with-temp-buffer
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (setq-local
     completing-read-function
     (lambda (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
       (-> collection ht-keys first)))
    (org-runbook-goto)
    (should (s-contains-p "fundamental-mode.org" (buffer-file-name)))
    (goto-char (point-min))
    (should (re-search-forward "echo test" nil t))))

(provide 'org-runbook-test)

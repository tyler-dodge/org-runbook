;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org)

(when (require 'undercover nil t)
  (undercover))
(require 'org-runbook (expand-file-name "org-runbook.el"))

(ert-deftest org-runbook-exists ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'org-runbook-execute)))

(defun relative-to-test-directory (file)
  (->
   (or (-some--> (and (f-exists-p (expand-file-name "test")) "test")
         (f-join it file))
       file)
   expand-file-name))

(defvar org-runbook-command-last-command nil "test variable for `org-runbook-command-execute-message'")
(defun org-runbook-command-execute-message (command)
  "Stubbed out execute-message function. formats COMMAND and outputs as a message. 
Also sets `org-runbook-command-last-command'"
  (org-runbook--validate-command command)
  (setq org-runbook-command-last-command command)
  (pcase-let (((cl-struct org-runbook-command full-command) command))
    (message "%s" full-command)))

(defun org-runbook--output-configuration ()
  (message "modes directory: %s, project directory: %s, org-version: %s"
           org-runbook-modes-directory
           org-runbook-project-directory
           (org-version)))

(ert-deftest org-runbook-execute-no-commands ()
  "org-runbook-execute should throw an error when no commands are available"
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "no-commands"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "no-commands"))
    (org-runbook--output-configuration)
    (should-error (org-runbook-execute))))

(defun org-runbook--test-first-target ()
  (->> (org-runbook-targets)
       (-map #'org-runbook-file-targets)
       (-flatten)
       (car)))

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

(provide 'org-runbook-test)

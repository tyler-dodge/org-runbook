;;; -*- lexical-binding: t -*-

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
    (message "%s" full-command)
    t))

(defun org-runbook--output-configuration ()
  (message "modes directory: %s, project directory: %s, org-version: %s"
           org-runbook-modes-directory
           org-runbook-project-directory
           (org-version)))

(defun org-runbook--test-first-target ()
  (->> (org-runbook-targets)
       (-map #'org-runbook-file-targets)
       (-flatten)
       (car)))

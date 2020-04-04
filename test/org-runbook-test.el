;;; -*- lexical-binding: t -*-

(require 'org-runbook (expand-file-name "org-runbook.el") t)

(defmacro with-completing-read (override &rest prog)
  (declare (indent 1))
  `(let ((completing-read (symbol-function 'org-runbook--completing-read))
         (execute-command-action org-runbook-execute-command-action))
     (unwind-protect
         (progn
           (setq org-runbook-execute-command-action #'org-runbook-command-execute-message)
           (fset 'org-runbook--completing-read ,override)
           ,@prog)
       (setq org-runbook-execute-command-action execute-command-action)
       (fset 'org-runbook--completing-read completing-read))))

(ert-deftest org-runbook-exists ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'org-runbook-execute)))

(defun relative-to-test-directory (file)
  (->
   (or (-some--> (and (f-exists-p (expand-file-name "test")) "test")
         (f-join it file))
       file)
   expand-file-name))

(defun org-runbook-command-execute-message (command)
  (org-runbook--validate-command command)
  (pcase-let (((cl-struct org-runbook-command full-command) command))
    (message "%s" full-command)))

(defun org-runbook--output-configuration ()
  (message "modes directory: %s, project directory: %s"
           org-runbook-modes-directory
           org-runbook-project-directory))

(ert-deftest org-runbook-execute-no-commands ()
  "org-runbook-execute should throw an error when no commands are available"
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "no-commands"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "no-commands"))
    (org-runbook--output-configuration)
    (should-error (org-runbook-execute))))

(defun org-runbook--test-first-target ()
  (->> (org-runbook-commands)
       (-map #'org-runbook-file-targets)
       (-flatten)
       (car)))

(ert-deftest org-runbook-execute-one-command ()
  "org-runbook-execute should execute the command referenced in the corresponding org file."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (org-runbook--output-configuration)
    (with-completing-read #'org-runbook--test-first-target
      (should (org-runbook-execute)))))

(ert-deftest org-runbook-view-one-command ()
  "org-runbook-execute should execute the command referenced in the corresponding org file."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (org-runbook--output-configuration)
    (with-completing-read #'org-runbook--test-first-target
      (org-runbook-view)
      (should (eq (get-buffer org-runbook-view-mode-buffer) (current-buffer)))
      (goto-char (point-min))
      (should (re-search-forward "echo test" nil t))
      (org-runbook-view--open-at-point)
      (should (s-contains-p "fundamental-mode.org" (buffer-file-name)))
      (goto-char (point-min))
      (should (re-search-forward "echo test" nil t)))))

(ert-deftest org-runbook-goto-one-command ()
  "org-runbook-execute should execute the command referenced in the corresponding org file."
  (with-temp-buffer
    (fundamental-mode)
    (setq-local org-runbook-modes-directory (relative-to-test-directory "one-command"))
    (setq-local org-runbook-project-directory (relative-to-test-directory "one-command"))
    (org-runbook--output-configuration)
    (with-completing-read #'org-runbook--test-first-target
      (org-runbook-goto)
      (should (s-contains-p "fundamental-mode.org" (buffer-file-name)))
      (goto-char (point-min))
      (should (re-search-forward "echo test" nil t)))))

(provide 'org-runbook-test)

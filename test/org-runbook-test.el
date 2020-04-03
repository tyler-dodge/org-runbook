;;; -*- lexical-binding: t -*-

(require 'org-runbook (expand-file-name "org-runbook.el") t)
(ert-deftest org-runbook-exists ()
  "Sanity check to make sure expected symbols are exported."
  (should (fboundp 'org-runbook-execute)))

(provide 'org-runbook-test)

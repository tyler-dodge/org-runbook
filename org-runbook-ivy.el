;;; org-runbook-ivy.el --- Ivy Extension for Org mode for runbooks -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 1.1
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;;
;;; Code:

(require 'org-runbook)
(require 'ivy)


;;;###autoload
(defun org-runbook-ivy (arg)
    "Prompt for command completion and execute the selected command.
Given a prefix ARG, this shows all available commands.

The rest of the interactive commands are accesible through this via
the extra actions. See `ivy-dispatching-done'."
    (interactive "P")
    (if arg (org-runbook-search)
      (ivy-read "Command"
                (cl-loop for target in (org-runbook-targets)
                         append
                         (->> target
                              (org-runbook-file-targets)
                              (-map #'org-runbook-target--to-ivy-target)))
                :action 'org-runbook-multiaction
                :caller 'org-runbook-ivy)))

;;;###autoload
(defun org-runbook-search ()
  "Lookup the targets in all known `org-runbook' files."
  (interactive)
  (ivy-read "Target"
            (cl-loop for target in (org-runbook-all-targets)
                     collect (org-runbook-target--to-ivy-target target t))
            :caller 'org-runbook-search
            :action 'org-runbook-multiaction))

(defun org-runbook-target--to-ivy-target (target &optional include-file-name-p)
  "Convert a `org-runbook-target' TARGET into a cons cell for use with ivy.
When INCLUDE-FILE-NAME-P is non-nil, cdr will be suffixed TARGET's target-buffer file name."
  (--> target
       (cons (concat
              (->> it (org-runbook-command-target-name))
              (when include-file-name-p
                (concat " - "
                        (substring-no-properties
                         (buffer-file-name (org-runbook-command-target-buffer it))))))
             it)))

(defun org-runbook-multiaction (x)
  "Add X to list of selected buffers `swiper-multi-buffers'.
If X is already part of the list, remove it instead.  Quit the selection if
X is selected by either `ivy-done', `ivy-alt-done' or `ivy-immediate-done',
otherwise continue prompting for buffers."
  (cond
   ((eq this-command 'ivy-done) (org-runbook-execute-target-action (cdr x)))
   (t (org-runbook-view-target-action (cdr x)))))

(cl-loop
 for command in (list 'org-runbook-ivy 'org-runbook-search)
 do
 (ivy-set-actions
  command
  `(
    ("o" org-runbook-multiaction "Execute Target")
    ("g" (lambda (target) (org-runbook-goto-target-action (cdr target))) "Goto Target")
    ("p" (lambda (&rest arg) (org-runbook-switch-to-projectile-file)) "Switch to Projectile File")
    ("y" (lambda (&rest arg) (org-runbook-switch-to-major-mode-file)) "Switch to Major Mode File")
    ("v" (lambda (target) (org-runbook-view-target-action (cdr target))) "View Target"))))

(provide 'org-runbook-ivy)
;;; org-runbook-ivy.el ends here

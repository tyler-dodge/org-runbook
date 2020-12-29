;;; org-runbook-ivy.el --- Ivy Extension for Org mode for runbooks -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 1.0
;; Keywords: convenience, processes, terminals, files
;; Package-Requires: ((emacs "25.1") (seq "2.3") (f "0.20.0") (s "1.12.0") (dash "2.17.0") (mustache "0.24") (ht "0.9") (ivy "0.13.0"))
;; URL: https://github.com/tyler-dodge/org-runbook
;; Git-Repository: git://github.com/tyler-dodge/org-runbook.git
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

;;;
;;;
;;; Code:

(require 'org-runbook)
(require 'ivy)


;;;###autoload
(defun org-runbook-ivy ()
    "Prompt for command completion and execute the selected command.
The rest of the interactive commands are accesible through this via
the extra actions. See `ivy-dispatching-done'."
    (interactive)
    (ivy-read "Command"
              (->> (org-runbook-targets)
                   (--map (->> it (org-runbook-file-targets)))
                   (-flatten)
                   (--map (cons (->> it (org-runbook-command-target-name)) it)))
              :action 'org-runbook-multiaction
              :caller 'org-runbook-ivy))

  (defun org-runbook-multiaction (x)
    "Add X to list of selected buffers `swiper-multi-buffers'.
If X is already part of the list, remove it instead.  Quit the selection if
X is selected by either `ivy-done', `ivy-alt-done' or `ivy-immediate-done',
otherwise continue prompting for buffers."
    (cond ((or (eq this-command 'ivy-toggle-calling)
               (eq this-command 'ivy-next-line)
               (eq this-command 'ivy-previous-line))
           (org-runbook-view-target-action (cdr x)))
          (t (message "%S" this-command) (org-runbook-execute-target-action (cdr x)))))

(ivy-set-actions
   'org-runbook-ivy
   `(
     ("o" org-runbook-multiaction "Execute Target")
     ("g" (lambda (target) (org-runbook-goto-target-action (cdr target))) "Goto Target")
     ("p" (lambda (&rest arg) (org-runbook-switch-to-projectile-file)) "Switch to Projectile File")
     ("y" (lambda (&rest arg) (org-runbook-switch-to-major-mode-file)) "Switch to Major Mode File")
     ("v" (lambda (target) (org-runbook-view-target-action (cdr target))) "View Target")))

(provide 'org-runbook-ivy)

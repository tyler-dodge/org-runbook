;;; org-runbook.el --- org mode for runbooks. -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 1.0
;; Package-Requires: ((emacs "24") (seq "2.3") (f "0.20.0") (s "1.12.0") (dash "2.17.0"))
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; 
;;; 
;;; Commentary:
;;; Code:
(require 'pulse)
(require 'rx)
(require 'f)
(require 'dash)
(require 'org)
(require 'cl)
(require 's)

(defgroup org-runbook nil "Org Runbook Options")

(defcustom org-runbook-files nil
  "Global files used by org runbook."
  :group 'org-runbook
  :type 'list)

(defcustom org-runbook-project-directory (expand-file-name (f-join user-emacs-directory "runbook" "projects"))
  "Directory used to lookup the org file corresponding to the current project."
  :group 'org-runbook
  :type 'directory)

(defcustom org-runbook-modes-directory (expand-file-name (f-join user-emacs-directory "runbook" "modes"))
  "Directory used to lookup the org file corresponding to the current major mode."
  :group 'org-runbook
  :type 'directory)

(defcustom org-runbook-view-mode-buffer "*compile-command*"
  "Buffer used for `org-runbook-view-command-action'."
  :group 'org-runbook
  :type 'string)

(defcustom org-runbook-execute-command-action #'org-runbook-command-execute-eshell
  "Function called to handle executing the given runbook.
It is provided the output of `org-runbook--shell-command-for-candidate'."
  :type 'function
  :group 'org-runbook)

(defun org-runbook-comands ()
  "Return the runbook commands corresponding to the current buffer."
  (let* ((major-mode-file (list (cons (symbol-name major-mode) (org-runbook-major-mode-file))))
         (current-buffer-file (when (eq major-mode 'org-mode)
                                (list (cons "*current buffer*"
                                            (buffer-file-name)))))
         (projectile-file (list (when (fboundp 'projectile-project-name)
                                  (cons (concat "*Project " (projectile-project-name) "*")
                                        (org-runbook-projectile-file)))))
         (global-files (--map (cons it it) org-runbook-files))
         (org-files
          (seq-uniq (append major-mode-file current-buffer-file projectile-file global-files)
                    (lambda (lhs rhs) (string= (cdr lhs) (cdr rhs))))))
    (cl-loop for file in org-files
             append
             (progn (set-buffer (find-file-noselect (cdr file)))
                    (goto-char (point-min))
                    (-> (list
                         :file file
                         :commands (org-runbook--command-at-point))
                        list)))))

(defun org-runbook-view-command-action (command)
  "View the selected command from helm.  Expects COMMAND to be a plist with (:name :buffer :point)."
  (-let* ((count 0)
          (project-root (org-runbook--project-root))
          ((&plist :commands) (org-runbook--shell-command-for-candidate command)))
    (when (get-buffer org-runbook-view-mode-buffer) (kill-buffer org-runbook-view-mode-buffer))
    (switch-to-buffer (or (get-buffer org-runbook-view-mode-buffer)
                          (generate-new-buffer org-runbook-view-mode-buffer)))

    (org-runbook-view-mode)
    (setq-local inhibit-read-only t)
    (erase-buffer)
    (->> commands
         (-map (-lambda ((section &as &plist :heading :command))
                 (setq count (1+ count))
                 (--> (concat (s-repeat count "*")
                              " "
                              heading
                              "\n\n"
                              "#+BEGIN_SRC shell\n\n"
                              command
                              "\n#+END_SRC\n")
                      (propertize it
                                  'point-entered
                                  (lambda (&rest args) (setq-local org-runbook-view--section section))))))
         (s-join "\n")
         (insert))
    (setq-local inhibit-read-only nil)))

(defun org-runbook-execute-command-action (command)
  "Execute the org-runbook compile COMMAND from helm.
Expects COMMAND to be of the form (:command :name)."
  (funcall org-runbook-execute-command-action (org-runbook--shell-command-for-candidate command)))

(defun org-runbook-command-execute-eshell (command)
  "Execute the COMMAND in eshell."
  (-let [(&plist :full-command) command]
    (eshell-command full-command)))

(defun org-runbook-goto-command-action (command)
  "Goto the position referenced by COMMAND.  Expects COMMAND to be a plist of the form (:buffer :point)."
  (let ((buffer (or (plist-get command :buffer) (error "Candidate must have :buffer")))
        (point (or (plist-get command :point) (error "Candidate must have :point"))))
    (switch-to-buffer buffer)
    (goto-char point)
    (pulse-momentary-highlight-one-line (point))))

(defun org-runbook--command-at-point ()
  "Walks up the org subtree in order to construct a plist containing the command at point."
  (cl-loop while (re-search-forward (rx line-start "#+BEGIN_SRC" (* whitespace) "shell") nil t)
           append
           (let* ((headings (save-excursion
                              (append
                               (list (org-get-heading))
                               (save-excursion
                                 (cl-loop while (org-up-heading-safe)
                                          append (list (org-get-heading)))))))
                  (name (->> headings
                             (-map 's-trim)
                             (reverse)
                             (s-join " >> "))))
             (list (cons name (list :name name :buffer (current-buffer) :point (point)))))))

(defun org-runbook-switch-to-major-mode-file ()
  "Switch current buffer to the file corresponding to the current buffer's major mode."
  (interactive)
  (find-file (org-runbook-major-mode-file)))

(defun org-runbook-switch-to-projectile-file ()
  "Switch current buffer to the file corresponding to the current buffer's projectile mode."
  (interactive)
  (find-file (org-runbook-projectile-file)))

(defun org-runbook-capture-target-major-mode-file ()
  "Switch current buffer to the file corresponding to the current buffer's major mode."
  (org-runbook-switch-to-major-mode-file)
  (goto-char (point-max)))

(defun org-runbook-capture-target-projectile-file ()
  "Target for appending at the end of the runbook corresponding to the current buffer's projectile project."
  (org-runbook-switch-to-projectile-file)
  (goto-char (point-max)))

(defun org-runbook-major-mode-file ()
  "Target for appending at the end of the runbook corresponding to the current buffer's major mode."
  (org-runbook--ensure-file (f-join org-runbook-project-directory (concat (symbol-name major-mode) ".org"))))

(defun org-runbook--ensure-file (file)
  "Create the FILE if it doesn't exist.  Return the fully expanded FILE name."
  (let ((full-file (expand-file-name file)))
    (when (not (f-exists-p full-file))
      (mkdir (f-parent full-file) t)
      (f-touch full-file))
    full-file))
(define-derived-mode org-runbook-view-mode org-mode "compile view"
  "Mode for viewing resolved org-runbook commands"
  (read-only-mode 1)
  (view-mode 1))


(defvar-local org-runbook-view--section nil "Tracks the section point is currently on in org-runbook-view-mode")

(define-key org-runbook-view-mode-map (kbd "<return>") #'org-runbook-view--open-at-point)

(defun org-runbook--project-root ()
  "Return the current project root if projectile is defined otherwise `default-directory'."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      default-directory))

(defun org-runbook-projectile-file ()
  "Return the path for the org runbook file correspoding to the current projectile project."
  (when (not (fboundp 'projectile-project-name))
    (user-error "Projectile must be installed for org-runbook-projectile-file"))
  (org-runbook--ensure-file (f-join org-runbook-project-directory (concat (projectile-project-name) ".org"))))

(defun org-runbook-view--open-at-point ()
  "Switch buffer to the file referenced at point in `org-runbook-view-mode'."
  (interactive)
  (or (-some-> org-runbook-view--section org-runbook-goto-candidate-action)
      (user-error "No known section at point")))

(defun org-runbook--shell-command-for-candidate (command)
  "Return the full compile command for a COMMAND of the form:
\t(:name :buffer :point)

Output will be of the form (:full-command :commands (list (:heading :point :buffer :command)))."
  (save-excursion
    (-let [(&plist :name :buffer :point) command]
      (push (plist-get command :name) dat-org--compile-history)
      (let* ((project-root (org-runbook--project-root))
             (blocks nil))
        (set-buffer buffer)
        (goto-char point)
        (org-back-to-heading)
        (save-excursion
          (let* ((at-root nil))
            (while (not at-root)
              (let* ((start (org-get-heading)))
                (save-excursion
                  (while (and (ignore-errors (org-babel-next-src-block 1)) (string= (org-get-heading) start))
                    (push
                     (list
                      :heading (org-get-heading)
                      :point (point)
                      :buffer (current-buffer)
                      :command
                      (mustache-render
                       (buffer-substring
                        (save-excursion (forward-line 1) (point))
                        (save-excursion (re-search-forward (rx "#+END_SRC")) (beginning-of-line) (point)))
                       (ht ("project_root" project-root))))
                     blocks)
                    (forward-line 1))))
              (setq at-root (not (org-up-heading-safe))))))
        (list
         :name name
         :full-command
         (->> blocks
              (--map (plist-get it :command))
              (--map (s-trim it))
              (s-join ";\n"))
         :commands blocks)))))

(add-to-list 'evil-motion-state-modes 'org-runbook-view-mode)

(provide 'org-runbook)
;;; org-runbook.el ends here

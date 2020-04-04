;;; org-runbook.el --- Org mode for runbooks -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 1.0
;; Keywords: convenience, processes, terminals, files
;; Package-Requires: ((emacs "25.1") (seq "2.3") (f "0.20.0") (s "1.12.0") (dash "2.17.0") (mustache "0.24") (ht "0.9"))
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
;; org-runbook provides heirarchical runbook commands from org file accessible directly from buffers.
;; Main entry points include `org-runbook-execute', `org-runbook-switch-to-major-mode-file',
;; and `org-runbook-switch-to-projectile-file'
;;
;; org-runbook lets you take org files structured like

;; #### MAJOR-MODE.org
;; ```
;; * Build
;; #+BEGIN_SRC shell
;; cd {{project_root}}
;; #+END_SRC

;; ** Quick
;; #+BEGIN_SRC shell
;; make quick
;; #+END_SRC

;; ** Clean
;; #+BEGIN_SRC shell
;; make clean
;; #+END_SRC

;; ** Prod
;; #+BEGIN_SRC shell
;; make prod
;; #+END_SRC
;; ```
;; and exposes them for easy access in buffers with corresponding major mode.
;; So, the function [org-runbook-execute](org-runbook-execute) has the following completions when the current buffer's major mode is MAJOR-MODE:
;; ```
;; Build >> Quick
;; Build >> Clean
;; Build >> Prod
;; ```
;; Each of these commands is the concatenation of the path of the tree.  So for example, Build >> Quick would resolve to:
;; ```
;; cd {{project_root}}
;; make quick
;; ```
;; If projectile-mode is installed, org-runbook also pulls the file named PROJECTILE-PROJECT-NAME.org.
;; All files in [org-runbook-files] are also pulled.
;; Commands will resolve placeholders before evaluating.  Currently the only available placeholder is {{project_root}}
;; which corresponds to the projectile-project-root of the buffer that called `org-runbook-execute'
;;; Code:

;; External Dependencies
(require 'seq)
(require 'f)
(require 's)
(require 'dash)
(require 'mustache)
(require 'ht)

;; Emacs Dependencies
(require 'pulse)
(require 'rx)
(require 'org)
(require 'pcase)
(require 'subr-x)


;; Optional Dependencies
(require 'projectile nil t)
(require 'evil nil t)

(defgroup org-runbook nil "Org Runbook Options" :group 'org)

(defcustom org-runbook-files nil
  "Global files used by org runbook.
When resolving commands for the current buffer, `org-runbook' appends
`org-runbook-files' with the major mode org file and the projectile
org file."
  :group 'org-runbook
  :type 'list)

(defcustom org-runbook-project-directory (expand-file-name (f-join user-emacs-directory "runbook" "projects"))
  "Directory used to lookup the org file corresponding to the current project.
`org-runbook-projectile-file' joins `org-runbook-project-directory'
with the function `projectile-project-name' for the current buffer."
  :group 'org-runbook
  :type 'directory)

(defcustom org-runbook-modes-directory (expand-file-name (f-join user-emacs-directory "runbook" "modes"))
  "Directory used to lookup the org file for the current major mode.
`org-runbook-major-mode-file' joins `org-runbook-modes-directory'
with the `symbol-name' of the `major-mode' for the current buffer."
  :group 'org-runbook
  :type 'directory)

(defcustom org-runbook-view-mode-buffer "*compile-command*"
  "Buffer used for `org-runbook-view-command-action' to display the resolved command."
  :group 'org-runbook
  :type 'string)

(defcustom org-runbook-execute-command-action #'org-runbook-command-execute-eshell
  "Function called to handle executing the given runbook.
It is provided as a single argument the plist output of `org-runbook--shell-command-for-target'."
  :type 'function
  :group 'org-runbook)

(defun org-runbook-subcommand-list-p (arg)
  "Return non-nil if ARG is a list and every element is an org-runbook-command."
  (and (listp arg)
       (not (--first (not (org-runbook-subcommand-p it)) arg))))

(defun org-runbook-command-list-p (arg)
  "Return non-nil if ARG is a list and every element is an org-runbook-command."
  (and (listp arg)
       (not (--first (not (org-runbook-command-p it)) arg))))

(cl-defstruct org-runbook-command-target
  (name nil :type stringp)
  (point nil :type numberp)
  (buffer nil :type bufferp))

(cl-defstruct org-runbook-subcommand
  (heading nil :type stringp)
  (target nil :type org-runbook-command-target-p)
  (command nil :type stringp))

(cl-defstruct org-runbook-command
  (name nil :type stringp :read-only t)
  (full-command nil :read-only t :type stringp)
  (target nil :read-only t :type org-runbook-command-target-p)
  (subcommands nil :read-only t :type org-runbook-subcommand-list-p))

(cl-defstruct org-runbook-file
  (name nil :type stringp :read-only t)
  (file nil :type stringp :read-only t)
  (targets nil :read-only t :type org-runbook-command-list-p))

;;;###autoload
(defun org-runbook-execute ()
  "Prompt for command completion and execute the selected command."
  (interactive)
  (when-let (command (org-runbook--completing-read))
    (org-runbook-execute-command-action command)))

;;;###autoload
(defun org-runbook-view ()
  "Prompt for command completion and view the selected command."
  (interactive)
  (when-let (command (org-runbook--completing-read))
    (org-runbook-view-command-action command)))

;;;###autoload
(defun org-runbook-goto ()
  "Prompt for command completion and goto the selected command's location."
  (interactive)
  (when-let (command (org-runbook--completing-read))
    (org-runbook-goto-command-action command)))

;;;###autoload
(defun org-runbook-commands ()
  "Return the runbook commands corresponding to the current buffer."
  (save-excursion
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
               (-let* (((name . file) file)
                       (targets (when (-some-> file f-exists-p)
                                  (set-buffer (find-file-noselect file))
                                  (org-runbook--targets-in-buffer))))
                 (when targets
                   (-> (make-org-runbook-file
                        :name name
                        :file file
                        :targets targets)
                       list)))))))

;;;###autoload
(defun org-runbook-switch-to-major-mode-file ()
  "Switch current buffer to the file corresponding to the current buffer's major mode."
  (interactive)
  (find-file (org-runbook-major-mode-file)))

;;;###autoload
(defun org-runbook-switch-to-projectile-file ()
  "Switch current buffer to the file corresponding to the current buffer's projectile mode."
  (interactive)
  (find-file (org-runbook-projectile-file)))

;;;###autoload
(defun org-runbook-capture-target-major-mode-file ()
  "Switch current buffer to the file corresponding to the current buffer's major mode."
  (org-runbook-switch-to-major-mode-file)
  (goto-char (point-max)))

;;;###autoload
(defun org-runbook-capture-target-projectile-file ()
  "Target for appending at the end of the runbook corresponding to the current buffer's projectile project."
  (org-runbook-switch-to-projectile-file)
  (goto-char (point-max)))

(defun org-runbook--completing-read ()
  "Prompt user for a runbook command."
  (let ((command-map
         (->> (org-runbook-commands)
              (--map (org-runbook-file-targets it))
              (-flatten)
              (--map (cons (org-runbook-command-target-name it) it))
              (ht<-alist))))
    (when (eq (ht-size command-map) 0) (org-runbook--no-commands-error))
    (when-let (key (completing-read "Runbook:" command-map nil t))
      (ht-get command-map key))))

(defun org-runbook-view-command-action (target)
  "View the selected command from helm.  Expects TARGET to be a `org-runbook-command-target'."
  (unless (org-runbook-command-target-p target) (error "Unexpected type provided: %s" target))
  (pcase-let* ((count 0)
               (project-root (org-runbook--project-root))
               ((cl-struct org-runbook-command subcommands) (org-runbook--shell-command-for-target target)))
    (when (get-buffer org-runbook-view-mode-buffer) (kill-buffer org-runbook-view-mode-buffer))
    (switch-to-buffer (or (get-buffer org-runbook-view-mode-buffer)
                          (generate-new-buffer org-runbook-view-mode-buffer)))

    (org-runbook-view-mode)
    (setq-local inhibit-read-only t)
    (erase-buffer)
    (->> subcommands
         (-map
          (pcase-lambda ((and section (cl-struct org-runbook-subcommand heading command)))
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
    (-some->> (first subcommands) (setq-local org-runbook-view--section))
    (setq-local inhibit-read-only nil)))

(defun org-runbook-execute-command-action (command)
  "Execute the `org-runbook' compile COMMAND from helm.
Expects COMMAND to be of the form (:command :name)."
  (org-runbook--validate-command command)
  (funcall org-runbook-execute-command-action (org-runbook--shell-command-for-target command)))

(defun org-runbook-command-execute-eshell (command)
  "Execute the COMMAND in eshell."
  (org-runbook--validate-command command)
  (pcase-let (((cl-struct org-runbook-command full-command) command))
    (eshell-command full-command)))

(defun org-runbook-goto-command-action (command)
  "Goto the position referenced by COMMAND.
Expects COMMAND to ether be a `org-runbook-subcommand'
or a `org-runbook-command-target'."
  (--> (pcase command
         ((or (cl-struct org-runbook-subcommand (target (cl-struct org-runbook-command-target point buffer)))
              (cl-struct org-runbook-command-target point buffer))
          (list :buffer buffer :point point)))
       (-let [(&plist :buffer :point) it]
         (switch-to-buffer buffer)
         (goto-char point)
         (pulse-momentary-highlight-one-line (point)))))

(defun org-runbook--targets-in-buffer ()
  "Get all targets by walking up the org subtree in order.
Return `org-runbook-command-target'."
  (save-excursion
    (goto-char (point-min))
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
               (list (make-org-runbook-command-target
                      :name name :buffer (current-buffer) :point (point)))))))

(defun org-runbook-major-mode-file ()
  "Target for appending at the end of the runbook corresponding to the current buffer's major mode."
  (org-runbook--ensure-file (f-join org-runbook-project-directory (concat (symbol-name major-mode) ".org"))))

(defun org-runbook-projectile-file ()
  "Return the path for the org runbook file correspoding to the current projectile project."
  (when (not (fboundp 'projectile-project-name))
    (user-error "Projectile must be installed for org-runbook-projectile-file"))
  (org-runbook--ensure-file (f-join org-runbook-project-directory (concat (projectile-project-name) ".org"))))

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

(define-key org-runbook-view-mode-map (kbd "<return>") #'org-runbook-view--open-at-point)

(defvar-local org-runbook-view--section nil "Tracks the section point is currently on in org-runbook-view-mode")

(defun org-runbook--project-root ()
  "Return the current project root if projectile is defined otherwise `default-directory'."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      default-directory))

(defun org-runbook-view--open-at-point ()
  "Switch buffer to the file referenced at point in `org-runbook-view-mode'."
  (interactive)
  (or (-some-> org-runbook-view--section org-runbook-goto-command-action)
      (user-error "No known section at point")))

(defun org-runbook--shell-command-for-target (target)
  "Return the `org-runbook-command' for a TARGET.
TARGET is a `org-runbook-command-target'."
  (unless (org-runbook-command-target-p target) (error "Unexpected type passed %s" target))
  (save-excursion
    (pcase-let (((cl-struct org-runbook-command-target name buffer point) target))
      (let* ((project-root (org-runbook--project-root))
             (subcommands nil))
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
                     (make-org-runbook-subcommand
                      :heading (org-get-heading)
                      :target (make-org-runbook-command-target
                               :buffer (current-buffer)
                               :point (point))
                      :command
                      (mustache-render
                       (buffer-substring
                        (save-excursion (forward-line 1) (point))
                        (save-excursion (re-search-forward (rx "#+END_SRC")) (beginning-of-line) (point)))
                       (ht ("project_root" project-root))))
                     subcommands)
                    (forward-line 1))))
              (setq at-root (not (org-up-heading-safe))))))
        (make-org-runbook-command
         :name name
         :target (->> subcommands last car org-runbook-subcommand-target)
         :full-command
         (->> subcommands
              (--map (org-runbook-subcommand-command it))
              (--filter it)
              (--map (s-trim it))
              (s-join ";\n"))
         :subcommands subcommands)))))

(defun org-runbook--no-commands-error ()
  "Error representing that no commands were found for the current buffer."
  (if (fboundp 'projectile-project-name)
      (user-error "No Commands Defined For Runbook.  (Major Mode: %s, Project: %s)"
                  (symbol-name major-mode)
                  (projectile-project-name))
    (user-error "No Commands Defined For Runbook.  (Major Mode: %s)"
                (symbol-name major-mode))))

(defun org-runbook--validate-command (command)
  "Validates COMMAND and throws errors if it doesn't match spec."
  (when (not command) (error "Command cannot be nil"))
  (org-runbook-command-p command))

(when (boundp 'evil-motion-state-modes)
  (add-to-list 'evil-motion-state-modes 'org-runbook-view-mode))

(provide 'org-runbook)
;;; org-runbook.el ends here

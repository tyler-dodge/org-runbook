# org-runbook.el [![Build Status](https://travis-ci.org/tyler-dodge/org-runbook.svg?branch=master)](https://travis-ci.org/github/tyler-dodge/org-runbook)

Library for looking up and executing commands from org files corresponding to the current buffer.

## Installation

Coming soon to melpa.

## Usage

org-runbook exposes a few commands meant to be example entry points using completing read. 

See [Helm Org Runbook](https://github.com/tyler-dodge/helm-org-runbook) for a nicer frontend.

* [org-runbook-execute](org-runbook-execute) Prompt for command completion and execute the selected command.

* [org-runbook-view](org-runbook-view) Prompt for command completion and view the selected command fully resolved.

* [org-runbook-goto](org-runbook-goto) Prompt for command completion and goto where the selected command is defined.

## API

### Commands

* [org-runbook-commands](org-runbook-commands) Return the runbook commands corresponding to the current buffer.
Intended to provide completions for completing-read functions

* [org-runbook-execute-command-action](org-runbook-execute-command-action) Execute the command.
Expects the command to be one of the elements of (org-runbook-commands)

* [org-runbook-view-command-action](org-runbook-view-command-action)  View the command.
Expects the command to be one of the elements of (org-runbook-commands)

* [org-runbook-goto-command-action](org-runbook-goto-command-action)  Switch to the file where the command is defined.
Expects the command to be one of the elements of (org-runbook-commands)

### Org Files
* [org-runbook-switch-to-projectile-file](org-runbook-switch-to-projectile-file) Switch current buffer to the file corresponding to the current buffer's projectile mode.

* [org-runbook-switch-to-major-mode-file](org-runbook-switch-to-major-mode-file) Switch current buffer to the file corresponding to the current buffer's major mode mode.

## Customization

* [org-runbook-files](org-runbook-files) Global file list used by org runbook. 
When resolving commands for the current buffer, org-runbook appends org-runbook-files with the major mode org file and the projectile org file.

* [org-runbook-project-directory](org-runbook-project-directory) Directory used to lookup the org file corresponding to the current project.
org-runbook-projectile-file joins org-runbook-project-directory
with the projectile-project-name for the current buffer.

* [org-runbook-modes-directory](org-runbook-modes-directory) Directory used to lookup the org file for the current major mode.
org-runbook-major-mode-file joins org-runbook-modes-directory
with the symbol-name of the major-mode for the current buffer.

* [org-runbook-view-mode-buffer](org-runbook-view-mode-bxuffer) Buffer used for org-runbook-view-command-action to display the resolved command.

* [org-runbook-execute-command-action](org-runbook-execute-command-action) Function called to handle executing the given runbook.
It is provided as a single argument the plist output of org-runbook--shell-command-for-candidate.

## Contributing

Contributions welcome, but forking preferred. 
I plan to actively maintain this, but I will be prioritizing features that impact me first.

I'll look at most pull requests eventually, but there is no SLA on those being accepted. 
    
Also, I will only respond to pull requests on a case by case basis. 
I have no obligation to comment on, justify not accepting, or accept any given pull request. 
Feel free to start a fork that has more support in that area.

If there's a great pull request that I'm slow on accepting, feel free to fork and rename the project.

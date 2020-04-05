# org-runbook.el [![Build Status](https://travis-ci.org/tyler-dodge/org-runbook.svg?branch=master)](https://travis-ci.org/github/tyler-dodge/org-runbook) [![Coverage Status](https://coveralls.io/repos/github/tyler-dodge/org-runbook/badge.svg)](https://coveralls.io/github/tyler-dodge/org-runbook)

Library for looking up and executing commands from org files corresponding to the current buffer.

## Installation

Coming soon to melpa.

## Usage

### Example

org-runbook lets you take org files structured like

#### MAJOR-MODE.org
```
* Build
#+BEGIN_SRC shell
cd {{project_root}}
#+END_SRC

** Quick
#+BEGIN_SRC shell
make quick
#+END_SRC

** Clean
#+BEGIN_SRC shell
make clean
#+END_SRC

** Prod
#+BEGIN_SRC shell
make prod
#+END_SRC
```

and exposes them for easy access in buffers with corresponding major mode.
So, the function [org-runbook-execute](#org-runbook-execute) has the following completions when the current buffer's major mode is MAJOR-MODE:

```
Build >> Quick
Build >> Clean
Build >> Prod
```

Each of these commands is the concatenation of the path of the tree. So for example, Build >> Quick would resolve to:

```
cd {{project_root}}
make quick
```

If projectile-mode is installed, org-runbook also pulls the file named PROJECTILE-PROJECT-NAME.org.

All files in [org-runbook-files] are also pulled.


### Placeholders
Commands will resolve placeholders before evaluating.

* {{project_root}} - the projectile-project-root of the buffer that called `org-runbook-execute'

* {{current_file}} - the file that the buffer that called org-runbook-execute was visiting. If the the buffer is a non file buffer, current_file is default-directory

### Interactive Commands

org-runbook exposes a few commands meant to be example entry points using completing read. 

See [Helm Org Runbook](https://github.com/tyler-dodge/helm-org-runbook) for a nicer frontend.

* [org-runbook-execute](#org-runbook-execute) <a name="org-runbook-execute"></a> Prompt for command completion and execute the selected command.

* [org-runbook-view](#org-runbook-view) <a name="org-runbook-view"></a> Prompt for command completion and view the selected command fully resolved.

* [org-runbook-goto](#org-runbook-goto) <a name="org-runbook-goto"></a> Prompt for command completion and goto where the selected command is defined.

## API

### Commands

* [org-runbook-targets](#org-runbook-targets) <a name="org-runbook-targets"></a> Return the runbook commands corresponding to the current buffer.
Intended to provide completions for completing-read functions

* [org-runbook-execute-command-action](#org-runbook-execute-command-action) <a name="org-runbook-execute-command-action"></a> Execute the command.
Expects the command to be one of the elements of (org-runbook-targets)

* [org-runbook-view-command-action](#org-runbook-view-command-action) <a name="org-runbook-view-command-action"></a>  View the command.
Expects the command to be one of the elements of (org-runbook-targets)

* [org-runbook-goto-command-action](#org-runbook-goto-command-action) <a name="org-runbook-goto-command-action"></a>  Switch to the file where the command is defined.
Expects the command to be one of the elements of (org-runbook-targets)

### Org Files
* [org-runbook-switch-to-projectile-file](#org-runbook-switch-to-projectile-file) <a name="org-runbook-switch-to-projectile-file"></a> Switch current buffer to the file corresponding to the current buffer's projectile mode.

* [org-runbook-switch-to-major-mode-file](#org-runbook-switch-to-major-mode-file) <a name="org-runbook-switch-to-major-mode-file"></a> Switch current buffer to the file corresponding to the current buffer's major mode mode.

## Customization

* [org-runbook-files](#org-runbook-files) <a name="org-runbook-files"></a> Global file list used by org runbook. 
When resolving commands for the current buffer, org-runbook appends org-runbook-files with the major mode org file and the projectile org file.

* [org-runbook-project-directory](#org-runbook-project-directory) <a name="org-runbook-project-directory"></a> Directory used to lookup the org file corresponding to the current project.
org-runbook-projectile-file joins org-runbook-project-directory
with the projectile-project-name for the current buffer.

* [org-runbook-modes-directory](#org-runbook-modes-directory) <a name="org-runbook-modes-directory"></a> Directory used to lookup the org file for the current major mode.
org-runbook-major-mode-file joins org-runbook-modes-directory
with the symbol-name of the major-mode for the current buffer.

* [org-runbook-view-mode-buffer](#org-runbook-view-mode-bxuffer) <a name="org-runbook-view-mode-bxuffer"></a> Buffer used for org-runbook-view-command-action to display the resolved command.

* [org-runbook-execute-command-action](#org-runbook-execute-command-action) <a name="org-runbook-execute-command-action"></a> Function called to handle executing the given runbook.
It is provided as a single argument the plist output of org-runbook--shell-command-for-candidate.

## Contributing

Contributions welcome, but forking preferred. 
I plan to actively maintain this, but I will be prioritizing features that impact me first.

I'll look at most pull requests eventually, but there is no SLA on those being accepted. 
    
Also, I will only respond to pull requests on a case by case basis. 
I have no obligation to comment on, justify not accepting, or accept any given pull request. 
Feel free to start a fork that has more support in that area.

If there's a great pull request that I'm slow on accepting, feel free to fork and rename the project.

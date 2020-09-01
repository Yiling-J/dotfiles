;;; My emacs settings

;;; Code:


(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; override anaconda-mode-server-command to support jedi 0.7 and get_context
(defvar anaconda-mode-server-command "

from __future__ import print_function

# CLI arguments.

import sys

assert len(sys.argv) > 3, 'CLI arguments: %s' % sys.argv

server_directory = sys.argv[-3]
server_address = sys.argv[-2]
virtual_environment = sys.argv[-1]

# Ensure directory.

import os

server_directory = os.path.expanduser(server_directory)
virtual_environment = os.path.expanduser(virtual_environment)

if not os.path.exists(server_directory):
    os.makedirs(server_directory)

# Installation check.

jedi_dep = ('jedi', '0.17.2')
service_factory_dep = ('service_factory', '0.1.5')

missing_dependencies = []

def instrument_installation():
    for package in (jedi_dep, service_factory_dep):
        package_is_installed = False
        for path in os.listdir(server_directory):
            path = os.path.join(server_directory, path)
            if path.endswith('.egg') and os.path.isdir(path):
                if path not in sys.path:
                    sys.path.insert(0, path)
                if package[0] in path:
                    package_is_installed = True
        if not package_is_installed:
            missing_dependencies.append('>='.join(package))

instrument_installation()

# Installation.

def install_deps():
    import site
    import setuptools.command.easy_install
    site.addsitedir(server_directory)
    cmd = ['--install-dir', server_directory,
           '--site-dirs', server_directory,
           '--always-copy','--always-unzip']
    cmd.extend(missing_dependencies)
    setuptools.command.easy_install.main(cmd)
    instrument_installation()

if missing_dependencies:
    install_deps()

del missing_dependencies[:]

try:
    import jedi
except ImportError:
    missing_dependencies.append('>='.join(jedi_dep))

try:
    import service_factory
except ImportError:
    missing_dependencies.append('>='.join(service_factory_dep))

# Try one more time in case if anaconda installation gets broken somehow
if missing_dependencies:
    install_deps()
    import jedi
    import service_factory

# Setup server.

assert jedi.__version__ >= jedi_dep[1], 'Jedi version should be >= %s, current version: %s' % (jedi_dep[1], jedi.__version__,)

if virtual_environment:
    virtual_environment = jedi.create_environment(virtual_environment, safe=False)
else:
    virtual_environment = None

# Define JSON-RPC application.

import functools
import threading

def script_method(f):
    @functools.wraps(f)
    def wrapper(source, line, column, path):
        timer = threading.Timer(30.0, sys.exit)
        timer.start()
        result = f(jedi.Script(code=source, path=path, environment=virtual_environment), line, column)
        timer.cancel()
        return result
    return wrapper

def process_definitions(f):
    @functools.wraps(f)
    def wrapper(script, line, column):
        definitions = f(script, line, column)
        if len(definitions) == 1 and not definitions[0].module_path:
            return '%s is defined in %s compiled module' % (
                definitions[0].name, definitions[0].module_name)
        return [[definition.module_path,
                 definition.line,
                 definition.column,
                 definition.get_line_code().strip()]
                for definition in definitions
                if definition.module_path] or None
    return wrapper

@script_method
def complete(script, line, column):
    return [[definition.name, definition.type]
            for definition in script.complete(line=line, column=column)]

@script_method
def company_complete(script, line, column):
    return [[definition.name,
             definition.type,
             definition.docstring(),
             definition.module_path,
             definition.line]
            for definition in script.complete(line=line, column=column)]

@script_method
def show_doc(script, line, column):
    return [[definition.module_name, definition.docstring()]
            for definition in script.infer(line=line, column=column)]

@script_method
@process_definitions
def goto_definitions(script, line, column):
    print(line)
    return script.infer(line=line, column=column)

@script_method
@process_definitions
def goto_assignments(script, line, column):
    return script.goto(line=line, column=column)

@script_method
@process_definitions
def usages(script, line, column):
    return script.get_references(line=line, column=column)

@script_method
def eldoc(script, line, column):
    signatures = script.get_signatures(line=line, column=column)
    if len(signatures) == 1:
        signature = signatures[0]
        return [signature.name,
                signature.index,
                [param.description[6:] for param in signature.params]]

@script_method
def get_context(script, line, column):
    name = script.get_context(line=line, column=column).full_name
    print(name)
    return name

# Run.

app = [complete, company_complete, show_doc, goto_definitions, goto_assignments, usages, eldoc, get_context]

service_factory.service_factory(app, server_address, 0, 'anaconda_mode port {port}')

" "Run `anaconda-mode' server.")
(package-initialize)
(require 'company)
(require 'company-tern)
(require 'expand-region)
(require 'flycheck)
(require 'js2-mode)
(require 'projectile)
(require 'tramp)
(require 'xref-js2)
(require 'transient)
(require 'magit)
(require 'org-jira)
(require 'ivy)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(projectile-mode +1)
(load-theme 'tangotango +1)
(global-display-line-numbers-mode +1)
(global-git-gutter-mode +1)
(global-hl-line-mode +1)
(counsel-projectile-mode +1)
(editorconfig-mode +1)
(delete-selection-mode +1)
(semantic-mode +1)
(show-paren-mode +1)
(nyan-mode +1)
(ivy-mode +1)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(defvar backup-dir (expand-file-name "~/.emacs.d/emacs_backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(setq flycheck-check-syntax-automatically '(mode-enabled save))
(setq flycheck-display-errors-function
      #'flycheck-display-error-messages-unless-error-list)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq py-python-command "python3")
(setq company-tooltip-align-annotations t)
(setq company-selection-wrap-around t)
(setq company-idle-delay 0.5)
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)
(setq py-trailing-whitespace-smart-delete-p t)
(setq show-paren-delay 0)
(setq prettier-js-args '(
  "--single-quote" "true"
  "--no-semi" "true"
))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'rjsx-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
(add-hook 'rjsx-mode-hook #'js2-imenu-extras-mode)
(add-hook 'rjsx-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'rjsx-mode-hook #'js2-refactor-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
  "--single-quote" "true"
  "--no-semi" "true"
))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'flycheck-after-syntax-check-hook #'error-list)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))
(add-to-list 'company-backends 'company-tern)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))


(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-x g") 'magit-status)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(set-face-background 'hl-line "#3e4446")

(global-set-key (kbd "C-<return>") 'counsel-semantic)

(define-key js2-mode-map (kbd "M-.") nil)

(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

(defun error-list ()
          (if (>  (length (flycheck-mode-line-status-text)) 6)
          (list-flycheck-errors)
	  (if (get-buffer-window "*Flycheck errors*") (delete-window (get-buffer-window "*Flycheck errors*")))
	  ))

(defun anaconda-mode-django-test-here ()
  "Run python manage.py test on function at cursor"
  (interactive)
  (anaconda-mode-call
   "get_context"
   (lambda (result)
     (switch-to-buffer-other-window "vterm")
     (process-send-string "vterm" (format "python manage.py test %s\n" result))
     (anaconda-mode-show-xrefs result nil "No definitions found"))))

(defvar jira-issues)

(defun commit-with-task ()
  "Choose jira task and auto add task id as commit prefix.
Need to custom some variables before use this: org-jira-custom-jqls, jiralib-url."

 ;; example jql for current user in progress task: assignee = currentUser() and status = "In Progress" order by created DESC
  (interactive)
  (let ((key nil) (key2 nil) (task nil) (final nil) (word nil) (summary nil) (task-id nil))
  (setq jira-issues (jiralib-do-jql-search  (car (cdr (car org-jira-custom-jqls)))))
  (dolist (issue jira-issues)
    (setq key (car (seq-filter (lambda (x) (string= (car x) "key")) issue)))
    (setq key2 (cdr key))
    (setq issue (car (seq-filter (lambda (x) (string= (car x) "fields")) issue)))
    
    (setq summary (seq-reduce (lambda (a b)
			    (setq word (cond
					((stringp b) nil)
					((symbolp b) nil)
					((string= (car b) "summary")
					  (concat a (cdr b))
					 )
					))
			    (or word a)
			    ) issue "")
	  )
    (setq task (concat key2 " " summary))
    (setq final (append final (list task)))
    )
  (setq task (ivy-read "Choose your task:" final))
  (setq task-id (car (split-string task)))
  (setq task-id (concat "[" task-id "] "))
  (magit-run-git "commit" "--message" task-id)
  (magit-commit-create '("--amend"))
  ))

(transient-append-suffix 'magit-commit "c"
  '("j" "Commit with jira" commit-with-task))

(provide '.emacs)
;;; .emacs ends here

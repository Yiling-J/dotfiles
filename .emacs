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

(package-initialize)


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'company)
(require 'expand-region)
(require 'flycheck)
(require 'projectile)
(require 'tramp)
(require 'transient)
(require 'magit)
(require 'org-jira)
(require 'ivy)
(require 'lsp-mode)
(require 'prettier-js)
(require 'js)

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
(show-paren-mode +1)
(nyan-mode +1)
(ivy-mode +1)
(display-time-mode +1)
(display-battery-mode +1)
(parrot-mode +1)

(use-package lsp-ui)

(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(defvar backup-dir (expand-file-name "~/.emacs.d/emacs_backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(define-key js-mode-map (kbd "M-.") #'xref-find-definitions)

(setq flycheck-check-syntax-automatically '(mode-enabled save))
(setq flycheck-display-errors-function
      #'flycheck-display-error-messages-unless-error-list)
(setq company-tooltip-align-annotations t)
(setq company-selection-wrap-around t)
(setq company-idle-delay 0.5)
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)
(setq prettier-js-args '(
  "--single-quote"
  ))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq rustic-format-on-save t)
(setq lsp-dart-flutter-widget-guides nil)
(setq lsp-enable-on-type-formatting nil)
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta nil)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier nil)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'python-mode-hook
                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'flycheck-after-syntax-check-hook #'error-list)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'before-save-hook #'lsp-organize-imports)
(add-hook 'before-save-hook 'parrot-start-animation)
(add-hook 'python-mode-hook 'blacken-mode)
(add-hook 'rustic-mode-hook #'lsp)
(add-hook 'dart-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'mspyls)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package parrot
  :config
  (parrot-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))

(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)

(global-set-key "\C-s" 'swiper)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(set-face-background 'hl-line "#3e4446")

(global-set-key (kbd "C-<return>") 'counsel-semantic-or-imenu)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(defun error-list ()
          (if (>  (length (flycheck-mode-line-status-text)) 6)
          (list-flycheck-errors)
	  (if (get-buffer-window "*Flycheck errors*") (delete-window (get-buffer-window "*Flycheck errors*")))
	  ))

(add-hook 'dart-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer t t)))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(provide '.emacs)
;;; .emacs ends here

;;; My emacs settings

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-tern git-gutter-fringe pythonic prettier-js add-node-modules-path js2-refactor xref-js2 js2-mode rust-mode rjsx-mode editorconfig isortify expand-region flycheck-color-mode-line flycheck company-anaconda anaconda-mode counsel-projectile counsel tangotango-theme dumb-jump nyan-mode zenburn-theme powerline neotree all-the-icons magit helm-ebdb))))
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
(require 'company)
(require 'company-tern)
(require 'expand-region)
(require 'flycheck)
(require 'js2-mode)
(require 'projectile)
(require 'tramp)
(require 'xref-js2)

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
(setq company-idle-delay 0.0)
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

(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
  "--single-quote" "true"
  "--no-semi" "true"
))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'isortify-mode)
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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


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

(setq inhibit-startup-screen t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(ido-mode 1)
(display-line-numbers-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-margin 5)
(setq-default truncate-lines 0)

(set-face-attribute 'default nil :font "Cascadia Code-15")

(set-fontset-font "fontset-default"
                  'unicode
                  '("Hack Nerd Font"))

(load (expand-file-name "~/Quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(setq evil-want-keybinding nil)
(require 'evil)
(add-to-list 'load-path "~/.emacs.d/evil")
(evil-mode 1)

(require 'undo-tree)
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil
    undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
    undo-tree-enable-undo-in-region nil)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(load-theme 'nord t)

(custom-set-variables
 '(org-agenda-files nil)
 '(package-selected-packages
   '(nord-theme
     geiser-mit
     evil-paredit
     paredit
     evil-collection
     magit
     vterm
     auto-save-buffers-enhanced
     smex
     undo-tree
     evil)))

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)

(when (require 'evil-collection nil t)
  (evil-collection-init))

(setq backup-directory-alist '(("." . "~/.emacs_saves")))

(setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))
(make-directory "/tmp/slime-fasls/" t)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

(load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))

(setq geiser-mit-binary "/usr/local/bin/scheme")
(setq geiser-active-implementations '(mit))

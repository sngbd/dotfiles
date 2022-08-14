(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun open-init-file ()
  (interactive) (find-file user-init-file))

(setq
 inhibit-startup-screen t
 use-short-answers t
 confirm-kill-processes nil
 delete-by-moving-to-trash t
 completions-detailed t
 ring-bell-function 'ignore
 mode-require-final-newline nil
 )

(tooltip-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq scroll-margin 5)
(setq-default truncate-lines 0)

(set-face-attribute 'default nil :font "Cascadia Code" :height 150)

(set-fontset-font "fontset-default"
                  'unicode
                  '("Hack Nerd Font"))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(load (expand-file-name "~/Quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced t)

(setq backup-directory-alist '(("." . "~/.emacs_saves")))

(setq slime-compile-file-options '(:fasl-directory "/tmp/slime-fasls/"))
(make-directory "/tmp/slime-fasls/" t)

(load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))

(setq geiser-mit-binary "/usr/local/bin/scheme")
(setq geiser-active-implementations '(mit))

(setq TeX-indent-open-delimiters "[")
(setq TeX-indent-close-delimiters "]")

(use-package doom-themes
    :init (load-theme 'doom-nord t))

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

(use-package centaur-tabs
  :ensure t
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-bar 'over
	centaur-tabs-set-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-height 24)
  :bind
  ("C-x h" . centaur-tabs-backward)
  ("C-x l" . centaur-tabs-forward))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  :init (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 3))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package all-the-icons)

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(general-define-key
 "C-M-j" 'counsel-switch-buffer)

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  vterm-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (evil-mode 1)
  :hook (evil-mode . rune/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(require 'undo-tree)
(evil-set-undo-system 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil
    undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
    undo-tree-enable-undo-in-region nil)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Programming/Git")
    (setq projectile-project-search-path '("~/Programming/Git")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode 1))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
             
  :hook (lsp-mode . efs/lsp-mode-setup))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook (web-mode . lsp)
  :custom
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")))

(use-package lsp-ivy)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

(use-package json-mode
  :ensure t)

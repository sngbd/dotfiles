(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Defaults
(setq ring-bell-function 'ignore)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-screen t)

(tooltip-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-margin 5)
(setq-default truncate-lines 0)

;; Fonts
(set-face-attribute 'default nil :font "Iosevka" :height 180)

(set-fontset-font "fontset-default"
                  'unicode
                  '("Hack Nerd Font"))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
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

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq custom-file (concat user-emacs-directory "/custom.el"))

(load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))

(setq geiser-mit-binary "/usr/local/bin/scheme")
(setq geiser-active-implementations '(mit))

(setq TeX-indent-open-delimiters "[")
(setq TeX-indent-close-delimiters "]")

;; use-package
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package magit
  :ensure t
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
  :custom ((doom-modeline-height 10)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

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

  ;; Use visual line motions even outside of visual-line-mode buffers
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

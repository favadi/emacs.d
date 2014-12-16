;; Use package.el to manage packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; disable startup message
(setq inhibit-startup-message t)

;; getting rid of the "yes or no" prompt and replace it with "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; disable confirmation if a file or buffer does not exist when you
;; use C-x C-f or C-x b
(setq confirm-nonexistent-file-or-buffer nil)

;; disable confirmation when kill a buffer with a live process
;; attached to it
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
        kill-buffer-query-functions))

;; making tooltips appear in the echo area
(tooltip-mode 0)
(setq tooltip-use-echo-area t)

;; highlight current line
(global-hl-line-mode 1)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; change indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 200 4))

;; use C-h as backspace
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; rebinding mark-defun
(global-set-key (kbd "C-c h") 'mark-defun)

;; enable case conversion
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; switch to most recent buffer
(global-set-key (kbd "C-c b") 'mode-line-other-buffer)

;; use ibuffer instead of buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; delete selection on insert
(delete-selection-mode 1)

;; use extra dired features
(require 'dired-x)

;; disable C-z
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)

;; undo/redo window configuration
(winner-mode 1)

;; making buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; use trash
(setq delete-by-moving-to-trash t)

;; solarized-theme
(use-package solarized-theme
  :ensure t
  :config
  (progn
    (load-theme 'solarized-light t)))

;; smartparens
(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (setq sp-autoescape-string-quote nil)
    (show-smartparens-global-mode 1)))

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'lua-mode-hook 'flycheck-mode)
    (add-hook 'sh-mode-hook 'flycheck-mode)
    (add-hook 'rst-mode-hook 'flycheck-mode)))

;; magit
(use-package magit
  :ensure t)

;; smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/setup)
    (sml/apply-theme 'respectful)))

;; helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-r" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (setq helm-command-prefix-key "C-c l")
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")  'helm-select-action)))

;; helm-ag
(use-package helm-ag
  :ensure t)

;; projectile
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-mode-line-lighter "Prj")
    (setq projectile-remember-window-configs t)))

;; helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (progn
    (helm-projectile-on)))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.sls$")

;; jinja2-mode
(use-package jinja2-mode
  :ensure t
  :mode "\\.jinja\\'")

;; go-mode
(use-package go-mode
  :ensure t
  :config
  (progn
    (add-hook 'before-save-hook #'gofmt-before-save)))

;; rst-mode
(use-package rst
  :config
  (progn
    (local-set-key (kbd "C-M-h") 'backward-kill-word)
    (local-set-key (kbd "C-c h") 'rst-mark-section)))

;; web-mode
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'")

;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; whole-line-ore-region
(use-package whole-line-or-region
  :ensure t
  :config
  (progn
    (defadvice whole-line-or-region-kill-region
      (before whole-line-or-region-kill-read-only-ok activate)
      (interactive "p")
      (unless kill-read-only-ok (barf-if-buffer-read-only)))
    (whole-line-or-region-mode 1)))

;; ws-butler
(use-package ws-butler
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'ws-butler-mode)
    (add-hook 'jinja2-mode-hook 'ws-butler-mode)
    (add-hook 'rst-mode-hook 'ws-butler-mode)
    (add-hook 'yaml-mode-hook 'ws-butler-mode)))

;; virtualenvwrapper
(use-package virtualenvwrapper
  :ensure t
  :config
  (progn
    (add-hook 'python-mode-hook (lambda ()
                                  (hack-local-variables)
                                  (when (boundp 'project-venv-name)
                                    (venv-workon project-venv-name))))
    (put 'project-venv-name 'safe-local-variable #'stringp)))

;; anaconda
(use-package anaconda-mode
  :ensure t
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)))

;; company-anaconda
(use-package company-anaconda
  :ensure t)

;; company-go
(use-package company-go
  :ensure t)

;; company
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("M-/" . company-complete-common)
  :config
  (progn
    (add-to-list 'company-backends 'company-anaconda)
    (add-to-list 'company-backends 'company-go)))

;; helm-dash
(use-package helm-dash
  :ensure t
  :bind (("C-c C-o" . helm-dash-at-point)
         ("C-c o" . helm-dash))
  :config
  (progn
    (defun dash-go-doc ()
      (interactive)
      (setq-local helm-dash-docsets '("Go")))
    (add-hook 'go-mode-hook 'dash-go-doc)

    (defun dash-salt-doc ()
      (interactive)
      (setq-local helm-dash-docsets '("SaltStack")))
    (add-hook 'yaml-mode-hook 'dash-salt-doc)

    (defun dash-python-doc ()
      (interactive)
      (setq-local helm-dash-docsets '("Python 2")))
    (add-hook 'python-mode-hook 'dash-python-doc)))

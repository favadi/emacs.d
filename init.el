;; Use package.el to manage packages
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; help key binding
(bind-key "C-z" 'help-command)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; disable startup message
(setq inhibit-startup-message t)

;; disable beep sound
(setq ring-bell-function 'ignore)

;; getting rid of the "yes or no" prompt and replace it with "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; display column number in mode line
(column-number-mode 1)

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
(bind-key "C-h" 'delete-backward-char)
(bind-key "C-M-h" 'backward-kill-word)

;; rebinding mark-defun
(bind-key "C-c h" 'mark-defun)

;; enable case conversion
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; use ibuffer instead of buffer
(bind-key "C-x C-b" 'ibuffer)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; delete selection on insert
(delete-selection-mode 1)

;; use extra dired features
(require 'dired-x)

;; undo/redo window configuration
(winner-mode 1)

;; making buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; use trash
(setq delete-by-moving-to-trash t)

;; disable garbage collection when minibuffer is active
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; solarized-theme
(use-package solarized-theme
  :ensure t
  :config
  (progn
    (setq solarized-distinct-fringe-background t)
    (setq solarized-use-variable-pitch nil)
    (setq solarized-high-contrast-mode-line t)
    (load-theme 'solarized-light t)))

;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (use-package exec-path-from-shell
        :ensure t
        :config
        (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)
          (exec-path-from-shell-copy-env "GOPATH")
          (exec-path-from-shell-copy-env "HOMEBREW_GITHUB_API_TOKEN")
          (exec-path-from-shell-copy-env "GO15VENDOREXPERIMENT")))
      ;; use bash installed from brew
      (setq explicit-shell-file-name "/usr/local/bin/bash")
      (set-frame-font "PragmataPro Mono 12" t t)
      ;; only for railwaycat emacs
      (setq mac-option-modifier 'meta)
      ;; OS X ls doesn't support --dired
      (setq dired-use-ls-dired nil)))

;; electric-pair-mode
(electric-pair-mode 1)
(show-paren-mode 1)

;; to suppress -Chg in mode line
(use-package hilit-chg
  :diminish highlight-changes-mode)

;; swiper
(use-package swiper
  :ensure t
  :bind (("\C-s" . swiper)
         ("\C-r" . swiper)
         ("C-c C-r" . ivy-resume))
  :diminish ivy-mode
  :init (ivy-mode 1))

;; counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-z f" . counsel-describe-function)
         ("C-z v" . counsel-describe-variable)
         ("C-c k" . counsel-ag)))

;; magit
(use-package magit
  :ensure t
  :config
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-completing-read-function 'ivy-completing-read)))

;; gitignore-mode
(use-package gitignore-mode
  :ensure t
  :config
  (progn
    (add-hook 'gitignore-mode-hook (lambda ()
                                (setq require-final-newline t)))))

;; ag.el
(use-package ag
  :ensure t
  :config
  (progn
    (add-hook 'ag-mode-hook 'toggle-truncate-lines)
    (setq ag-highlight-search t)))

;; projectile
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-mode-line
          '(:eval (format " [%s]" (projectile-project-name))))
    (setq projectile-remember-window-configs t)
    (setq projectile-switch-project-action 'projectile-dired))
    (setq projectile-completion-system 'ivy))

;; perspective-el
(use-package perspective
  :ensure t
  :config
  (progn
    (setq persp-show-modestring nil)
    (add-hook 'persp-switch-hook 'hack-dir-local-variables-non-file-buffer)
    (persp-mode)))

(use-package yaml-mode
  :ensure t
  :mode "\\.sls$"
  :config
  (progn
    (add-hook 'yaml-mode-hook (lambda ()
                                (setq require-final-newline t)))))

;; go-mode
(use-package go-mode
  :ensure t
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-k") 'godoc-at-point)))))

;; go-eldoc
(use-package go-eldoc
  :ensure t
  :config
  (progn
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

;; rst-mode
(use-package rst
  :config
  (progn
    (add-hook 'rst-mode-hook
              (lambda ()
                (local-set-key (kbd "C-M-h") 'backward-kill-word)
                (setq-local fill-column 80)
                (turn-on-auto-fill)))))

;; cc-mode
(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook
              (lambda ()
                (local-set-key (kbd "C-M-h") 'backward-kill-word)
                (local-set-key (kbd "C-c h") 'c-mark-function)))))

;; web-mode
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'")

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;; whole-line-ore-region
(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :config
  (progn
    (whole-line-or-region-mode 1)))

;; comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; ws-butler
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'ws-butler-mode)
    (add-hook 'jinja2-mode-hook 'ws-butler-mode)
    (add-hook 'rst-mode-hook 'ws-butler-mode)
    (add-hook 'yaml-mode-hook 'ws-butler-mode)))

;; company-go
(use-package company-go
  :ensure t)

;; company-jedi
(use-package company-jedi
  :ensure t)

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("M-/" . company-complete-common)
  :config
  (progn
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'my/python-mode-hook)
    (add-to-list 'company-backends 'company-go)
    (setq company-dabbrev-downcase nil)))

;; avy
(use-package avy
  :ensure t
  :bind
  (("C-c SPC" . avy-goto-word-1)))

;; ace-window
(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window))

(use-package flycheck-package
  :ensure t)

(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (flycheck-gometalinter-setup)
    (setq flycheck-gometalinter-vendor t)
    (setq flycheck-gometalinter-disable-linters
          '("structcheck" "varcheck" "errcheck" "aligncheck" "testify"
            "test" "interfacer" "gotype"))))

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'go-mode-hook 'flycheck-mode)
    (add-hook 'lua-mode-hook 'flycheck-mode)
    (add-hook 'sh-mode-hook 'flycheck-mode)
    (add-hook 'rst-mode-hook 'flycheck-mode)))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; expand-region
(use-package expand-region
  :ensure t
  :config
  (bind-key* "C-=" 'er/expand-region))

;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (add-to-list 'yas-snippet-dirs (expand-file-name
                                    "yasnippet-snippets" user-emacs-directory))
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)))

;; help key binding
(bind-key "C-z" 'help-command)

;; dockerfile-mode
(use-package dockerfile-mode
  :ensure t)

;; elixir
(use-package elixir-mode
  :ensure t)

;; install packages not available in melpa stable
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; jinja2 mode, https://github.com/paradoxxxzero/jinja2-mode
(use-package jinja2-mode)
(use-package go-rename)
(use-package persp-projectile)

;; change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

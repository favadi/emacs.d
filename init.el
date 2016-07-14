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
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 8 200 8))

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

;; confirm to quit
(setq confirm-kill-emacs #'y-or-n-p)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; toggle fullscreen
(global-set-key (kbd "C-c C-c C-f") 'toggle-frame-fullscreen)

;; help key binding
(bind-key "C-z" 'help-command)

;; solarized-theme
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-(point)itch nil)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))

;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (use-package exec-path-from-shell
        :ensure t
        :config
        (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))
      ;; use bash installed from brew
      (setq explicit-shell-file-name "/opt/local/bin/bash")
      (set-frame-font "PragmataPro Mono 12" t t)
      (setq mac-right-option-modifier 'control)
      ;; macOS ls doesn't support --dired
      (setq dired-use-ls-dired nil)))

;; electric-pair-mode
(electric-pair-mode 1)
(show-paren-mode 1)

;; to suppress -Chg in mode line
(use-package hilit-chg
  :diminish highlight-changes-mode)

;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; gitignore-mode
(use-package gitignore-mode
  :ensure t
  :config
  (add-hook 'gitignore-mode-hook (lambda ()
                                   (setq require-final-newline t))))

;; ag.el
(use-package ag
  :ensure t
  :config
  (add-hook 'ag-mode-hook 'toggle-truncate-lines)
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

;; ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired))

;; swiper
(use-package swiper
  :ensure t
  :bind ("\C-s" . swiper))

;; counsel
(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-z f" . counsel-describe-function)
  ("C-z v" . counsel-describe-variable)
  ("C-c k" . counsel-ag))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.sls$"
  :config
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq require-final-newline t))))

;; go-mode
(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (subword-mode)
                            (local-set-key (kbd "C-c C-k") 'godoc-at-point))))

;; go-direx
(use-package go-direx
  :ensure t
  :config
  (define-key go-mode-map (kbd "C-c C-t") 'go-direx-switch-to-buffer))

;; go-eldoc
(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; rst-mode
(use-package rst
  :config
  (add-hook 'rst-mode-hook
            (lambda ()
              (local-set-key (kbd "C-M-h") 'backward-kill-word)
              (setq-local fill-column 80)
              (turn-on-auto-fill))))

;; cc-mode
(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "C-M-h") 'backward-kill-word)
              (local-set-key (kbd "C-c h") 'c-mark-function))))

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
  (whole-line-or-region-mode 1))

;; comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; ws-butler
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'jinja2-mode-hook 'ws-butler-mode)
  (add-hook 'rst-mode-hook 'ws-butler-mode)
  (add-hook 'yaml-mode-hook 'ws-butler-mode))

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
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  (add-to-list 'company-backends 'company-go)
  (setq company-dabbrev-downcase nil))

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
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-fast t)
  (setq flycheck-gometalinter-disable-linters '("gotype")))

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'rst-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode))

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
  (add-to-list 'yas-snippet-dirs (expand-file-name
                                  "yasnippet-snippets" user-emacs-directory))
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;; dockerfile-mode
(use-package dockerfile-mode
  :ensure t)

;; elixir
(use-package elixir-mode
  :ensure t)

;; protobuf
(use-package protobuf-mode
  :ensure t)

;; js-mode
(use-package js
  :config
  (setq js-indent-level 2))

;; install packages not available in melpa stable
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;; jinja2 mode, https://github.com/paradoxxxzero/jinja2-mode
(use-package jinja2-mode
  :load-path "~/go-dev-tools/src/golang.org/x/tools/refactor/rename")
(use-package go-rename
  :bind ("C-c r" . go-rename)
  :load-path "~/go-dev-tools/src/golang.org/x/tools/cmd/guru")
(use-package go-guru)

;; change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (setq sp-autoescape-string-quote nil)
    (show-smartparens-global-mode 1)))

;; magit
(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode)

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
  :diminish helm-mode
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
    (define-key helm-map (kbd "C-h") nil)
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
    (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
    (setq projectile-remember-window-configs t)))

;; perspective-el
(use-package perspective
  :ensure t
  :config
  (progn
    (persp-mode)))

;; persp-projectile
(use-package persp-projectile
  :ensure t)

;; helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (progn
    (helm-projectile-on)))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.sls$"
  :config
  (progn
    (add-hook 'yaml-mode-hook (lambda ()
                                (setq require-final-newline t)))))

;; jinja2-mode
(use-package jinja2-mode
  :ensure t
  :mode "\\.jinja\\'")

;; go-mode
(use-package go-mode
  :ensure t
  :config
  (progn
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") #'godef-jump)
                          (local-set-key (kbd "C-c C-k") 'godoc-at-point)
                          (local-set-key (kbd "C-u C-c C-k") 'godoc)))))

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
              (lambda()
                (setq-local fill-column 80)
                (turn-on-auto-fill)
                (local-set-key (kbd "C-M-h") 'backward-kill-word)
                (local-set-key (kbd "C-c h") 'rst-mark-section)))))

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
    (defadvice whole-line-or-region-kill-region
      (before whole-line-or-region-kill-read-only-ok activate)
      (interactive "p")
      (unless kill-read-only-ok (barf-if-buffer-read-only)))
    (whole-line-or-region-mode 1)))

;; evil-nerd-commenter
(use-package evil-nerd-commenter
  :ensure t
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

;; hilit-chg
(use-package hilit-chg
  :diminish highlight-changes-mode)

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
  :diminish company-mode
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
  :init
  (progn
    (add-hook 'go-mode-hook (lambda()
                              (setq-local helm-dash-docsets '("Go"))))

    (add-hook 'yaml-mode-hook (lambda()
                                (setq-local helm-dash-docsets '("SaltStack"))))
    (add-hook 'python-mode-hook (lambda()
                                (setq-local helm-dash-docsets '("Python"))))))

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode)
  :config
  (progn
    (setq ace-jump-mode-scope 'window)))

;; quickrun
(use-package quickrun
  :ensure t
  :bind ("C-c q" . quickrun)
  :config
  (progn
    (setq quickrun-timeout-seconds nil)))

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

;; go-flycheck
(use-package go-flycheck
  :load-path "~/go/src/github.com/dougm/goflymake")

;; simplenote2.el
(use-package simplenote2
  :ensure t
  :init
  (progn
    (setq simplenote2-auth-file "~/.simplenote")
    (if (file-exists-p simplenote2-auth-file)
        (progn (setq lines
                     (with-temp-buffer
                       (insert-file-contents simplenote2-auth-file)
                       (split-string (buffer-string) "\n" t)))
               (setq simplenote2-email (car lines))
               (setq simplenote2-password (nth 1 lines))))
    (simplenote2-setup))
  :config
  :bind (("C-c s b" . simplenote2-browse)
         ("C-c s p" . simplenote2-push-buffer)
         ("C-c s P" . simplenote2-pull-buffer)
         ("C-c s f" . simplenote2-filter-note-by-tag)
         ("C-c s t" . simplenote2-add-tag)
         ("C-c s i" . simplenote2-set-pinnped)
         ("C-c s m" . simplenote2-set-markdown)))

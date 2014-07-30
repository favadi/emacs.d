;; show column
(setq column-number-mode t)

;; enable recent files mode
(require 'recentf)
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; delete selection on insert
(delete-selection-mode +1)

;; jump to current directory
(require 'dired-x)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(setq sp-autoescape-string-quote nil)   ;disable auto escape
(show-smartparens-global-mode +1)       ;show matching delimiters

;; https://github.com/emacsmirror/whole-line-or-region
(require 'whole-line-or-region)

(defun whole-line-or-region-comment-dwim-2 (prefix)
  "Call `comment-dwim' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-call-with-prefix 'comment-dwim prefix nil t))

(add-to-list 'whole-line-or-region-extensions-alist
             '(comment-dwim whole-line-or-region-comment-dwim-2 nil))
(whole-line-or-region-mode)

;; flycheck
(require 'flycheck)
;; only check if the mode is enabled or the buffer was saved
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'lua-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'rst-mode-hook 'flycheck-mode)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-set-key "\M-/" 'auto-complete)
(setq ac-auto-show-menu nil)
(setq ac-ignore-case nil)
;; use C-n, C-p to select
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; use yaml-mode for salt state files
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

;; yaml
(add-hook 'yaml-mode-hook
          (lambda ()
            (auto-complete-mode)
            (local-set-key (kbd "C-j") 'smart-open-line)))

;; slime
(require 'slime-autoloads)
(setq inferior-lisp-program "clisp")
(setq slime-contribs '(slime-fancy))

;; lua
(setq lua-indent-level 2)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; jinja2
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))
(add-hook 'jinja2-mode-hook
          (lambda ()
            (auto-complete-mode)))

;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(setq ace-jump-mode-scope 'global)

;; rst
(add-hook 'rst-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (local-set-key (kbd "C-M-h") 'backward-kill-word)
            (local-set-key (kbd "C-c h") 'rst-mark-section)
            (local-set-key (kbd "C-=") 'er/expand-region)))

(require 'auto-complete-rst)
(auto-complete-rst-init)

;; the silver search - ag
(setq ag-highlight-search t)

;; projectile
(projectile-global-mode)
(setq projectile-mode-line-lighter "Prj")
(setq projectile-completion-system 'helm)

;; smart-mode-line
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'respectful)
;; (setq sml/hidden-modes '(" AC" " WLR" " SP" " ew:mnlt"))
;; (setq sml/name-width 30)

;; undo-tree
(global-undo-tree-mode)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; change inner
(global-set-key (kbd "C-c i") 'change-inner)
(global-set-key (kbd "C-c o") 'change-outer)

;; abbrev
(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...

(setq save-abbrevs t)              ;; save abbrevs when files are saved

;; ws-butler
(add-hook 'prog-mode-hook 'ws-butler-mode)
(add-hook 'jinja2-mode-hook 'ws-butler-mode)
(add-hook 'yaml-mode-hook 'ws-butler-mode)

;; making buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq delete-by-moving-to-trash t)

;; do not use tab
(setq-default indent-tabs-mode nil)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-command-prefix-key "C-c l")
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'conf-common)

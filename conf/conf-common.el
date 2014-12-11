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

;; enble winner mode
(winner-mode 1)

;; flycheck
(require 'flycheck)
;; only check if the mode is enabled or the buffer was saved
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'lua-mode-hook 'flycheck-mode)
(add-hook 'sh-mode-hook 'flycheck-mode)
(add-hook 'rst-mode-hook 'flycheck-mode)

;; use yaml-mode for salt state files
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

;; yaml
(add-hook 'yaml-mode-hook
          (lambda ()
            (local-set-key (kbd "C-j") 'smart-open-line)))

;; slime
(require 'slime-autoloads)
(setq inferior-lisp-program "clisp")
(setq slime-contribs '(slime-fancy))

;; lua
(setq lua-indent-level 2)

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; jinja2
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . jinja2-mode))

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

;; projectile
(projectile-global-mode)
(setq projectile-mode-line-lighter "Prj")
(setq projectile-remember-window-configs t)
(require 'helm-projectile)
(helm-projectile-on)

;; smart-mode-line
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'respectful)

;; undo-tree
(global-undo-tree-mode)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

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
(global-set-key (kbd "M-x") 'helm-M-x)  ;; use helm for M-x
(setq helm-command-prefix-key "C-c l")  ;; better helm prefix

;; turn on helm
(require 'helm-config)
(helm-mode 1)

;; helm global key bindings
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char) ; use C-h to delete with helm
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; auto active virtualenv
(add-hook 'python-mode-hook (lambda ()
                              (hack-local-variables)
                              (when (boundp 'project-venv-name)
                                (venv-workon project-venv-name))))

;; always load project-venv-name from dir-locals.el
(put 'project-venv-name 'safe-local-variable #'stringp)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
    '(progn
        (add-to-list 'company-backends 'company-anaconda)))
(global-set-key (kbd "M-/") 'company-complete-common)

;; anaconda-mode
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)

;; helm-dash
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
(add-hook 'python-mode-hook 'dash-python-doc)

(global-set-key (kbd "C-c C-o") 'helm-dash-at-point)
(global-set-key (kbd "C-c o") 'helm-dash)

;; go mode
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'conf-common)

;; enable ido everywhere
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; use ido for bookmark
(global-set-key (kbd "C-x r b")
    (lambda ()
      (interactive)
      (bookmark-jump
       (ido-completing-read "Jump to bookmark: " (bookmark-all-names)))))

;; ido style for M-x
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; enable recent files mode
(require 'recentf)
(recentf-mode t)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; delete selection on insert
(delete-selection-mode +1)

;; cleanup traling white space
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; jump to current directory
(require 'dired-x)
(global-set-key (kbd "C-x Cj") 'dired-jump)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
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

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-set-key "\M-/" 'auto-complete)
;; use C-n, C-p to select
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; use yaml-mode for salt state files
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

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

(provide 'conf-common)

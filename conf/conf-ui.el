;; turn off scrollbar, menubar, toolbar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; load solarized theme
(load-theme 'solarized-dark t)

;; Set default font
(add-to-list 'default-frame-alist
             '(font . "Consolas-13"))

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
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; highlight current line
(global-hl-line-mode +1)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; change tab width
(setq-default tab-width 4)

(provide 'conf-ui)

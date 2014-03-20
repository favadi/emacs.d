;; turn off scrollbar, menubar, toolbar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; load solarized theme
(setq solarized-use-variable-pitch nil)  ; disable variable size font
;; do not use different font size for org mode
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
;; load the theme
(load-theme 'solarized-light t)

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

(provide 'conf-ui)

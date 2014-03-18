;; Enable Emacs auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; invoke M-x without the Alt key
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; use ibuffer instead of buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; use C-h as backspace
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; rebinding mark-defun
(global-set-key (kbd "C-c h") 'mark-defun)

;; rebinding help-command
(global-set-key (kbd "C-z") 'help-command)

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "C-j") 'smart-open-line)


(provide 'conf-keyboard)

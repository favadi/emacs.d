;; Enable Emacs auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; use ibuffer instead of buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; use C-h as backspace
(setf (global-key-binding (kbd "C-h")) (kbd "<backspace>"))
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; rebinding mark-defun
(global-set-key (kbd "C-c h") 'mark-defun)

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "C-j") 'smart-open-line)

;; enable case control
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; switch to most recent buffer
(global-set-key (kbd "C-c b") 'mode-line-other-buffer)

(provide 'conf-keyboard)

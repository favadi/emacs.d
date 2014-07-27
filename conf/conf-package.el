; Use package.el to manage packages
(require 'package)
;; Milkypostman’s Emacs Lisp Package Archive
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; http://blog.zhengdong.me/2012/03/14/how-i-manage-emacs-packages
;; Guarantee all packages are installed on start
(require 'cl)
(defvar packages-list
  '(
    ace-jump-mode
    ag
    auto-complete
    auto-complete-rst
    change-inner
    expand-region
    flycheck
    gitignore-mode
    gnuplot-mode
    go-mode
    ido-ubiquitous
    jedi
    jinja2-mode
    lua-mode
    magit
    markdown-mode
    projectile
    slime
    smart-mode-line
    smartparens
    smex
    solarized-theme
    undo-tree
    virtualenvwrapper
    whole-line-or-region
    ws-butler
    yaml-mode
    )
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'conf-package)

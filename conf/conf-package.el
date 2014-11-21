; Use package.el to manage packages
(require 'package)
;; Milkypostman’s Emacs Lisp Package Archive
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; http://blog.zhengdong.me/2012/03/14/how-i-manage-emacs-packages
;; Guarantee all packages are installed on start
(require 'cl)
(defvar packages-list
  '(
    ace-jump-mode
    anaconda-mode
    company
    company-anaconda
    expand-region
    flycheck
    git-timemachine
    gitignore-mode
    gnuplot-mode
    go-mode
    goto-last-change
    helm
    helm-ag
    helm-dash
    helm-projectile
    jinja2-mode
    lua-mode
    magit
    markdown-mode
    projectile
    rich-minority
    slime
    smart-mode-line
    smartparens
    smex
    solarized-theme
    undo-tree
    virtualenvwrapper
    web-mode
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

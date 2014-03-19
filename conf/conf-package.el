; Use package.el to manage packages
(require 'package)
;; Milkypostman’s Emacs Lisp Package Archive
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Latest Org mode
(add-to-list 'package-archives
  '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; http://blog.zhengdong.me/2012/03/14/how-i-manage-emacs-packages
;; Guarantee all packages are installed on start
(require 'cl)
(defvar packages-list
  '(solarized-theme
    org
    markdown-mode
    gitignore-mode
    gnuplot-mode
    ethan-wspace
    smartparens
    whole-line-or-region
    flycheck
    ido-vertical-mode
    smex
    auto-complete
    jedi
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

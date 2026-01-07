;; File: early-init.el

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; 用 straight.el 做包管理，保证包的可重复构建与独立性
(defvar bootstrap-version)
(let* ((straight-url "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")
	       (bootstrapfile (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrapfile)
    (with-current-buffer
        (url-retrieve-synchronously straight-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrapfile nil 'nomessage))

;; 让 use-package 通过 straight 安装
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'use-package)

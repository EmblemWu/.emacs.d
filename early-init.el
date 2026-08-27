;; File: early-init.el

;; 加载路径：lisp/ 下存放本配置的模块文件
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 性能：用 straight 接管后，禁用 package.el 的初始化；启动期调高 GC 阈值，结束后恢复
(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 64 1024 1024))))

;; 提升 LSP 类进程（如 copilot 的 language server）输出吞吐
(setq read-process-output-max (* 1024 1024))

;; 用 straight.el 做包管理，保证包的可重复构建与独立性
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 让 use-package 通过 straight 安装
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'use-package)

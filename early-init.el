;; File: early-init.el

;;;; 0. 跨平台操作系统判定宏（解耦 macOS / Linux / OpenBSD）
(defconst sys/mac-p (eq system-type 'darwin))
(defconst sys/linux-p (eq system-type 'gnu/linux))
(defconst sys/bsd-p (memq system-type '(berkeley-unix openbsd freebsd netbsd)))

(defmacro when-mac (&rest body)
  "仅在 macOS (darwin) 系统下执行 BODY。"
  (declare (indent 0))
  `(when sys/mac-p ,@body))

(defmacro when-linux (&rest body)
  "仅在 GNU/Linux 系统下执行 BODY。"
  (declare (indent 0))
  `(when sys/linux-p ,@body))

(defmacro when-bsd (&rest body)
  "仅在 BSD 系列系统 (OpenBSD, FreeBSD) 下执行 BODY。"
  (declare (indent 0))
  `(when sys/bsd-p ,@body))

;; 通用环境 PATH 解析（自动探测 Homebrew / Unix local / 用户 bin，兼顾 Mac/Linux/OpenBSD）
(dolist (dir (list "/opt/homebrew/bin"
                   "/usr/local/bin"
                   (expand-file-name "~/.local/bin")))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))))

;;;; 1. 极致启动提速（抑制文件处理与启动开销）
;; 启动期间临时清空 file-name-handler-alist，跳过每个 require 的正则匹配，启动后自动恢复
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; 加载路径：lisp/ 下存放本配置的模块文件
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 性能：用 straight 接管后，禁用 package.el 的初始化；启动期调高 GC 阈值至最大，结束后设为 128MB
(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))
            ;; 闲置 5 秒时才静默执行 GC，打字与高频操作期坚决不卡顿
            (run-with-idle-timer 5 t #'garbage-collect)))

;; 提升 LSP/Copilot 类子进程管道吞吐（从 1MB 升至 3MB，减少上下文切换）
(setq read-process-output-max (* 3 1024 1024))

;;;; 2. 跨平台原生渲染与高刷低延迟
(when-mac
  (setq inhibit-compacting-font-caches t))    ; 仅 macOS 需禁用 CoreText 字体缓存压缩以防掉帧
(setq-default bidi-paragraph-direction 'left-to-right) ; 纯代码默认从左往右，禁用无用的 RTL 扫描
(setq-default bidi-inhibit-bpa t)            ; 禁用双向括号算法匹配，长行与大文件性能暴增
(setq redisplay-dont-pause t)                ; 渲染不排队等待，保证 120Hz 视网膜流畅度
(setq native-comp-async-report-warnings-errors 'silent) ; 静默原生编译背景警告

;;;; 3. Straight.el 启动优化（避免启动时扫盘几十个 Git 仓库）
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; 跨版本兼容性补丁（Emacs 29 兼容旧宏与 Emacs 30 新 API，如 vertico 所需的 incf/decf 与 set-local）
(require 'cl-lib)
(unless (fboundp 'incf)
  (defalias 'incf 'cl-incf))
(unless (fboundp 'decf)
  (defalias 'decf 'cl-decf))
(unless (fboundp 'set-local)
  (defsubst set-local (var val)
    "Set the local binding of VAR to VAL (compatibility for Emacs < 30)."
    (set (make-local-variable var) val)))

;; 兼容 (seconds-to-string DELAY &optional EXPANDED ABBREV) 多参数调用
(with-eval-after-load 'time-date
  (advice-add 'seconds-to-string :around
              (lambda (orig-fun delay &rest args)
                (condition-case nil
                    (apply orig-fun delay args)
                  (wrong-number-of-arguments
                   (funcall orig-fun delay))))))

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

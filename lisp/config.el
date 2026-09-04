;; File: config.el
;; 基础配置、编辑行为与杂项调优

;;;; 1. 交互与确认调优（人性化提效）
(setq use-short-answers t)               ; Emacs 28+: 简短确认，y/n 替代 yes/no
(defalias 'yes-or-no-p 'y-or-n-p)       ; 兼容旧接口
(setq ring-bell-function 'ignore)        ; 静音所有刺耳的蜂鸣提示
(setq confirm-kill-emacs nil)            ; 退出时无需多余询问

;;;; 2. 文件与目录卫生（彻底杜绝项目目录下的垃圾文件污染）
(let ((backup-dir (expand-file-name ".cache/backups/" user-emacs-directory))
      (autosave-dir (expand-file-name ".cache/auto-save/" user-emacs-directory)))
  (unless (file-directory-p backup-dir) (make-directory backup-dir t))
  (unless (file-directory-p autosave-dir) (make-directory autosave-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,autosave-dir t))
        backup-by-copying t              ; 使用拷贝制作备份，保留原文件软链接与属性
        delete-old-versions t            ; 自动静默删除旧备份
        kept-new-versions 5              ; 保留最新 5 个版本
        kept-old-versions 2              ; 保留最初 2 个版本
        version-control t))              ; 开启备份版本号

;;;; 3. 平滑滚动与极致渲染性能（告别触底触顶瞬间半屏跳动的眩晕感）
(setq scroll-conservatively 101          ; 逐行平滑滚动
      scroll-margin 2                    ; 视口边缘留 2 行余量
      scroll-preserve-screen-position t  ; 翻页时尽量维持光标在屏幕的相对位置
      fast-but-imprecise-scrolling t)    ; 提升大文件滚动流畅度

;; 行号渲染性能压榨（避免每次滚屏重算行号边距宽度）
(setq-default display-line-numbers-grow-only t
              display-line-numbers-width-start t)

;; 语法高亮异步微调度（打字与快速翻页时零卡顿）
(setq jit-lock-defer-time 0.05
      jit-lock-stealth-time 1.0)

;; Minibuffer 零感知响应：补全输入期间彻底挂起 GC，退出时恢复
(defun my-minibuffer-setup-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-gc ()
  (setq gc-cons-threshold (* 128 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-gc)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-gc)

;;;; 4. 自动化与跨会话记忆（状态记忆与零摩擦）
(electric-pair-mode 1)                   ; 自动补全成对的括号与引号 () [] {} ""
(save-place-mode 1)                      ; 重新打开文件时自动跳转到上次光标位置
(recentf-mode 1)                         ; 记录最近打开的文件
(setq recentf-max-saved-items 200
      recentf-exclude '("/tmp/" "/ssh:" "\\.cache" "\\.git/"))
(savehist-mode 1)                        ; 持久化 Minibuffer 历史记录、搜索词与命令
(global-auto-revert-mode 1)              ; 磁盘文件被 Git 或外部程序修改时无感知自动刷新
(setq auto-revert-verbose nil)           ; 静默刷新，不刷屏提示

;;;; 5. 文件管理（Dired 人性化增强）
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t                ; 双栏分屏时，复制/重命名自动以另一栏为目标路径
      dired-recursive-copies 'always     ; 递归复制目录不反复弹窗询问
      dired-recursive-deletes 'top)      ; 递归删除目录仅询问一次顶部

;;;; 6. LaTeX 自动编译
;; 1. 静默编译：xelatex 非交互模式，遇错即停，输出 file:line 便于定位
(defun my-xelatex-quiet ()
  "Save and quietly compile current TeX file with xelatex, minimal output."
  (interactive)
  (unless (and buffer-file-name (string-match-p "\\.tex\\'" buffer-file-name))
    (user-error "当前 buffer 不是 .tex 文件"))
  (save-buffer)
  (let* ((cmd (format "xelatex -interaction=nonstopmode -halt-on-error -file-line-error -synctex=1 %s"
                      (shell-quote-argument buffer-file-name))))
    (compile cmd)))

;; 2. 保存时自动编译（仅 .tex 文件）
(defun my-latex-auto-compile-on-save ()
  "On save, quietly compile current TeX file with xelatex."
  (when (and buffer-file-name (string-match-p "\\.tex\\'" buffer-file-name))
    (my-xelatex-quiet)))

;; 3. LaTeX 模式下仅注册一次，避免重复叠加
(add-hook 'latex-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") #'my-xelatex-quiet)
            (add-hook 'after-save-hook #'my-latex-auto-compile-on-save 0 t)))

;; 4. 编译输出显示策略：底部侧窗固定高度
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 12)))

;; 编译异常退出时主动弹出输出窗口（用 add-hook，避免覆盖其它包的钩子）
(add-hook 'compilation-finish-functions
          (lambda (buf msg)
            (when (string-match "exited abnormally" msg)
              (display-buffer buf))))

;;;; 7. Org-mode 思考与行动闭环（Living in Emacs 核心中枢）
(setq org-directory (expand-file-name "~/org")
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                             (expand-file-name "tasks.org" org-directory))
      org-todo-keywords '((sequence "TODO(t)" "DOING(i!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-log-done 'time
      org-log-into-drawer t
      org-startup-indented t
      org-hide-emphasis-markers t
      org-ellipsis " ▾"
      org-return-follows-link t)

;; Org Capture 随手捕获模板：零认知摩擦，随手打断随时存
(setq org-capture-templates
      '(("t" "待办任务 (Task)" entry
         (file+headline "~/org/tasks.org" "收集箱与待办")
         "* TODO %?\n  SCHEDULED: %t\n  录入时间: %U\n  来源: %a\n  %i"
         :empty-lines 1)
        ("i" "快速闪念 (Inbox)" entry
         (file+headline "~/org/inbox.org" "快速闪念")
         "* %?\n  录入时间: %U\n  %i"
         :empty-lines 1)
        ("n" "知识随笔 (Note)" entry
         (file+headline "~/org/notes.org" "知识沉淀与备忘")
         "* %? :NOTE:\n  录入时间: %U\n  来源: %a\n\n  %i"
         :empty-lines 1)))

;;;; 8. 悬浮式极速内嵌终端（告别 Command-Tab 频繁切窗口）
(require 'term)
(defvar my/terminal-buffer-name "*terminal-drawer*")

(defun my/toggle-terminal ()
  "Toggle a dedicated bottom-drawer terminal window."
  (interactive)
  (let ((term-win (get-buffer-window my/terminal-buffer-name)))
    (if term-win
        (if (eq (selected-window) term-win)
            (delete-window term-win)
          (select-window term-win))
      (let ((buf (get-buffer my/terminal-buffer-name)))
        (unless (and buf (buffer-live-p buf) (get-buffer-process buf))
          (let ((shell-prog (or (getenv "SHELL")
                                (executable-find "zsh")
                                (executable-find "bash")
                                "/bin/sh")))
            (setq buf (make-term "terminal-drawer" shell-prog)))
          (with-current-buffer buf
            (term-char-mode)))
        (let ((win (display-buffer-in-side-window
                    buf
                    '((side . bottom)
                      (window-height . 0.35)
                      (slot . 0)))))
          (select-window win))))))

;; 确保在终端内敲快捷键也能瞬间收缩抽屉
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-c t") #'my/toggle-terminal)
  (define-key term-mode-map (kbd "C-c t") #'my/toggle-terminal))

;;;; 9. 多任务工作区与项目管理（Tab-bar + Project.el）
(setq tab-bar-show 1                     ; 仅在有多个工作区时自动显示标签栏，单工作区保持纯净
      tab-bar-close-button-show nil      ; 隐藏无用 X 按钮
      tab-bar-new-button-show nil        ; 隐藏 + 按钮
      tab-bar-tab-hints t                ; 显示数字标签 1, 2, 3...
      tab-bar-select-tab-modifiers '(meta) ; M-1, M-2, M-3 瞬时直达指定工作区
      project-vc-extra-root-markers '(".git" "package.json" "Cargo.toml"))

(tab-bar-mode 1)

(provide 'config)

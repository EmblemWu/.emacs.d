;; File: config.el
;; 基础配置与杂项

;;;; 文件管理
;; 允许在 dired 中用 d 直接进入子目录
(put 'dired-find-alternate-file 'disabled nil)

;;;; LaTeX 自动编译
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

;; 编译异常退出时主动弹出输出窗口
(setq compilation-finish-functions
      (list (lambda (buf msg)
              (when (string-match "exited abnormally" msg)
                (display-buffer buf)))))

(provide 'config)

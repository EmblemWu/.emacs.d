;;File: config.el

;; ============================================================================
;; LaTeX 自动编译配置
;; ============================================================================

;; 1. 静默编译函数
(defun my-xelatex-quiet ()
  "Save and quietly compile current TeX file with xelatex, minimal output."
  (interactive)
  (unless (and buffer-file-name (string-match-p "\\.tex\\'" buffer-file-name))
    (user-error "当前 buffer 不是 .tex 文件"))
  (save-buffer)
  ;; -interaction=nonstopmode: 不在错误处停下
  ;; -halt-on-error: 遇致命错误尽快退出
  ;; -file-line-error: 便于 Emacs 定位错误
  ;; -synctex=1: 生成同步信息
  (let* ((cmd (format "xelatex -interaction=nonstopmode -halt-on-error -file-line-error -synctex=1 %s"
                      (shell-quote-argument buffer-file-name))))
    (compile cmd)))

;; 2. 保存时自动编译（仅在 .tex 文件）
(defun my-latex-auto-compile-on-save ()
  "On save, quietly compile current TeX file with xelatex."
  (when (and buffer-file-name (string-match-p "\\.tex\\'" buffer-file-name))
    (my-xelatex-quiet)))

;; 3. LaTeX 模式下仅注册一次，避免重复叠加
(add-hook 'latex-mode-hook
          (lambda ()
            ;; 手动快捷键：C-c C-c
            (local-set-key (kbd "C-c C-c") #'my-xelatex-quiet)
            ;; 保存自动编译（buffer-local，不会重复注册）
            (add-hook 'after-save-hook #'my-latex-auto-compile-on-save 0 t)))

;; 4. 编译缓冲区显示策略
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               ;; 使用 side-window 在底部并设定固定高度
               (window-height . 12)))

(setq compilation-finish-functions
      (list (lambda (buf msg)
              (when (string-match "exited abnormally" msg)
                (display-buffer buf)))))

(provide 'config)

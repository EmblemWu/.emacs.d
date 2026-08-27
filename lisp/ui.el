;; File: ui.el
;; 界面、字体与主题

;;;; 基础界面
(global-display-line-numbers-mode)
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-buffer-choice t)   ; 启动时显示 *scratch*

;;;; 字体
;; Height 单位为 1/10 pt（170 = 17pt）
(set-face-attribute 'default nil :family "Roboto mono" :height 170 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "Fira Sans" :height 190 :weight 'bold)
(when (facep 'fixed-pitch-serif)
  (set-face-attribute 'fixed-pitch-serif nil :family "Fira Code" :height 210 :weight 'regular))

;; 中文字体映射，避免中英文混排时 fallback 不一致
(let ((cn-font (seq-some (lambda (f)
                           (when (member f (font-family-list)) f))
                         '("PingFang SC" "Noto Sans CJK SC" "Source Han Sans SC" "Hiragino Sans GB"))))
  (when cn-font
    (set-fontset-font t 'han (font-spec :family cn-font :size 16))
    (set-fontset-font t 'kana (font-spec :family cn-font :size 16))
    (set-fontset-font t 'cjk-misc (font-spec :family cn-font :size 16))))

;;;; 主题：auto-dark 的配置与启用已迁移到 packages.el 的 use-package 中

;;;; macOS 窗口细节
;; 移除装饰 + 最大化；如需系统原生全屏，把 fullscreen 改为 'fullboth 并设 (undecorated . nil)
(setq default-frame-alist
      '((undecorated . t)
        (fullscreen . maximized)
        (ns-transparent-titlebar . t)))

(provide 'ui)

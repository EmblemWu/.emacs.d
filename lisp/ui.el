;; File: ui.el
;; 界面、字体与主题

;;;; 基础界面与现代交互细节
(global-display-line-numbers-mode)
(global-hl-line-mode 1)          ; 高亮当前光标所在行，清晰辨位
(delete-selection-mode 1)        ; 选中文字后直接输入即可覆盖（现代编辑器标配）
(show-paren-mode 1)              ; 即时高亮匹配的成对括号
(setq show-paren-delay 0.1)
(blink-cursor-mode -1)           ; 禁用光标闪烁定时器，降低 CPU 唤醒与渲染能耗
(setq frame-inhibit-implied-resize t) ; 禁止字体载入时的隐式窗口尺寸重排，消除启动闪烁
(setq inhibit-startup-screen t)  ; 彻底禁用启动 Splash 屏幕开销
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-buffer-choice t)   ; 启动时显示 *scratch*

;; 极简细分屏边框，多窗口分屏时视觉干净利落
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

;;;; 字体
;; Height 单位为 1/10 pt（170 = 17pt）
(set-face-attribute 'default nil :family "Roboto mono" :height 170 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "Fira Sans" :height 190 :weight 'bold)
(when (facep 'fixed-pitch-serif)
  (set-face-attribute 'fixed-pitch-serif nil :family "Fira Code" :height 210 :weight 'regular))

;; 中文字体映射，避免中英文混排时 fallback 不一致（兼顾 Mac / Linux / OpenBSD 常见中文字体）
(let ((cn-font (seq-some (lambda (f)
                           (when (member f (font-family-list)) f))
                         '("PingFang SC" "Noto Sans CJK SC" "Source Han Sans SC"
                           "Hiragino Sans GB" "WenQuanYi Micro Hei" "WenQuanYi Zen Hei"))))
  (when cn-font
    (set-fontset-font t 'han (font-spec :family cn-font :size 16))
    (set-fontset-font t 'kana (font-spec :family cn-font :size 16))
    (set-fontset-font t 'cjk-misc (font-spec :family cn-font :size 16))))

;;;; 主题：auto-dark 的配置与启用已迁移到 packages.el 的 use-package 中

;;;; 窗口外观细节（无边框、最大化，macOS 专属透明标题栏通过宏解耦）
(setq default-frame-alist
      `((undecorated . t)
        (fullscreen . maximized)
        ,@(when sys/mac-p
            '((ns-transparent-titlebar . t)))))

;;;; 终端调色板自适应（对齐 auto-dark，彻底根治浅色刺眼与深色对比度）
(require 'term)

(defvar my/term-light-palette
  (vector "#1e293b"  ; 0: 碳黑
          "#b91c1c"  ; 1: 沉稳砖红
          "#15803d"  ; 2: 森林深绿（告别浅底荧光绿）
          "#b45309"  ; 3: 暖琥珀褐（告别白底刺眼黄）
          "#1d4ed8"  ; 4: 海军深蓝
          "#7e22ce"  ; 5: 雅致深紫
          "#0e7490"  ; 6: 深靛青（高对比温润青，彻底不刺眼）
          "#f8fafc") ; 7: 柔和浅白
  "浅色背景下高对比、低刺激的护眼调色板。")

(defvar my/term-dark-palette
  (vector "#1a1b26"  ; 0: 幽夜黑
          "#f7768e"  ; 1: 柔光浅红
          "#9ece6a"  ; 2: 柔和草绿
          "#e0af68"  ; 3: 柔和暖黄
          "#7aa2f7"  ; 4: 天空柔蓝
          "#bb9af7"  ; 5: 浅薰衣草
          "#7dcfff"  ; 6: 清澈青蓝
          "#c0caf5") ; 7: 银白文字
  "深色背景下舒适柔和的低刺目调色板。")

(defun my/apply-term-palette (&optional dark-p)
  "根据深浅色模式动态重映射终端 ANSI 8 色调色板。"
  (let* ((is-dark (if (null dark-p)
                      (or (eq (frame-parameter nil 'background-mode) 'dark)
                          (member 'alabaster-themes-dark custom-enabled-themes))
                    dark-p))
         (palette (if is-dark my/term-dark-palette my/term-light-palette))
         (faces '(term-color-black term-color-red term-color-green term-color-yellow
                  term-color-blue term-color-magenta term-color-cyan term-color-white)))
    ;; ansi-color.el 使用十六进制颜色字符串向量
    (setq ansi-color-names-vector palette)
    ;; 保持 Emacs 29 原生 17 元素 ansi-term-color-vector 结构不变，严禁修改数组元素
    ;; 实际颜色通过直接修改各个 term-color-* face 的前景与背景生效
    (dotimes (i 8)
      (let ((face (nth i faces))
            (color (aref palette i)))
        (set-face-attribute face nil :foreground color :background color)))))

;; 挂载到 auto-dark 与主题切换钩子
(add-hook 'auto-dark-light-mode-hook (lambda () (my/apply-term-palette nil)))
(add-hook 'auto-dark-dark-mode-hook  (lambda () (my/apply-term-palette t)))
(add-hook 'enable-theme-functions
          (lambda (theme)
            (my/apply-term-palette (string-match-p "dark" (symbol-name theme)))))

;; 启动后立即初次校准一次调色板
(add-hook 'after-init-hook #'my/apply-term-palette)

(provide 'ui)

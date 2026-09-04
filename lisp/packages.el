;; File: packages.el
;; 所有包均通过 straight 安装（early-init.el 中已设 straight-use-package-by-default t），
;; 此处不再写 :ensure

;;;; 现代补全与交互增强（人性化核心：极速、直观、按键自动提示）
(use-package which-key
  :init (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.4)             ; 按下前缀键 0.4s 后自动浮现可用按键清单
  (which-key-separator " → ")
  (which-key-prefix-prefix "+"))

(use-package vertico
  :straight (:host github :repo "minad/vertico" :tag "2.8")
  :init (vertico-mode 1)                 ; 现代垂直 Minibuffer 补全，淘汰横向单行
  :custom
  (vertico-count 12)                     ; 显示 12 行候选项
  (vertico-resize nil)
  (vertico-cycle t))                     ; 支持循环上下滚动

(use-package orderless
  :custom
  (completion-styles '(orderless basic)) ; 空格分隔的多关键词无序模糊匹配（如 "pkg el" 匹配 "packages.el"）
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :tag "2.10")
  :after vertico
  :init (marginalia-mode 1))             ; 在候选词旁显示丰富元数据（docstring、文件大小、修改日期）

(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)          ; 类似 swiper 的极速单 buffer 搜索与即时预览
         ("C-x b" . consult-buffer)      ; 现代多功能 buffer/最近文件/书签智能切换
         ("M-y" . consult-yank-pop)      ; 可视化剪贴板历史粘贴
         ("C-c r" . consult-ripgrep)))   ; 项目级极速文本搜索

(use-package ace-window
  :bind ("M-o" . ace-window)             ; 单键快速跳转多分割窗口，彻底告别连按 C-x o
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package magit
  :commands (magit-status)
  :bind ("C-c g" . magit-status))        ; Emacs 殿堂级 Git 操作界面

;;;; 通用增强
(use-package dired-preview
  :straight (:host github :repo "protesilaos/dired-preview")
  :hook (dired-mode . dired-preview-mode)
  :custom
  (dired-preview-delay 0.2)                  ; 触发预览的延迟时间（秒）
  (dired-preview-max-size (* 10 1024 1024))  ; 最大预览文件大小（10MB）
  :bind (:map dired-mode-map
              ("P" . dired-preview-mode)     ; 按 P 快速切换预览模式
              ("C-c p" . dired-preview-mode)))

(use-package emacs-everywhere
  :commands (emacs-everywhere))   ; 从任意程序唤起 Emacs 编辑

;; macOS 修复：仅在 macOS 上需要用 osacompile 重新编译 AppleScript 去除 resource fork
(when-mac
  (defun my/emacs-everywhere-fix-osacompile (&rest _)
    "Recompile emacs-everywhere's AppleScripts without resource forks."
    (let ((default-directory emacs-everywhere--dir))
      (dolist (script '("app-name" "window-title" "window-geometry"))
        (shell-command
         (format "xattr -d com.apple.ResourceFork %s 2>/dev/null; xattr -d com.apple.FinderInfo %s 2>/dev/null; osacompile -o %s %s.applescript"
                 script script script script)))))

  (with-eval-after-load 'emacs-everywhere
    (advice-add 'emacs-everywhere--ensure-oscascript-compiled :after
                #'my/emacs-everywhere-fix-osacompile)))
(use-package hide-mode-line :defer t)   ; 按需隐藏 mode-line（当前未启用，可移除）
(use-package mini-frame :defer t)       ; 迷你缓冲区浮窗（当前未启用，可移除）

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package editorconfig
  :config (editorconfig-mode +1))

;;;; 主题
(use-package alabaster-themes
  :commands (alabaster-themes-select))
(use-package ample-theme :defer t)
(use-package auto-dark             ; 跟随系统深/浅色，after-init 时启用 auto-dark-mode
  :custom
  (auto-dark-themes '((alabaster-themes-dark) (alabaster-themes-light)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :hook (after-init . auto-dark-mode))

;;;; Web 前端
(use-package vue-mode
  :mode "\\.vue\\'")

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[tsx]")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  (add-hook 'typescript-tsx-mode-hook
            (lambda ()
              (tree-sitter-mode)
              (tree-sitter-hl-mode))))

(use-package tree-sitter
  :commands (tree-sitter-mode))

(use-package tree-sitter-langs
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

;;;; Flutter / Dart
(use-package dart-mode
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path (or (executable-find "flutter") "/opt/homebrew/bin/flutter")))

;;;; Swift
(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift")

;;;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)   ; 抑制 missing indent offset 警告
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell" :files ("*.el" "*.png"))
  :commands (agent-shell agent-shell-openai-start-codex)
  :bind (("C-c a c" . agent-shell-openai-start-codex)
         ("C-c a a" . agent-shell))
  :config
  (setq agent-shell-preferred-agent-config 'codex
        agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t)
        agent-shell-openai-codex-acp-command '("codex-acp")
        agent-shell-openai-codex-environment
        (agent-shell-make-environment-variables :inherit-env t)))

;;;; LeetCode
(use-package leetcode
  :commands (leetcode leetcode-try leetcode-submit)
  :init
  ;; 登录说明：LeetCode 不允许第三方登录，插件通过浏览器 Cookie 恢复 Session，
  ;; 需先安装 Python3 包：pip3 install --user my_cookies
  (setq leetcode-prefer-language "cpp"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory (expand-file-name "~/leetcode"))
  :config
  ;; 题解 buffer 中关闭 IDE 类 minor mode，保持纯编辑体验
  (add-hook 'leetcode-solution-mode-hook
            (lambda ()
              (when (fboundp 'flycheck-mode)
                (flycheck-mode -1))
              (copilot-mode -1)
              (company-mode -1)))
  :bind (("C-c l l" . leetcode)
         ("C-c l t" . leetcode-try)
         ("C-c l s" . leetcode-submit)))

;; (use-package emacs-hnreader) ; 占位，待指定正确 recipe
(provide 'packages)

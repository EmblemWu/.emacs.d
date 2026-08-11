;; File: packages.el
;; 所有包均通过 straight 安装（early-init.el 中已设 straight-use-package-by-default t），
;; 此处不再写 :ensure

;;;; 通用增强
(use-package emacs-everywhere)   ; 从任意程序唤起 Emacs 编辑
(use-package hide-mode-line)     ; 按需隐藏 mode-line 的 minor mode
(use-package mini-frame)         ; 迷你缓冲区浮窗

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package editorconfig
  :config (editorconfig-mode +1))

;;;; 主题
(use-package alabaster-themes
  :commands (alabaster-themes-select))
(use-package ample-theme :defer t)
(use-package auto-dark :defer t)   ; 跟随系统深/浅色（启用与配色见 ui.el）

;;;; Web 前端
(use-package vue-mode)

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
  (flutter-sdk-path "/opt/homebrew/bin/flutter"))

;;;; Swift
(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift")

;;;; AI 辅助
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :custom
  (copilot-indent-offset-warning-disable t)   ; 抑制 missing indent offset 警告
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package agent-shell
  :straight (:host github :repo "xenodium/agent-shell" :files ("*.el" "*.png"))
  :commands (agent-shell agent-shell-openai-start-codex)
  :bind (("C-c a c" . agent-shell-openai-start-codex)
         ("C-c a a" . agent-shell))
  :init
  (let ((homebrew-bin "/opt/homebrew/bin"))
    (add-to-list 'exec-path homebrew-bin)
    (setenv "PATH" (concat homebrew-bin path-separator (getenv "PATH"))))
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

(provide 'packages)

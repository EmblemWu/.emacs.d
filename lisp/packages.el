;;File: packages.el

(use-package emacs-everywhere
   :config
   (setq emacs-everywhere-enable t))

(use-package hide-mode-line)

(use-package olivetti)

(use-package magit)

(use-package devdocs)

(use-package multiple-cursors)

(use-package fzf)

(use-package nyan-mode
  :hook
  (after-init . nyan-mode)
  :config
  (setq nyan-animate-nyancat t))

;; (use-package mini-frame
;; ;;  :hook
;; ;;  (after-init . mini-frame-mode)
;;   :config)

;; 主题包只在这里声明，具体启用/配置放到 ui.el
(use-package alabaster-emacs
  :straight (alabaster-emacs :type git :host github :repo "emblemwu/alabaster-emacs")
  :defer t)

;; (use-package ample-theme
;;   :ensure t
;;   :defer t)

(use-package auto-dark
  :init (auto-dark-mode))

;; Assuming usage with dart-mode
(use-package dart-mode
  ;; Optional
  :hook (dart-mode . flutter-test-mode))
(use-package tree-sitter :commands (tree-sitter-mode))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package tree-sitter-langs
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :custom
;;   (copilot-indent-offset-warning-disable t) ;; suppress missing indent offset warnings
;;   :config
;;   (add-hook 'prog-mode-hook 'copilot-mode)
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;;   :ensure t)


(use-package typescript-mode
  :mode "\.ts\'"
  :config
  (setq typescript-indent-level 2)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[tsx]")
  (add-to-list 'auto-mode-alist '("\.tsx\'" . typescript-tsx-mode))
  (add-hook 'typescript-tsx-mode-hook
	    (lambda ()
	      (tree-sitter-mode) (tree-sitter-hl-mode))) )


;; (use-package tsx-mode
;;   :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el" :branch "emacs29"))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/opt/homebrew/bin/flutter"))


;; ======================================Swift-configs===================================
;; .editorconfig file support
(use-package editorconfig
    :ensure t
    :config (editorconfig-mode +1))

;; Swift editing support
(use-package swift-mode
    :ensure t
    :mode "\\.swift\\'"
    :interpreter "swift")

;; Rainbow delimiters makes nested delimiters easier to understand
(use-package rainbow-delimiters
    :ensure t
    :hook ((prog-mode . rainbow-delimiters-mode)))

;; Company mode (completion)
;; (use-package company
;;     :ensure t
;;     :config
;;     (global-company-mode +1))

;; ;; Used to interface with swift-lsp.
;; (use-package lsp-mode
;;     :ensure t
;;     :commands lsp
;;     :hook ((swift-mode . lsp)))

;; lsp-mode's UI modules
(use-package lsp-ui
    :ensure t)

(use-package leetcode
  :ensure t
  :commands (leetcode leetcode-try leetcode-submit)
  :init
  ;; 可选：LeetCode 语言与 SQL 偏好（按 README 所述）
  (setq leetcode-prefer-language "cpp")   ;; 也可选 "cpp" "java" "go" 等
  (setq leetcode-prefer-sql "mysql")
  ;; 可选：默认不显示题目标签
  ;; (setq leetcode-prefer-tag-display nil)

  ;; 保存题解到本地
  (setq leetcode-save-solutions t)
  (setq leetcode-directory (expand-file-name "~/leetcode"))

  ;; 登录方式说明：
  ;; LeetCode 不允许第三方登录。插件通过浏览器 Cookie（Firefox/Chrome）恢复 Session。
  ;; 默认会安装 Python3 包 my_cookies 来读取浏览器 Cookie。
  ;; 请确保系统有 Python3，并执行：
  ;;   pip3 install --user my_cookies
  ;; 或者系统范围安装：
  ;;   pip3 install my_cookies

  :config
  ;; 可选：在 solution buffer 禁用 Flycheck 等 IDE 特性
  ;; 如果你喜欢纯编辑体验，启用下面的 hook；否则保持默认即可
  (add-hook 'leetcode-solution-mode-hook
            (lambda ()
              ;; 关闭 flycheck（若已安装）
              (when (fboundp 'flycheck-mode)
                (flycheck-mode -1))
              ;; 也可根据喜好关闭其它 minor modes
              (copilot-mode -1)
              (company-mode -1)
              ))

  ;; 便捷按键绑定（可按需调整）
  (let ((map global-map))
    ;; 打开 leetcode 列表
    (define-key map (kbd "C-c l l") #'leetcode)
    ;; 在题解 buffer 内运行/提交（也可用默认 C-c C-t / C-c C-s）
    (define-key map (kbd "C-c l t") #'leetcode-try)
    (define-key map (kbd "C-c l s") #'leetcode-submit)))

;; sourcekit-lsp support
;; (use-package lsp-sourcekit
;;     :ensure t
;;     :after lsp-mode
;;     :custom
;;     (lsp-sourcekit-executable (find-sourcekit-lsp) "Find sourcekit-lsp"))
;;; ============================================================================

;; (use-package eaf
;;   :init
;;   ;;proxy
;;   (setq eaf-proxy-type "socks5")
;;   (setq eaf-proxy-host "127.0.0.1")
;;   (setq eaf-proxy-port "20170")
  
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom

;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser))
;;   ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki


;; ;; ============================================================================
;; ;; EAF 相关 require
;; ;; ============================================================================

;; (require 'eaf-js-video-player)
;; (require 'eaf-image-viewer)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; ;;(require 'eaf-file-browser)
;; ;;(require 'eaf-file-manager)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-system-monitor)

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

;;(load-file "~/.emacs.d/lisp/jandan.el")

(provide 'packages)

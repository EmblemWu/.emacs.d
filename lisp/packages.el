;;; packages.el --- Package declarations via straight.el -*- lexical-binding: t -*-

;; All packages are managed via straight.el (straight-use-package-by-default is enabled in early-init.el).

;;;; Modern Completion and Interaction
(use-package which-key
  :init (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.4)             ; Popup keymap help after 0.4s idle
  (which-key-separator " → ")
  (which-key-prefix-prefix "+"))

(use-package vertico
  :straight (:host github :repo "minad/vertico" :tag "2.8")
  :init (vertico-mode 1)                 ; Vertical minibuffer completion UI
  :custom
  (vertico-count 12)                     ; Show 12 candidates
  (vertico-resize nil)
  (vertico-cycle t))                     ; Enable cycling through candidates

(use-package orderless
  :custom
  (completion-styles '(orderless basic)) ; Space-separated regex/fuzzy matching
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :tag "2.10")
  :after vertico
  :init (marginalia-mode 1))             ; Rich candidate metadata (docstrings, file sizes, timestamps)

(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)          ; Fast in-buffer search with live preview
         ("C-x b" . consult-buffer)      ; Modern buffer/recentf/bookmark switcher
         ("M-y" . consult-yank-pop)      ; Visual clipboard history yank
         ("C-c r" . consult-ripgrep)))   ; Project-wide ripgrep

(use-package ace-window
  :bind ("M-o" . ace-window)             ; Fast window jumping with single letter hints
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package magit
  :commands (magit-status)
  :bind ("C-c g" . magit-status))        ; Git porcelain for Emacs

;;;; General Enhancements
(use-package dired-preview
  :straight (:host github :repo "protesilaos/dired-preview")
  :hook (dired-mode . dired-preview-mode)
  :custom
  (dired-preview-delay 0.2)                  ; Delay before preview popup
  (dired-preview-max-size (* 10 1024 1024))  ; Max preview file size (10MB)
  :bind (:map dired-mode-map
              ("P" . dired-preview-mode)     ; Toggle preview mode
              ("C-c p" . dired-preview-mode)))

(use-package emacs-everywhere
  :commands (emacs-everywhere))   ; System-wide text editing via Emacs

;; macOS fix: recompile AppleScripts without resource forks to prevent osascript -1758 error
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

;;;; Native LSP Code Navigation (Eglot + Xref)
(use-package eglot
  :hook ((typescript-mode . eglot-ensure)
         (typescript-tsx-mode . eglot-ensure)
         (vue-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (dart-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)            ; Shutdown language server when last buffer is killed
  (eglot-sync-connect nil)          ; Non-blocking asynchronous server connection
  (eglot-events-buffer-size 0)      ; Disable noisy event logging for maximum performance
  (eglot-send-changes-idle-time 0.2)
  :config
  ;; Use Consult for visual xref candidate selection with live preview
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)
  ;; Disable in-buffer completion popup interference (Copilot handled separately)
  (setq eglot-stay-out-of '(company corfu)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package editorconfig
  :config (editorconfig-mode +1))

;;;; Themes
(use-package alabaster-themes
  :commands (alabaster-themes-select))

(use-package ample-theme :defer t)

(use-package auto-dark
  :custom
  (auto-dark-themes '((alabaster-themes-dark) (alabaster-themes-light)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :hook (after-init . auto-dark-mode))

;;;; Web / Frontend
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

;;;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure))

;;;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :custom
  (copilot-indent-offset-warning-disable t)
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
  (setq leetcode-prefer-language "cpp"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory (expand-file-name "~/leetcode"))
  :config
  ;; Disable heavy minor modes inside solution buffer
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
;;; packages.el ends here

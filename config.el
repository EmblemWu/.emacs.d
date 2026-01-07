;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Emblem Wu" user-mail-address "emblemwu@outlook.com")


;;ui
;;;;blur background only for emacs-macport;
(defun my/doom-dashboard-config ()
  "Configure the Doom dashboard."
  (setq fancy-splash-image (concat doom-user-dir "images/image.png"))
  (setq fancy-splash-footer-text "Emblem's Emacs")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)   ;; remove menu
  (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)      ;; remove github icon on footer
  (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-loaded)      ;; remove loaded message
  (add-hook! '+doom-dashboard-functions :append
    (insert "\n"
            (+doom-dashboard--center +doom-dashboard--width
                                     (propertize "Emblem's Emacs."
                                                 'face `(:height 190)))))
  (add-hook 'doom-dashboard-mode-hook #'my/hide-cursor-on-dashboard)
  (add-hook 'kill-buffer-hook #'my/restore-cursor-after-dashboard))


(defun my/hide-cursor-on-dashboard ()
  "Hide the cursor when in the Doom dashboard."
  (when (string-equal (buffer-name) "*doom*")
    (setq-local cursor-type nil)))

(defun my/restore-cursor-after-dashboard ()
  "Restore the cursor when leaving the Doom dashboard."
  (unless (string-equal (buffer-name) "*doom*")
    (setq-local cursor-type t)))

(my/doom-dashboard-config)

(defun my/blur-background-config()
  (set-face-background 'default "mac:windowBackgroundColor")
  (dolist (f (face-list)) (set-face-stipple f "alpha:3%"))
  (setq face-remapping-alist (append face-remapping-alist '((default my/default-blurred))))
  (defface my/default-blurred
    '((t :inherit 'default :stipple "alpha:3%"))
    "Like 'default but blurred."
    :group 'my))

(when IS-MAC (my/blur-background-config))         ;;make sure you are using emacs-macport, or it would not to work correctly

;;;;remove window tilte bar (only for emacs-macport)
(add-to-list 'default-frame-alist '(undecorated . t))


;;;;start-up screen config

;;deactivate evil on startup
(after! evil
  (evil-mode -1))


(defun latex-image-directory ()
  "Return directory name to save Latex preview images in."
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (concat
     (file-name-as-directory "./images/")
     (file-name-as-directory
      (file-name-sans-extension file-name)))))

;;EAF - Emacs Application Framework
(use-package! eaf
  ;; 设定只有手动调用以下命令后，eaf才会加载
  :commands (eaf-open eaf-open-bookmark eaf-open-browser eaf-open-browser-with-history)
  :init
  ;; 设定emacs中打开链接默认使用eaf打开
  (setq browse-url-browser-function 'eaf-open-browser)
  (defalias 'browse-web #'eaf-open-browser)

  ;; 定义了一个用于开启eaf debug模式的函数
  (defun +eaf-enable-debug ()
    (interactive)
    (setq eaf-enable-debug t))
  ;;:custom
  ;;设定eaf代理
  ;;(eaf-proxy-type "socks5")
  ;;(eaf-proxy-host "127.0.0.1")
  ;;(eaf-proxy-port "7890")
  :config
  ;; 下面的require都是引入你已经安装的eaf扩展
  (require 'eaf)
  (require 'eaf-image-viewer)
  (require 'eaf-git)
  (require 'eaf-browser)
  (require 'eaf-music-player)
  (require 'eaf-video-player)
  (require 'eaf-pdf-viewer)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  ;;  (require 'eaf-evil)
  ;; 使得在eaf buffer下能正常使用evil的keymap
  ;;  (define-key key-translation-map (kbd "SPC")
  ;;    (lambda ()
  ;;      (if (derived-mode-p 'eaf-mode)
  ;;          (pcase eaf--buffer-app-name
  ;;            ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
  ;;                           (kbd "SPC")
  ;;                         (kbd eaf-evil-leader-key)))
  ;;            ("pdf-viewer" (kbd eaf-evil-leader-key))
  ;;            ("image-viewer" (kbd eaf-evil-leader-key))
  ;;            (_  (kbd "SPC")))
  ;;        (kbd "SPC"))))
  ;; 设定eaf默认搜索引擎
  (setq eaf-browser-default-search-engine "bing")
  ;; 设定eaf开启广告屏蔽器
  (setq eaf-browser-enable-adblocker t)
  ;; 设定eaf浏览器的缩放
  (setq eaf-browser-default-zoom 1.2)
  ;; 修复鼠标乱跑的问题，让她一直放在左下角
  ;;  (setq mouse-avoidance-banish-position '((frame-or-window . frame)
  ;;                                          (side . right)
  ;;                                          (side-pos . 100)
  ;;                                          (top-or-bottom . bottom)
  ;;                                          (top-or-bottom-pos . -100)))
  ;;  (mouse-avoidance-mode 'banish)
  )

;;auto-dark-emacs
;;(use-package! auto-dark-emacs
;;  :when IS-MAC
;;  :custom
;;  (auto-dark-emacs/light-theme +list-light-themes)
;;  (auto-dark-emacs/dark-theme  +list-dark-themes)
;;  :config
;;  (add-hook! doom-load-theme :append
;;    (when (and (featurep 'solaire-mode)
;;               (not (string-prefix-p "doom-" (symbol-name doom-theme))))
;;      (set-face-background 'solaire-hl-line-face nil)
;;      (set-face-background 'solaire-default-face nil)))
;;  (add-hook! after-init #'auto-dark-emacs/check-and-set-dark-mode))


;;Apheleia (replaced with format-all)
;;(apheleia-global-mode +1)

;;lsp-tailwindcss
(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))


;; accept completion from copilot and fallback to company
;;(use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;         ("C-<tab>" . 'copilot-accept-completion-by-word)
;;         :map copilot-completion-map
;;         ("C-," . 'copilot-next-completion)
;;         ("C-." . 'copilot-previous-completion)
;;         ("<tab>" . 'copilot-accept-completion)
;;         ("TAB" . 'copilot-accept-completion)))

;;Ob-sagemath
(use-package! ob-sagemath
  :config
  ;; Ob-sagemath supports only evaluating with a session.
  (setq org-babel-default-header-args:sage '((:session . t)
                                             (:results . "drawer")))
  (setq sage-shell:input-history-cache-file  "~/sage_history")
  (add-hook! 'sage-shell-after-prompt-hook #'sage-shell-view-mode))


;;treemacs
(use-package! treemacs
  :ensure t
  :defer t
  :config
  (treemacs-resize-icons 15)
  (setq treemacs-width 30)
  (treemacs-follow-mode -1)
  (treemacs-tag-follow-mode)
  (treemacs-get-icon-value 'root-closed nil "Default")
  (setq doom-themes-treemacs-theme "Default")
  (treemacs-get-icon-value "org" t)

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("/" . treemacs-advanced-helpful-hydra))
  )

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

;;org config
;;
;;org appearance

;;org-superstar
(use-package! org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("")) ;;hide bullets
  (add-hook! 'org-mode-hook 'org-superstar-mode))

;; (use-package! helm-dash
;;   :config
;;   (setq helm-dash-docsets-path "~/.docsets")  ;; 改为你想要存放文档的目录
;;   (setq helm-dash-common-docsets '("React"))
;;   (setq helm-dash-enable-debugging 't)
;;   (map!
;;         :desc "Dash itself"   "C-c d d" #'helm-dash
;;         :desc "Dash at point" "C-c d p" #'helm-dash-at-point))

;; Key bindings
(when IS-MAC
  (global-set-key (kbd "C-c C-d") 'osx-dictionary-search-word-at-point)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'command))
;; (global-set-key (kbd "C-c i") 'osx-dictionary-search-input)

;; Work with popwin-el (https://github.com/m2ym/popwin-el)
;; (push "*osx-dictionary*" popwin:special-display-config)


;;avy
(use-package! avy
  :ensure t
  :bind
  (("C-x j" . avy-goto-char)
   ("C-x C-," . avy-move-line)
   ("C-x C-." . avy-move-region)))


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;p
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; Fira Code/Roboto Mono(the best)
(setq doom-font (font-spec :family "Roboto mono" :size 17 :weight 'Medium)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :weight 'Bold :size 19)
      doom-serif-font (font-spec :family "Fira Code" :weight 'Regular :size 21))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; `doom-dracula' for dark themen, `tsdh-light'for light theme, `doom-flatwhite'for mirage.
(setq doom-theme 'doom-tomorrow-day)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; Show images when opening a file.
(setq org-startup-with-inline-images t)
;; Show images after evaluating code blocks.
(add-hook! 'org-babel-after-execute-hook 'org-display-inline-images)
;; change latex images cache location
(defun modify-preview-dir ()
  (setq-local org-preview-latex-image-directory (latex-image-directory)))

(add-hook 'org-mode-hook 'modify-preview-dir)

;;org-download
(use-package! org-download
  :config
  (setq-default org-download-image-dir "./images")

  (add-hook! 'dired-mode-hook 'org-download-enable))

;;org-roam
;;(use-package! org-roam
;;  :ensure t
;;  :init
;;  (setq org-roam-v2-ack t)
;;  :custom
;;  (org-roam-directory "~/brain")
;;  :bind (("C-c n l" . org-roam-buffer-toggle)
;;         ("C-c n f" . org-roam-node-find)
;;         ("C-c n g" . org-roam-graph)
;;         ("C-c n i" . org-roam-node-insert))
;;  :config
;;  (org-roam-db-autosync-enable))

;;workspace
(map!
 (:when (modulep! :ui workspaces)
   :desc "New workspace"             "C-c o n"   #'+workspace/new
   :desc "Save workspace to file"    "C-c o s"   #'+workspace/save
   :desc "Delete this workspace"     "C-c o d"   #'+workspace/delete
   :desc "Load workspace from file"  "C-c o l"   #'+workspace/load)
 :desc "comment/uncomment"  "C-c /" 'comment-or-uncomment-region)

(use-package! lsp-mode
  :after-call pre-command-hook
  :init
  (map! :desc "Run code" "C-c C-r C-r" 'quickrun)
  (map! :desc "Run code in shell" "C-c C-r C-s" 'quick-run-shell))



;; (use-package! chatgpt
;;   :defer t
;;   :config
;;   (unless (boundp 'python-interpreter)
;;     (defvaralias 'python-interpreter 'python-shell-interpreter))
;;   (setq chatgpt-repo-path (expand-file-name "straight/repos/ChatGPT.el/" doom-local-dir))
;;   (set-popup-rule! (regexp-quote "*ChatGPT*")
;;     :side 'bottom :size .5 :ttl nil :quit t :modeline nil)
;;   :bind ("C-c q" . chatgpt-query))

;;(add-hook! emacs-everywhere-mode
;;  (setq emacs-everywhere-paste-p relative))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

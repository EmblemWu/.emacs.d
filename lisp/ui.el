;;; ui.el --- UI, typography, and modeline styling -*- lexical-binding: t -*-

;;;; Core UI and interaction ergonomics
(global-display-line-numbers-mode)
(global-hl-line-mode 1)          ; Highlight current line for clear orientation
(delete-selection-mode 1)        ; Replace selected region on typing
(show-paren-mode 1)              ; Instant delimiter matching
(setq show-paren-delay 0.1)
(blink-cursor-mode -1)           ; Disable cursor blink timer to save CPU cycles
(setq frame-inhibit-implied-resize t) ; Prevent window resize flickers on font load
(setq inhibit-startup-screen t)  ; Disable startup splash screen overhead
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-buffer-choice t)   ; Default to *scratch* buffer

;; Window dividers: clean 1px borders for multi-window splits
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode 1)

;;;; Typography
;; Height unit is 1/10 pt (170 = 17pt)
(set-face-attribute 'default nil :family "Roboto mono" :height 170 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "Fira Sans" :height 190 :weight 'bold)
(when (facep 'fixed-pitch-serif)
  (set-face-attribute 'fixed-pitch-serif nil :family "Fira Code" :height 210 :weight 'regular))

;; CJK font fallback across macOS, Linux, and OpenBSD
(let ((cn-font (seq-some (lambda (f)
                           (when (member f (font-family-list)) f))
                         '("PingFang SC" "Noto Sans CJK SC" "Source Han Sans SC"
                           "Hiragino Sans GB" "WenQuanYi Micro Hei" "WenQuanYi Zen Hei"))))
  (when cn-font
    (set-fontset-font t 'han (font-spec :family cn-font :size 16))
    (set-fontset-font t 'kana (font-spec :family cn-font :size 16))
    (set-fontset-font t 'cjk-misc (font-spec :family cn-font :size 16))))

;;;; Frame appearance
(setq default-frame-alist
      `((undecorated . t)
        (fullscreen . maximized)
        ,@(when sys/mac-p
            '((ns-transparent-titlebar . t)))))

;;;; Terminal palette adaptation (synchronized with auto-dark)
(require 'term)

(defvar my/term-light-palette
  (vector "#1e293b"  ; 0: charcoal black
          "#b91c1c"  ; 1: brick red
          "#15803d"  ; 2: forest green
          "#b45309"  ; 3: warm amber ochre
          "#1d4ed8"  ; 4: royal navy blue
          "#7e22ce"  ; 5: deep purple
          "#0e7490"  ; 6: deep teal cyan
          "#f8fafc") ; 7: soft light white
  "High-contrast, low-glare palette for light backgrounds.")

(defvar my/term-dark-palette
  (vector "#1a1b26"  ; 0: night black
          "#f7768e"  ; 1: soft neon red
          "#9ece6a"  ; 2: soft grass green
          "#e0af68"  ; 3: soft warm yellow
          "#7aa2f7"  ; 4: sky blue
          "#bb9af7"  ; 5: lavender
          "#7dcfff"  ; 6: clear cyan
          "#c0caf5") ; 7: silver white text
  "Soft vibrant palette for dark backgrounds.")

(defun my/apply-term-palette (&optional dark-p)
  "Dynamically remap terminal ANSI 8-color palette based on dark/light mode."
  (let* ((is-dark (if (null dark-p)
                      (or (eq (frame-parameter nil 'background-mode) 'dark)
                          (member 'alabaster-themes-dark custom-enabled-themes))
                    dark-p))
         (palette (if is-dark my/term-dark-palette my/term-light-palette))
         (faces '(term-color-black term-color-red term-color-green term-color-yellow
                  term-color-blue term-color-magenta term-color-cyan term-color-white)))
    ;; ansi-color.el uses hex color string vector
    (setq ansi-color-names-vector palette)
    ;; Preserve native 17-element ansi-term-color-vector face symbols in Emacs 29
    ;; Apply colors directly to term-color-* faces
    (dotimes (i 8)
      (let ((face (nth i faces))
            (color (aref palette i)))
        (set-face-attribute face nil :foreground color :background color)))))

;; Hook into auto-dark and theme switching
(add-hook 'auto-dark-light-mode-hook (lambda () (my/apply-term-palette nil)))
(add-hook 'auto-dark-dark-mode-hook  (lambda () (my/apply-term-palette t)))
(add-hook 'enable-theme-functions
          (lambda (theme)
            (my/apply-term-palette (string-match-p "dark" (symbol-name theme)))))

;; Initial calibration after startup
(add-hook 'after-init-hook #'my/apply-term-palette)

;;;; Modern Minimal Native Modeline (pure typography, zero emoji, fixed width)

(defun my/mode-line-modified-indicator ()
  "Fixed-width monospace status badge: [+] modified, [RO] read-only, [ ] saved."
  (cond
   (buffer-read-only
    (propertize " [RO] " 'face '(:foreground "#7e22ce" :weight bold) 'help-echo "Read-only buffer"))
   ((buffer-modified-p)
    (propertize " [+]  " 'face '(:foreground "#cf222e" :weight bold) 'help-echo "Modified buffer"))
   (t
    (propertize " [ ]  " 'face '(:foreground "#94a3b8") 'help-echo "Buffer unmodified"))))

(defun my/mode-line-vc-branch ()
  "Clean typographic Git branch badge."
  (when (and (boundp 'vc-mode) (stringp vc-mode))
    (let ((branch (if-let ((pos (string-match-p ":" vc-mode)))
                      (substring-no-properties vc-mode (1+ pos))
                    (string-trim vc-mode))))
      (propertize (format " git:%s " (string-trim branch))
                  'face '(:foreground "#0e7490" :weight medium)
                  'help-echo (format "Git branch: %s" branch)))))

;; Minimalist modeline layout
(setq-default mode-line-format
  (list
   ;; 1. Fixed-width status badge: [+] modified / [RO] read-only / [ ] saved
   '(:eval (my/mode-line-modified-indicator))
   ;; 2. Buffer name in bold
   '(:propertize "%b" face (:weight bold))
   ;; 3. Git branch (git:branch)
   '(:eval (my/mode-line-vc-branch))
   ;; 4. Line and column coordinates
   '(:propertize "   %l:%c " face (:foreground "#64748b"))
   ;; 5. Scroll percentage
   '(:propertize " %p " face (:foreground "#94a3b8"))
   ;; 6. Clean major mode indicator
   '(:propertize (" [" mode-name "]") face (:foreground "#64748b"))
   ;; 7. Trailing space
   " "))

;; Dynamic light/dark modeline faces (hairline border, flat modern aesthetic)
(defun my/update-mode-line-faces (&optional dark-p)
  "Adapt modeline faces to current light/dark theme with flat 1px hairline border."
  (let ((is-dark (if (null dark-p)
                     (or (eq (frame-parameter nil 'background-mode) 'dark)
                         (member 'alabaster-themes-dark custom-enabled-themes))
                   dark-p)))
    (if is-dark
        (progn
          (set-face-attribute 'mode-line nil
                              :background "#161b22" :foreground "#c9d1d9"
                              :box '(:line-width 1 :color "#30363d" :style nil)
                              :underline nil)
          (set-face-attribute 'mode-line-inactive nil
                              :background "#0d1117" :foreground "#6e7681"
                              :box '(:line-width 1 :color "#21262d" :style nil)
                              :underline nil))
      (progn
        (set-face-attribute 'mode-line nil
                            :background "#eaeef2" :foreground "#24292e"
                            :box '(:line-width 1 :color "#d0d7de" :style nil)
                            :underline nil)
        (set-face-attribute 'mode-line-inactive nil
                            :background "#f6f8fa" :foreground "#8c959f"
                            :box '(:line-width 1 :color "#e1e4e8" :style nil)
                            :underline nil)))))

;; Register theme hooks
(add-hook 'auto-dark-light-mode-hook (lambda () (my/update-mode-line-faces nil)))
(add-hook 'auto-dark-dark-mode-hook  (lambda () (my/update-mode-line-faces t)))
(add-hook 'enable-theme-functions
          (lambda (theme)
            (my/update-mode-line-faces (string-match-p "dark" (symbol-name theme)))))

(add-hook 'after-init-hook #'my/update-mode-line-faces)

(provide 'ui)
;;; ui.el ends here

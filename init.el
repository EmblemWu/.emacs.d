;; File: init.el

;; 加载顺序：包 → 基础配置 → 界面 → 按键（按键最后加载，可覆盖默认键位）
(require 'packages)
(require 'config)
(require 'ui)
(require 'keybinding)

;; Emacs server：供 emacsclient 复用
(require 'server)
(unless (server-running-p)
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("56faebfbf10dd28dcb403ce3e4185f999852bba96d5e61c194119b4656318991" "01f6946488b7d6f6857e58b2372527b7bd1b63910f38123e72cf00e4c9651895" "3dcc6bb29782f1c1dcb3d58e34d7527e56f4fb67bfbfd8f28b0a0037ef241c47" "1747a02911dec6d66aee4aacfd6b090b823151abcd73a5c68c93c022bf34a7ab" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

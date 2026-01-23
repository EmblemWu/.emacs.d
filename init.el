;; File: init.el

(require 'packages)
(require 'keybinding)
(require 'config)
(require 'ui)

;;server
(require 'server)
(unless (server-running-p)
  (server-start))
(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1747a02911dec6d66aee4aacfd6b090b823151abcd73a5c68c93c022bf34a7ab"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

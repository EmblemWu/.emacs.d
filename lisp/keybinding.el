;; File: keybinding.el
;; macOS 键位与剪贴板

;; select-enable-primary 是 X11 的 primary selection，macOS 无意义，已移除
(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      select-enable-clipboard t)

;; 快速上下移动 7 行
(global-set-key (kbd "M-p") (lambda () (interactive) (forward-line -7)))
(global-set-key (kbd "M-n") (lambda () (interactive) (forward-line 7)))

(provide 'keybinding)

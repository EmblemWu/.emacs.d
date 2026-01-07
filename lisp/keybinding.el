;; File: keybinding.el
;; macOS 键位与剪贴板
(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      select-enable-clipboard t
      select-enable-primary t)

(global-set-key (kbd "M-p")
  (lambda ()
    (interactive)
    (forward-line -7)))

(global-set-key (kbd "M-n")
  (lambda ()
    (interactive)
    (forward-line 7)))

(provide 'keybinding)

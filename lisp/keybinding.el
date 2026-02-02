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

;; multiple cursors
(global-set-key (kbd "C-x y") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-h d") 'devdocs-lookup)

(provide 'keybinding)

;; File: keybinding.el
;; macOS 键位、剪贴板与常用人体工学快捷键

;; macOS 键位映射（Linux 与 OpenBSD 下原生使用 Alt 键作为 Meta，无需额外设置）
(when-mac
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))

(setq select-enable-clipboard t)

;;;; 1. 光标快速纵向穿梭（保留 7 行快速移动）
(global-set-key (kbd "M-p") (lambda () (interactive) (forward-line -7)))
(global-set-key (kbd "M-n") (lambda () (interactive) (forward-line 7)))

;;;; 2. 现代窗口与文件流转（人性化快捷通道）
;; M-o 已在 packages.el 绑定 ace-window，此处补充助记快捷键 C-c w
(global-set-key (kbd "C-c w") #'ace-window)
;; 快速访问最近打开过的文件（跨会话沉淀）
(global-set-key (kbd "C-c f") #'recentf-open-files)

;;;; 3. 缩放字体大小（对齐现代应用快捷习惯：C-+ / C-- / C-0）
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))

;;;; 4. Org-mode 思考与行动中枢（随时随地瞬时捕获）
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c o t") (lambda () (interactive) (find-file "~/org/tasks.org")))
(global-set-key (kbd "C-c o i") (lambda () (interactive) (find-file "~/org/inbox.org")))
(global-set-key (kbd "C-c o n") (lambda () (interactive) (find-file "~/org/notes.org")))

;;;; 5. 悬浮式极速终端抽屉（单键唤起/收缩原生 Shell）
(global-set-key (kbd "C-c t") #'my/toggle-terminal)
(global-set-key (kbd "C-`") #'my/toggle-terminal)

;;;; 6. 多任务工作区与项目流转（Tab-bar + Project.el）
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p d") #'project-dired)
(global-set-key (kbd "C-c p t") #'tab-bar-new-tab)
(global-set-key (kbd "C-c p w") #'tab-bar-close-tab)
(global-set-key (kbd "C-c p r") #'tab-bar-rename-tab)

(provide 'keybinding)

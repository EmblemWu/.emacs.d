;;; keybinding.el --- Keybindings and ergonomics -*- lexical-binding: t -*-

;; macOS modifier keys (Linux and OpenBSD naturally use Alt as Meta and Super as Super)
(when-mac
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))

(setq select-enable-clipboard t)

;;;; 1. Rapid vertical navigation (7 lines jump)
(global-set-key (kbd "M-p") (lambda () (interactive) (forward-line -7)))
(global-set-key (kbd "M-n") (lambda () (interactive) (forward-line 7)))

;;;; 2. Window and file management
;; M-o bound to ace-window in packages.el; mnemonic C-c w added here
(global-set-key (kbd "C-c w") #'ace-window)
;; Fast access to recent files
(global-set-key (kbd "C-c f") #'recentf-open-files)

;;;; 3. Text scale adjustment (standard shortcuts: C-+ / C-- / C-0)
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))

;;;; 4. Org-mode capture and agenda
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c o t") (lambda () (interactive) (find-file "~/org/tasks.org")))
(global-set-key (kbd "C-c o i") (lambda () (interactive) (find-file "~/org/inbox.org")))
(global-set-key (kbd "C-c o n") (lambda () (interactive) (find-file "~/org/notes.org")))
(global-set-key (kbd "C-c o b") (lambda () (interactive) (find-file "~/org/buy.org")))

;;;; 5. Floating drawer terminal
(global-set-key (kbd "C-c t") #'my/toggle-terminal)
(global-set-key (kbd "C-`") #'my/toggle-terminal)

;;;; 6. Workspaces and project management (Tab-bar + Project.el)
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p d") #'project-dired)
(global-set-key (kbd "C-c p t") #'tab-bar-new-tab)
(global-set-key (kbd "C-c p w") #'tab-bar-close-tab)
(global-set-key (kbd "C-c p r") #'tab-bar-rename-tab)

;;;; 7. Code navigation and cross-references (Eglot / Xref)
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-go-back)
(global-set-key (kbd "M-?") #'xref-find-references)

(provide 'keybinding)
;;; keybinding.el ends here

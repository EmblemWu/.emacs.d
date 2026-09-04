;;; config.el --- Core editing behavior and system configurations -*- lexical-binding: t -*-

;;;; 1. Interaction and confirmation tuning
(setq use-short-answers t)               ; Emacs 28+: short confirmation (y/n instead of yes/no)
(defalias 'yes-or-no-p 'y-or-n-p)       ; Backward compatibility
(setq ring-bell-function 'ignore)        ; Disable audible bell
(setq confirm-kill-emacs nil)            ; Exit without confirmation prompt

;;;; 2. File and directory hygiene (prevent cluttering project directories)
(let ((backup-dir (expand-file-name ".cache/backups/" user-emacs-directory))
      (autosave-dir (expand-file-name ".cache/auto-save/" user-emacs-directory)))
  (unless (file-directory-p backup-dir) (make-directory backup-dir t))
  (unless (file-directory-p autosave-dir) (make-directory autosave-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,autosave-dir t))
        backup-by-copying t              ; Copy files to create backups to preserve symlinks
        delete-old-versions t            ; Silently remove outdated backups
        kept-new-versions 5              ; Keep 5 newest versions
        kept-old-versions 2              ; Keep 2 oldest versions
        version-control t))              ; Enable version numbered backups

;;;; 3. Smooth scrolling and rendering performance
(setq scroll-conservatively 101          ; Line-by-line smooth scrolling
      scroll-margin 2                    ; Keep 2 lines margin at viewport edges
      scroll-preserve-screen-position t  ; Preserve cursor screen position on paging
      fast-but-imprecise-scrolling t)    ; Improve scrolling performance on large files

;; Line number rendering optimization (avoid recalculating margin width on scroll)
(setq-default display-line-numbers-grow-only t
              display-line-numbers-width-start t)

;; Syntax highlighting deferred micro-scheduling
(setq jit-lock-defer-time 0.05
      jit-lock-stealth-time 1.0)

;; Minibuffer zero-latency GC: suspend garbage collection during minibuffer input
(defun my-minibuffer-setup-gc ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-gc ()
  (setq gc-cons-threshold (* 128 1024 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-gc)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-gc)

;;;; 4. Automation and cross-session persistence
(electric-pair-mode 1)                   ; Auto-pair delimiters: () [] {} ""
(save-place-mode 1)                      ; Restore cursor position on file open
(recentf-mode 1)                         ; Track recently opened files
(setq recentf-max-saved-items 200
      recentf-exclude '("/tmp/" "/ssh:" "\\.cache" "\\.git/"))
(savehist-mode 1)                        ; Persist minibuffer history, search queries, and commands
(global-auto-revert-mode 1)              ; Automatically revert buffers when modified externally
(setq auto-revert-verbose nil)           ; Silent auto-revert without echo noise

;;;; 5. Dired enhancements
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t                ; Suggest other split window as target directory
      dired-recursive-copies 'always     ; Copy directories recursively without prompting
      dired-recursive-deletes 'top)      ; Delete directories recursively with a single confirmation

;;;; 6. LaTeX compilation
(defun my-xelatex-quiet ()
  "Save and quietly compile current TeX file with xelatex, minimal output."
  (interactive)
  (unless (and buffer-file-name (string-match-p "\\.tex\\'" buffer-file-name))
    (user-error "Current buffer is not a .tex file"))
  (save-buffer)
  (let* ((cmd (format "xelatex -interaction=nonstopmode -halt-on-error -file-line-error -synctex=1 %s"
                      (shell-quote-argument buffer-file-name))))
    (compile cmd)))

(defun my-latex-auto-compile-on-save ()
  "On save, quietly compile current TeX file with xelatex."
  (when (and buffer-file-name (string-match-p "\\.tex\\'" buffer-file-name))
    (my-xelatex-quiet)))

(add-hook 'latex-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") #'my-xelatex-quiet)
            (add-hook 'after-save-hook #'my-latex-auto-compile-on-save 0 t)))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 12)))

(add-hook 'compilation-finish-functions
          (lambda (buf msg)
            (when (string-match "exited abnormally" msg)
              (display-buffer buf))))

;;;; 7. Org-mode capture and workflow integration
(setq org-directory (expand-file-name "~/org")
      org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                             (expand-file-name "tasks.org" org-directory)
                             (expand-file-name "buy.org" org-directory))
      org-todo-keywords '((sequence "TODO(t)" "DOING(i!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-log-done 'time
      org-log-into-drawer t
      org-startup-indented t
      org-hide-emphasis-markers t
      org-ellipsis " ▾"
      org-return-follows-link t)

;; Org Capture templates
(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "~/org/tasks.org" "Tasks")
         "* TODO %?\n  SCHEDULED: %t\n  Captured: %U\n  Source: %a\n  %i"
         :empty-lines 1)
        ("i" "Quick Inbox" entry
         (file+headline "~/org/inbox.org" "Quick Notes")
         "* %?\n  Captured: %U\n  %i"
         :empty-lines 1)
        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Notes")
         "* %?\n  Captured: %U\n  Source: %a\n\n  %i"
         :empty-lines 1)
        ("b" "Buy" entry
         (file+headline "~/org/buy.org" "Wishlist")
         "* TODO %?\n  Captured: %U\n  Price: \n  Link: %a\n  %i"
         :empty-lines 1)))

;;;; 8. Floating drawer terminal
(require 'term)
(defvar my/terminal-buffer-name "*terminal-drawer*")

(defun my/toggle-terminal ()
  "Toggle a dedicated bottom-drawer terminal window."
  (interactive)
  (let ((term-win (get-buffer-window my/terminal-buffer-name)))
    (if term-win
        (if (eq (selected-window) term-win)
            (delete-window term-win)
          (select-window term-win))
      (let ((buf (get-buffer my/terminal-buffer-name)))
        (unless (and buf (buffer-live-p buf) (get-buffer-process buf))
          (let ((shell-prog (or (getenv "SHELL")
                                (executable-find "zsh")
                                (executable-find "bash")
                                "/bin/sh")))
            (setq buf (make-term "terminal-drawer" shell-prog)))
          (with-current-buffer buf
            (term-char-mode)))
        (let ((win (display-buffer-in-side-window
                    buf
                    '((side . bottom)
                      (window-height . 0.35)
                      (slot . 0)))))
          (select-window win))))))

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-c t") #'my/toggle-terminal)
  (define-key term-mode-map (kbd "C-c t") #'my/toggle-terminal))

;;;; 9. Workspaces and project isolation (Tab-bar + Project.el)
(setq tab-bar-show 1                     ; Show tab-bar only when multiple tabs exist
      tab-bar-close-button-show nil      ; Hide tab close buttons
      tab-bar-new-button-show nil        ; Hide new tab button
      tab-bar-tab-hints t                ; Show tab number indicators
      tab-bar-select-tab-modifiers '(meta) ; Switch tabs directly via M-1, M-2, etc.
      project-vc-extra-root-markers '(".git" "package.json" "Cargo.toml"))

(tab-bar-mode 1)

(provide 'config)
;;; config.el ends here

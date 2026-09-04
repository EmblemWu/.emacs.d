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
(setq tab-bar-show nil                   ; Hide top tab-bar completely for zero visual distraction
      tab-bar-close-button-show nil      ; Hide tab close buttons
      tab-bar-new-button-show nil        ; Hide new tab button
      tab-bar-tab-hints t                ; Show tab number indicators
      tab-bar-select-tab-modifiers '(meta) ; Switch tabs directly via M-1, M-2, etc.
      tab-bar-separator " "              ; Clean subtle space between tabs
      project-vc-extra-root-markers '(".git" "package.json" "Cargo.toml"))

(tab-bar-mode 1)

;; Transient workspace switch notification in echo area
(add-hook 'tab-bar-tab-post-select-functions
          (lambda (_prev current)
            (let ((name (alist-get 'name current)))
              (message "Workspace: %s" name))))

;;;; 10. Instant GitHub repository explorer (Shallow clone to /tmp with full LSP)
(defvar my/github-cache-dir
  (expand-file-name "gh-repos" temporary-file-directory)
  "Temporary cache directory for shallow-cloned GitHub repositories.")

(defun my/github--open-cached-repo (target-dir)
  "Open cached repository in a dedicated tab with project integration."
  (let ((default-directory (file-name-as-directory target-dir)))
    (when (fboundp 'tab-bar-new-tab)
      (tab-bar-new-tab)
      (tab-bar-rename-tab (file-name-nondirectory (directory-file-name target-dir))))
    ;; Trigger asynchronous background indexing for Rust projects if Cargo.toml exists
    (when (and (file-exists-p (expand-file-name "Cargo.toml" target-dir))
               (executable-find "cargo"))
      (message "Background indexing Rust dependencies for rust-analyzer...")
      (make-process :name "cargo-check-init"
                    :buffer "*cargo-check-init*"
                    :command '("cargo" "check" "--quiet")))
    (project-find-file)))

(defun my/github-open-repo (repo-input)
  "Prompt for a GitHub repository (owner/repo or URL), shallow-clone asynchronously, and open via project."
  (interactive
   (list (read-string "GitHub repository (owner/repo or URL): ")))
  (let* ((clean-input (string-trim repo-input))
         (repo-slug
          (cond
           ((string-match "github\\.com[:/]\\([^/]+/[^/.]+?\\)\\(?:\\.git\\)?$" clean-input)
            (match-string 1 clean-input))
           ((string-match "^\\([^/]+/[^/.]+\\)$" clean-input)
            (match-string 1 clean-input))
           (t (user-error "Invalid repository format. Please use 'owner/repo' or a GitHub URL"))))
         (target-dir (expand-file-name repo-slug my/github-cache-dir))
         (clone-url (format "https://github.com/%s.git" repo-slug)))
    (if (file-directory-p target-dir)
        ;; Already cached: open immediately without network overhead
        (my/github--open-cached-repo target-dir)
      ;; Clone asynchronously to ensure Emacs UI remains completely unblocked
      (make-directory (file-name-directory target-dir) t)
      (let ((clone-buf (get-buffer-create (format "*clone: %s*" repo-slug))))
        (message "Cloning %s in background (shallow)..." repo-slug)
        (make-process
         :name (format "git-clone-%s" repo-slug)
         :buffer clone-buf
         :command (list "git" "clone" "--depth=1" "--single-branch" clone-url target-dir)
         :sentinel
         (lambda (proc event)
           (cond
            ((string-match-p "finished" event)
             (message "Successfully cloned %s! Opening project workspace..." repo-slug)
             (my/github--open-cached-repo target-dir))
            ((string-match-p "\\(exited\\|failed\\)" event)
             (message "Failed to clone %s. Check buffer %s"
                      repo-slug (buffer-name (process-buffer proc)))))))))))

;;;; 10. Virtual GitHub Explorer (Dired-like interactive remote browser, 0-clone VFS)
(defvar-local my/gh-dired-repo nil "Current GitHub repository slug (owner/repo).")
(defvar-local my/gh-dired-path "" "Current directory path within the repository.")
(defvar-local my/gh-file-repo nil "Repository slug for streamed file buffer.")
(defvar-local my/gh-file-dir nil "Parent directory path for streamed file buffer.")

(defvar my/gh-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my/gh-dired-open)
    (define-key map (kbd "f")   #'my/gh-dired-open)
    (define-key map (kbd "e")   #'my/gh-dired-open)
    (define-key map (kbd "^")   #'my/gh-dired-up)
    (define-key map (kbd "C-x C-j") #'my/gh-dired-up)
    (define-key map (kbd "g")   #'my/gh-dired-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    map)
  "Keymap for `my/gh-dired-mode`.")

(define-derived-mode my/gh-dired-mode special-mode "GH-Dired"
  "Major mode for browsing remote GitHub repositories interactively like Dired."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t))

(defun my/gh-dired-format-size (size)
  "Format byte SIZE into human-readable string."
  (cond
   ((< size 1024) (format "%4dB" size))
   ((< size (* 1024 1024)) (format "%5.1fK" (/ (float size) 1024)))
   (t (format "%5.1fM" (/ (float size) (* 1024 1024))))))

(defun my/gh-dired-render (repo path)
  "Fetch directory contents via GitHub API and render in a Dired-style buffer."
  (let* ((clean-path (string-trim (or path "") "/"))
         (buf-name (if (string-empty-p clean-path)
                       (format "*gh: %s*" repo)
                     (format "*gh: %s/%s*" repo clean-path)))
         (buf (get-buffer-create buf-name))
         (api-cmd (if (string-empty-p clean-path)
                      (format "gh api repos/%s/contents" repo)
                    (format "gh api repos/%s/contents/%s" repo clean-path))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "  GitHub Remote: https://github.com/%s\n" repo))
        (insert (format "  Directory: /%s\n\n" clean-path))
        ;; Parent directory navigation entry if not at root
        (unless (string-empty-p clean-path)
          (let ((start (point)))
            (insert "  [dir]        ..  (Parent Directory)\n")
            (put-text-property start (point) 'gh-type 'up)))
        (message "Fetching GitHub directory %s/%s..." repo clean-path)
        (condition-case err
            (let* ((json-str (shell-command-to-string api-cmd))
                   (items (json-parse-string json-str :array-type 'list :object-type 'alist))
                   (dirs (seq-filter (lambda (x) (equal (alist-get 'type x) "dir")) items))
                   (files (seq-filter (lambda (x) (equal (alist-get 'type x) "file")) items)))
              ;; Insert directories
              (dolist (d (sort dirs (lambda (a b) (string< (alist-get 'name a) (alist-get 'name b)))))
                (let* ((name (alist-get 'name d))
                       (subpath (alist-get 'path d))
                       (start (point)))
                  (insert (format "  [dir]       %s/\n" name))
                  (put-text-property start (point) 'gh-type 'dir)
                  (put-text-property start (point) 'gh-path subpath)
                  (put-text-property start (point) 'gh-name name)))
              ;; Insert files
              (dolist (f (sort files (lambda (a b) (string< (alist-get 'name a) (alist-get 'name b)))))
                (let* ((name (alist-get 'name f))
                       (subpath (alist-get 'path f))
                       (size (or (alist-get 'size f) 0))
                       (start (point)))
                  (insert (format "  [file] %6s  %s\n" (my/gh-dired-format-size size) name))
                  (put-text-property start (point) 'gh-type 'file)
                  (put-text-property start (point) 'gh-path subpath)
                  (put-text-property start (point) 'gh-name name))))
          (error
           (insert (format "\n  Error fetching directory: %s\n" (error-message-string err))))))
      (my/gh-dired-mode)
      (setq-local my/gh-dired-repo repo)
      (setq-local my/gh-dired-path clean-path)
      (goto-char (point-min))
      (forward-line 3))
    (switch-to-buffer buf)))

(defun my/gh-dired-open ()
  "Open file or enter directory at point in GitHub Dired."
  (interactive)
  (let ((type (get-text-property (point) 'gh-type))
        (path (get-text-property (point) 'gh-path))
        (repo my/gh-dired-repo))
    (cond
     ((eq type 'up)
      (my/gh-dired-up))
     ((eq type 'dir)
      (my/gh-dired-render repo path))
     ((eq type 'file)
      (my/gh-dired-stream-file repo path my/gh-dired-path))
     (t (message "No GitHub file or directory at point.")))))

(defun my/gh-dired-up ()
  "Navigate up to parent directory in GitHub Dired."
  (interactive)
  (if (or (null my/gh-dired-path) (string-empty-p my/gh-dired-path))
      (message "Already at repository root.")
    (let* ((parent-path (file-name-directory (directory-file-name my/gh-dired-path)))
           (clean-parent (if parent-path (directory-file-name parent-path) "")))
      (my/gh-dired-render my/gh-dired-repo clean-parent))))

(defun my/gh-dired-refresh ()
  "Refresh current GitHub Dired buffer."
  (interactive)
  (my/gh-dired-render my/gh-dired-repo my/gh-dired-path))

(defun my/gh-dired-stream-file (repo file-path parent-dir)
  "Stream single file from GitHub into buffer with syntax highlighting and dired-jump support."
  (let* ((buf-name (format "*gh: %s/%s*" repo file-path))
         (buf (get-buffer-create buf-name))
         (raw-url (format "https://raw.githubusercontent.com/%s/HEAD/%s" repo file-path)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (message "Streaming %s/%s from GitHub..." repo file-path)
        (let ((exit-code (call-process "curl" nil t nil "-sL" raw-url)))
          (if (and (zerop exit-code) (> (buffer-size) 0))
              (progn
                (setq buffer-file-name file-path)
                (set-auto-mode)
                (setq buffer-file-name nil)
                (setq-local my/gh-file-repo repo)
                (setq-local my/gh-file-dir parent-dir)
                (local-set-key (kbd "C-x C-j") #'my/gh-file-jump-to-dired)
                (read-only-mode 1)
                (switch-to-buffer (current-buffer))
                (message "Streamed %s/%s on-demand. Press C-x C-j to return to directory." repo file-path))
            (kill-buffer (current-buffer))
            (user-error "Failed to fetch %s/%s" repo file-path)))))))

(defun my/gh-file-jump-to-dired ()
  "Jump back from streamed file to its parent GitHub Dired buffer."
  (interactive)
  (if (and my/gh-file-repo (boundp 'my/gh-file-dir))
      (my/gh-dired-render my/gh-file-repo my/gh-file-dir)
    (call-interactively #'dired-jump)))

(defun my/github-browse-repo (repo-input)
  "Browse remote GitHub repository interactively like Dired without cloning (0 bytes download)."
  (interactive
   (list (read-string "GitHub repository to browse (owner/repo or URL): ")))
  (let* ((clean-input (string-trim repo-input))
         (repo-slug
          (cond
           ((string-match "github\\.com[:/]\\([^/]+/[^/.]+?\\)\\(?:\\.git\\)?$" clean-input)
            (match-string 1 clean-input))
           ((string-match "^\\([^/]+/[^/.]+\\)$" clean-input)
            (match-string 1 clean-input))
           (t (user-error "Invalid repository format. Use 'owner/repo'")))))
    (my/gh-dired-render repo-slug "")))

;;;; 11. Universal terminal and remote SSH adaptation (macOS, Linux, OpenBSD)
(unless (display-graphic-p)
  ;; Disable textual menu bar row in terminal
  (menu-bar-mode -1)

  ;; Enable terminal mouse support (clicking, scrolling, window resizing over SSH)
  (xterm-mouse-mode 1)

  ;; Eliminate escape sequence latency in TTY (fast ESC response)
  (setq tty-escape-delay 0.05)
  (setq-default echo-keystrokes 0.1)

  ;; Universal OSC 52 clipboard: pipes text through SSH stdout back to client machine
  ;; Compatible with iTerm2, Ghostty, WezTerm, Alacritty, Kitty, Blink Shell, tmux
  (defun my/osc52-copy (text &optional _push)
    "Send TEXT to the client system clipboard via terminal OSC 52 escape sequence."
    (when (and text (stringp text))
      (let* ((b64 (base64-encode-string (encode-coding-string text 'utf-8) t))
             (inside-tmux (or (getenv "TMUX") (string-prefix-p "screen" (or (getenv "TERM") ""))))
             (osc52-seq
              (if inside-tmux
                  (format "\ePtmux;\e\e]52;c;%s\a\e\\" b64)
                (format "\e]52;c;%s\a" b64))))
        (send-string-to-terminal osc52-seq))))

  ;; Local clipboard paste fallback across OS platforms
  (defun my/terminal-paste ()
    "Retrieve clipboard content using local platform utilities when available."
    (cond
     ((and sys/mac-p (executable-find "pbpaste"))
      (shell-command-to-string "pbpaste"))
     ((and (executable-find "wl-paste") (getenv "WAYLAND_DISPLAY"))
      (shell-command-to-string "wl-paste --no-newline"))
     ((executable-find "xclip")
      (shell-command-to-string "xclip -selection clipboard -o"))
     ((executable-find "xsel")
      (shell-command-to-string "xsel --clipboard --output"))
     (t nil)))

  (setq interprogram-cut-function
        (lambda (text &optional push)
          ;; Primary: Send OSC 52 for remote SSH and modern terminals
          (my/osc52-copy text push)
          ;; Secondary: Local clipboard fallback
          (cond
           ((and sys/mac-p (executable-find "pbcopy"))
            (let ((process-connection-type nil))
              (let ((proc (start-process "pbcopy" nil "pbcopy")))
                (process-send-string proc text)
                (process-send-eof proc))))
           ((and (executable-find "wl-copy") (getenv "WAYLAND_DISPLAY"))
            (let ((process-connection-type nil))
              (let ((proc (start-process "wl-copy" nil "wl-copy")))
                (process-send-string proc text)
                (process-send-eof proc))))
           ((executable-find "xclip")
            (let ((process-connection-type nil))
              (let ((proc (start-process "xclip" nil "xclip" "-selection" "clipboard")))
                (process-send-string proc text)
                (process-send-eof proc)))))))

  (setq interprogram-paste-function #'my/terminal-paste)

  ;; Universal TTY / SSH escape key sequence decoding (VT100 / Xterm / Linux / OpenBSD)
  (let ((map (if (boundp 'input-decode-map) input-decode-map local-function-key-map)))
    ;; Ctrl + Arrow keys
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    ;; Alt / Meta + Arrow keys
    (define-key map "\e[1;3A" [M-up])
    (define-key map "\e[1;3B" [M-down])
    (define-key map "\e[1;3C" [M-right])
    (define-key map "\e[1;3D" [M-left])
    ;; Shift + Arrow keys
    (define-key map "\e[1;2A" [S-up])
    (define-key map "\e[1;2B" [S-down])
    (define-key map "\e[1;2C" [S-right])
    (define-key map "\e[1;2D" [S-left])
    ;; Home, End, PageUp, PageDown, Delete
    (define-key map "\e[1~" [home])
    (define-key map "\e[4~" [end])
    (define-key map "\e[H"  [home])
    (define-key map "\e[F"  [end])
    (define-key map "\e[5~" [prior])
    (define-key map "\e[6~" [next])
    (define-key map "\e[3~" [deletechar])
    ;; macOS Option dead character fallbacks in TTY
    (when-mac
      (dolist (mapping '(("≈" . [?\e ?x])
                         ("ƒ" . [?\e ?f])
                         ("∫" . [?\e ?b])
                         ("π" . [?\e ?p])
                         ("˜" . [?\e ?n])
                         ("√" . [?\e ?v])
                         ("∑" . [?\e ?w])
                         ("∂" . [?\e ?d])
                         ("≤" . [?\e ?<])
                         ("≥" . [?\e ?>])))
        (define-key map (car mapping) (cdr mapping))))))

(provide 'config)
;;; config.el ends here

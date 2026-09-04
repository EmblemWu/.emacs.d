;;; early-init.el --- Early Initialization Configuration -*- lexical-binding: t -*-

;;;; 0. Cross-platform OS predicates and macros (macOS / Linux / OpenBSD)
(defconst sys/mac-p (eq system-type 'darwin))
(defconst sys/linux-p (eq system-type 'gnu/linux))
(defconst sys/bsd-p (memq system-type '(berkeley-unix openbsd freebsd netbsd)))

(defmacro when-mac (&rest body)
  "Execute BODY only on macOS (darwin)."
  (declare (indent 0))
  `(when sys/mac-p ,@body))

(defmacro when-linux (&rest body)
  "Execute BODY only on GNU/Linux."
  (declare (indent 0))
  `(when sys/linux-p ,@body))

(defmacro when-bsd (&rest body)
  "Execute BODY only on BSD systems (OpenBSD, FreeBSD)."
  (declare (indent 0))
  `(when sys/bsd-p ,@body))

;; Universal PATH resolution: discover user bin, local bin, and Homebrew
;; Note: Processed from lowest to highest priority so /opt/homebrew/bin ends up first on Apple Silicon
(let ((path-dirs (if sys/mac-p
                     (list (expand-file-name "~/.local/bin")
                           "/usr/local/bin"
                           "/opt/homebrew/bin")
                   (list (expand-file-name "~/.local/bin")
                         "/usr/local/bin"))))
  (dolist (dir path-dirs)
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir path-separator (getenv "PATH"))))))

;;;; 1. Startup acceleration (suppress redundant I/O and handler lookups)
;; Temporarily clear file-name-handler-alist during startup to skip regex checks on require
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Add custom module path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Garbage collection tuning: maximize during startup, reset to 128MB post-startup
(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))
            ;; Perform GC only after 5 seconds of idle time
            (run-with-idle-timer 5 t #'garbage-collect)))

;; Increase IPC sub-process pipe output buffer for LSP and Copilot (3MB)
(setq read-process-output-max (* 3 1024 1024))

;;;; 2. Rendering and typography smoothness
(when-mac
  (setq inhibit-compacting-font-caches t))    ; Prevent macOS CoreText font cache GC jitter
(setq-default bidi-paragraph-direction 'left-to-right) ; Enforce LTR to bypass bidirectional text scan
(setq-default bidi-inhibit-bpa t)            ; Disable bidirectional bracket algorithm for speed
(setq redisplay-dont-pause t)                ; Don't pause redisplay, ensures smooth 120Hz scrolling
(setq native-comp-async-report-warnings-errors 'silent) ; Silence async native compilation warnings

;;;; 3. Straight.el optimization
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Compatibility shims for Emacs 29/30 APIs
(require 'cl-lib)
(unless (fboundp 'incf)
  (defalias 'incf 'cl-incf))
(unless (fboundp 'decf)
  (defalias 'decf 'cl-decf))
(unless (fboundp 'set-local)
  (defsubst set-local (var val)
    "Set the local binding of VAR to VAL (compatibility for Emacs < 30)."
    (set (make-local-variable var) val)))

;; Compatibility advice for seconds-to-string argument variance across versions
(with-eval-after-load 'time-date
  (advice-add 'seconds-to-string :around
              (lambda (orig-fun delay &rest args)
                (condition-case nil
                    (apply orig-fun delay args)
                  (wrong-number-of-arguments
                   (funcall orig-fun delay))))))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate use-package with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(require 'use-package)

;;; early-init.el ends here

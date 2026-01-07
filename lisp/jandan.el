;;; jandan.el --- Browse Jandan.net content from within Emacs -*- lexical-binding: t; -*-
;;
;; Author: ChatGPT
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://jandan.net
;; Keywords: convenience, multimedia
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This package provides simple commands to browse various sections of the
;; Chinese humor website Jandan (煎蛋网) directly within Emacs.  The
;; website underwent a major overhaul in late 2025 and many of the old
;; public APIs stopped working.  The functions in this package use the
;; new JSON-based API endpoints discovered by reverse‑engineering the
;; site.  Each interactive command fetches a page of posts from a
;; specific section—such as 问答 (Q&A), 树洞 (tree hole), 随手拍 (snapshots),
;; 无聊图 (random images), 鱼塘 (forum), 热榜 (hot topics) and 段子 (jokes)—and
;; displays them in a dedicated buffer.
;;
;; Only anonymous content browsing is supported.  Posting new content,
;; voting, replying and other authenticated actions are not implemented.
;; Some sections, notably 段子 (jokes), require authentication; in such
;; cases a message is displayed instead of content.

;;; Code:

(require 'url)
(require 'json)
(require 'subr-x)
(require 'cl-lib)

;; Helpers for accessing JSON alists with string keys.  Jandan's API
;; returns JSON objects with string keys.  Emacs' built‑in
;; `alist-get' uses `eq' by default, which will not match strings
;; reliably because two distinct string objects with the same
;; contents are not necessarily `eq'.  Therefore we provide a
;; wrapper that always uses `equal' for comparisons.
(defun jandan--aget (key alist &optional default)
  "Fetch KEY from ALIST using `equal' comparison, returning DEFAULT if not found."
  (alist-get key alist default nil #'equal))

;; String decoding helper
;;
;; Some API responses return unibyte strings containing UTF‑8 encoded bytes.
;; When such strings are inserted into a multibyte buffer, Emacs will
;; interpret each byte as a separate Latin-1 character (resulting in
;; mojibake like "ã").  This helper detects unibyte strings and
;; decodes them as UTF‑8.  Multibyte strings are returned unchanged.
;;
(defun jandan--decode-string (str)
  "Return STR decoded as UTF-8.  This helper treats the bytes of STR
as UTF-8 regardless of whether STR is currently multibyte or
unibyte.  It first converts STR into a unibyte representation
via `string-make-unibyte` and then decodes it as UTF-8.  If STR
is not a string, it is returned unchanged."
  (if (stringp str)
      (decode-coding-string (string-make-unibyte str) 'utf-8)
    str))

;;
;; Viewing mode and pagination support
;;

(defvar jandan-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map view-mode-map)
    (define-key map (kbd "n") #'jandan-next-page)
    (define-key map (kbd "p") #'jandan-prev-page)
    map)
  "Keymap used in `jandan-view-mode'.  Inherits from `view-mode-map'.
Press `n' for next page and `p' for previous page.")

;;;###autoload
(define-derived-mode jandan-view-mode view-mode "Jandan-View"
  "Major mode for viewing Jandan posts.
\{jandan-view-mode-map}"
  :group 'jandan)

;;;###autoload
(defun jandan-next-page ()
  "Load the next page of the current Jandan section, if available.
When invoked in a Jandan buffer, this command increments the
page number and reloads the content.  For comment-based sections
(问答、树洞、随手拍、无聊图)，it uses `jandan--comment-section'.  For
forum sections, it calls `jandan--forum-section'."
  (interactive)
  (unless (and (boundp 'jandan-section-id) jandan-section-id)
    (user-error "Not in a Jandan buffer"))
  (let ((new-page (1+ (or jandan-current-page 0))))
    (cond
     ((eq jandan-section-type 'comment)
      (jandan--comment-section jandan-section-id jandan-section-name new-page))
     ((eq jandan-section-type 'forum)
      (jandan--forum-section jandan-section-id jandan-section-name new-page))
     (t (user-error "Unknown Jandan section type")))))

;;;###autoload
(defun jandan-prev-page ()
  "Load the previous page of the current Jandan section, if available.
Decrements the page number but never below zero."
  (interactive)
  (unless (and (boundp 'jandan-section-id) jandan-section-id)
    (user-error "Not in a Jandan buffer"))
  (let ((new-page (max 0 (1- (or jandan-current-page 0)))))
    (cond
     ((eq jandan-section-type 'comment)
      (jandan--comment-section jandan-section-id jandan-section-name new-page))
     ((eq jandan-section-type 'forum)
      (jandan--forum-section jandan-section-id jandan-section-name new-page))
     (t (user-error "Unknown Jandan section type")))))

;; Customization group
(defgroup jandan nil
  "Browse content from jandan.net."
  :group 'applications)

(defcustom jandan-api-base "https://jandan.net/api"
  "Base URL for Jandan API endpoints."
  :type 'string
  :group 'jandan)

(defcustom jandan-page-size 20
  "Number of items to display per page.  The API returns a fixed number
of items per page (usually 20).  This variable controls how many
entries are inserted into the buffer before truncating."
  :type 'integer
  :group 'jandan)

;;; Helper functions

(defun jandan--fetch-json (url)
  "Synchronously fetch JSON data from URL and return the decoded object.
If the HTTP status is not 200, return nil and log a message.  The
function leverages `url-retrieve-synchronously' which blocks until
the entire response has been downloaded."
  (let ((buffer (url-retrieve-synchronously url t t 30)))
    (if (not buffer)
        (progn
          (message "Failed to retrieve data from %s" url)
          nil)
      (with-current-buffer buffer
        (unwind-protect
            (progn
              ;; Move point to beginning of response body
              (goto-char (point-min))
              (re-search-forward "\n\n" nil 'move)
              (let ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-key-type 'string))
                (json-read))
              )
          (kill-buffer buffer))))))

(defun jandan--strip-html (string)
  "Strip basic HTML tags from STRING and return plain text.
This helper is not a full HTML renderer—it removes common
entities and replaces <img> tags with a placeholder showing the
image URL."
  (when string
    (let ((text string))
      ;; replace <br> tags with newlines
      (setq text (replace-regexp-in-string "<br\s*/?>" "\n" text t t))
      ;; replace &amp; etc.
      (setq text (replace-regexp-in-string "&lt;" "<" text t t))
      (setq text (replace-regexp-in-string "&gt;" ">" text t t))
      (setq text (replace-regexp-in-string "&amp;" "&" text t t))
      ;; convert <img src="URL" ...> to [img: URL]
      ;; Use a loop with `string-match' and `replace-match' to reliably
      ;; capture the src attribute.  This avoids issues with match data
      ;; disappearing when using `replace-regexp-in-string' with a
      ;; lambda.
      (let ((pos 0))
        (while (string-match "<img[^>]*src=\\\"\\([^\"]+\\)\\\"[^>]*>" text pos)
          (let ((url (match-string 1 text)))
            (setq text (replace-match (format "[img: %s]" url) t t text)))
          ;; Continue search after replacement
          (setq pos (match-end 0))))
      ;; remove remaining tags
      (setq text (replace-regexp-in-string "<[^>]+>" "" text t t))
      (string-trim text))))

(defun jandan--insert-heading (title)
  "Insert a heading TITLE at the beginning of the current buffer."
  (insert (propertize title 'face '(:height 1.5 :weight bold)))
  (insert "\n\n"))

;;;###autoload
(defun jandan--display-list (buffer-name entries formatter &optional info)
  "Display ENTRIES in a buffer BUFFER-NAME using FORMATTER.
ENTRIES should be a list where each element is passed to
FORMATTER to produce a string.  Only the first
`jandan-page-size' entries are displayed.  INFO is an
optional plist containing metadata about the section being
displayed.  Recognized keys include :section-type (symbol
`comment' or `forum'), :section-id (numeric id), :section-name
(string) and :current-page (integer).  These values are stored
as buffer-local variables for pagination commands."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (setq-local truncate-lines t)
      ;; Set buffer-local metadata
      (when info
        (setq-local jandan-section-type (plist-get info :section-type))
        (setq-local jandan-section-id (plist-get info :section-id))
        (setq-local jandan-section-name (plist-get info :section-name))
        (setq-local jandan-current-page (plist-get info :current-page)))
      ;; Insert items
      (cl-loop for entry in entries
               for idx from 1 do
               (when (> idx jandan-page-size)
                 (cl-return))
               (let ((text (funcall formatter entry idx)))
                 (insert text)
                 (insert "\n\n")))
      (goto-char (point-min))
      (jandan-view-mode)
      ;; Override keymap locally to ensure `n' and `p' call our pagination commands.
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map (kbd "n") #'jandan-next-page)
        (define-key map (kbd "p") #'jandan-prev-page)
        (use-local-map map))
      (pop-to-buffer buf))))

;;; Section commands

(defun jandan--comment-section (post-id section-name &optional page)
  "Fetch a comment-based section by POST-ID and display it.
SECTION-NAME is used as the buffer name heading.  Optional PAGE
number can be passed; default is 1 (most recent).  The API
returns data in descending order when `order=desc`."
  ;; Use page 0 by default, which returns the most recent page.  Passing a
  ;; specific positive page number fetches that numbered page.
  (let* ((pg (if page page 0))
         (url (format "%s/comment/post/%d?order=desc&page=%d" jandan-api-base post-id pg))
         (json (jandan--fetch-json url)))
    (if (not json)
        (message "Failed to load section %s" section-name)
      (let* ((code (jandan--aget "code" json))
             (msg (jandan--decode-string (jandan--aget "msg" json))))
        (cond
         ((and code (not (= code 0)))
          ;; Non-zero code means error (e.g. login required)
          (message "Cannot display %s: %s" section-name msg))
         (t
          (let* ((data (jandan--aget "data" json))
                 (list (jandan--aget "list" data)))
            (jandan--display-list
             (format "*Jandan-%s*" section-name)
             list
             (lambda (item idx)
               (let* ((author (jandan--decode-string (jandan--aget "author" item)))
                      (date (jandan--decode-string (jandan--aget "date_gmt" item)))
                      (content (jandan--decode-string (jandan--aget "content" item)))
                      (oo (jandan--aget "vote_positive" item 0))
                      (xx (jandan--aget "vote_negative" item 0))
                      (sub-cnt (jandan--aget "sub_comment_count" item 0)))
                 (setq content (jandan--strip-html content))
                 (format "%2d. %s\n%s\n点赞:%s 踩:%s 回复:%s"
                         idx
                         (if (or (null author) (string-empty-p author)) "匿名" author)
                         content
                         (or oo 0) (or xx 0) (or sub-cnt 0))))
             ;; metadata for pagination
             (list :section-type 'comment
                   :section-id post-id
                   :section-name section-name
                   :current-page pg)))))))))

;;;###autoload
(defun jandan-qa (&optional page)
  "Browse 问答 (Q&A) posts.  Optional PAGE number selects which page."
  (interactive "p")
  (jandan--comment-section 88399 "问答" (if (= page 1) nil page)))

;;;###autoload
(defun jandan-treehole (&optional page)
  "Browse 树洞 (tree hole) posts.  Optional PAGE number selects which page."
  (interactive "p")
  (jandan--comment-section 102312 "树洞" (if (= page 1) nil page)))

;;;###autoload

;; The 段子 (jokes) section requires login on the new Jandan site and is
;; intentionally omitted from this package.  Previously there was a
;; `jandan-jokes' command here, but it has been removed at the user's
;; request because the endpoint returns an error code when accessed
;; anonymously.  See the commentary for details.

;;;###autoload
(defun jandan-ooxx (&optional page)
  "Browse 随手拍 (snapshots) posts.  Optional PAGE number selects which page."
  (interactive "p")
  (jandan--comment-section 21183 "随手拍" (if (= page 1) nil page)))

;;;###autoload
(defun jandan-pic (&optional page)
  "Browse 无聊图 (random images) posts.  Optional PAGE number selects which page."
  (interactive "p")
  (jandan--comment-section 26402 "无聊图" (if (= page 1) nil page)))

;;; Forum (鱼塘) implementation

(defun jandan--forum-section (forum-id section-name &optional page)
  "Fetch a forum section by FORUM-ID and display it.
SECTION-NAME is used for the buffer heading.  Optional PAGE selects the page number."
  (let* ((pg (or page 1))
         (url (format "%s/forum/posts/%d?page=%d" jandan-api-base forum-id pg))
         (json (jandan--fetch-json url)))
    (if (not json)
        (message "Failed to load section %s" section-name)
      (let* ((code (jandan--aget "code" json))
             (msg (jandan--decode-string (jandan--aget "msg" json))))
        (cond
         ((and code (not (= code 0)))
          (message "Cannot display %s: %s" section-name msg))
         (t
          (let* ((data (jandan--aget "data" json))
                 (list (jandan--aget "list" data)))
            (jandan--display-list
             (format "*Jandan-%s*" section-name)
             list
             (lambda (item idx)
               (let* ((title (jandan--decode-string (jandan--aget "title" item)))
                      (content (jandan--decode-string (jandan--aget "content" item)))
                      (author (jandan--decode-string (jandan--aget "author_name" item)))
                      (create (jandan--decode-string (jandan--aget "create_time" item)))
                      (oo (jandan--aget "oo" item 0))
                      (xx (jandan--aget "xx" item 0))
                      (reply (jandan--aget "reply_count" item 0)))
                 (setq content (jandan--strip-html content))
                 (format "%2d. %s\n标题: %s\n%s\n顶:%s 踩:%s 回复:%s"
                         idx
                         (if (or (null author) (string-empty-p author)) "匿名" author)
                         title
                         content
                         (or oo 0) (or xx 0) (or reply 0))))
             (list :section-type 'forum
                   :section-id forum-id
                   :section-name section-name
                   :current-page pg)))))))))

;;;###autoload
(defun jandan-bbs (&optional page)
  "Browse 鱼塘 (forum) posts.  Optional PAGE selects which page."
  (interactive "p")
  (jandan--forum-section 112928 "鱼塘" (if (= page 1) nil page)))

;;; Top/hot section implementation

(defun jandan-top (&optional type)
  "Browse 热榜 (top/hot) posts.
TYPE determines the timeframe; it can be one of 4hr, 8hr, day,
week or month.  The default is 4hr.  Hot posts are aggregated
across all categories and represent the most upvoted content for
the chosen period."
  (interactive (list (completing-read
                      "Timeframe (4hr, 8hr, day, week, month): "
                      '("4hr" "8hr" "day" "week" "month")
                      nil t nil nil "4hr")))
  (let* ((tframe (or type "4hr"))
         (url (format "%s/top/%s" jandan-api-base tframe))
         (json (jandan--fetch-json url)))
    (if (not json)
        (message "Failed to load hot posts")
      (let ((code (jandan--aget "code" json))
            (msg (jandan--aget "msg" json)))
        (if (and code (not (= code 0)))
            (message "Cannot display hot posts: %s" msg)
          (let* ((data (jandan--aget "data" json)))
            (jandan--display-list
             (format "*Jandan-热榜-%s*" tframe)
             data
             (lambda (item idx)
               (let* ((author (jandan--decode-string (jandan--aget "author" item)))
                      (date (jandan--decode-string (jandan--aget "date" item)))
                      (content (jandan--decode-string (jandan--aget "content" item)))
                      (oo (jandan--aget "vote_positive" item 0))
                      (xx (jandan--aget "vote_negative" item 0))
                      (sub-cnt (jandan--aget "sub_comment_count" item 0)))
                 (setq content (jandan--strip-html content))
                 (format "%2d. %s (%s)\n%s\n顶:%s 踩:%s 回复:%s"
                         idx
                         (if (or (null author) (string-empty-p author)) "匿名" author)
                         (or date "")
                         content
                         (or oo 0) (or xx 0) (or sub-cnt 0)))))))))))

;;; Provide feature
(provide 'jandan)

;;; jandan.el ends here

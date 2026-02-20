;;; github-clone-all.el --- Clone all owned GitHub repos via API v4 -*- lexical-binding: t; -*-

;; Author: yanqirenshi
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: github tools vc
;; URL: https://github.com/yanqirenshi/github.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GitHub GraphQL API (v4) を使い、認証ユーザーがオーナーのリポジトリを
;; すべて取得し、指定ディレクトリに git clone する。
;;
;; すべての処理は非同期で実行されるため、Emacs をブロックしない。
;;
;; 使い方:
;;   1. `github-variable-token' にパーソナルアクセストークンを設定する
;;   2. M-x github-clone-all を実行する
;;
;; トークンには `repo' スコープが必要。
;; 共通変数は `github-variables' で定義されている。

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'github-variables)

(defconst github-clone-all--graphql-query
  "query($cursor: String) {
  viewer {
    repositories(first: 100, after: $cursor, ownerAffiliations: [OWNER], orderBy: {field: NAME, direction: ASC}) {
      pageInfo {
        hasNextPage
        endCursor
      }
      nodes {
        name
        sshUrl
        url
        isArchived
        isFork
      }
    }
  }
}"
  "GraphQL query to fetch repository list.")

;;; ============================================================
;;; State management
;;; ============================================================

(cl-defstruct (github-clone-all--state
               (:constructor github-clone-all--state-create))
  "State of the entire clone operation."
  (target-dir nil)
  (repos nil)
  (pending nil)
  (active 0)
  (results nil)
  (total 0))

(defvar github-clone-all--current-state nil
  "Current running clone operation state.")

;;; ============================================================
;;; Log buffer
;;; ============================================================

(defun github-clone-all--log (format-string &rest args)
  "Log message to buffer and minibuffer using FORMAT-STRING and ARGS."
  (let ((msg (apply #'format format-string args)))
    (message "%s" msg)
    (with-current-buffer (get-buffer-create "*github-clone-all*")
      (goto-char (point-max))
      (insert msg "\n"))))

;;; ============================================================
;;; Async GraphQL request
;;; ============================================================

(defun github-clone-all--ensure-token ()
  "Ensure that `github-variable-token' is set."
  (unless github-variable-token
    (error "`github-variable-token' is not set")))

(defun github-clone-all--parse-response (buffer)
  "Parse HTTP response from BUFFER and return JSON.
Signal an error if the response contains errors."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (let* ((response (json-read))
           (errors (alist-get 'errors response)))
      (when errors
        (error "GitHub API error: %s"
               (mapconcat (lambda (e) (alist-get 'message e))
                          errors ", ")))
      response)))

(defun github-clone-all--graphql-request-async (query variables callback)
  "Send async request to GitHub GraphQL API.
QUERY is a GraphQL query string.  VARIABLES is an alist of variables.
CALLBACK is called with the response as (lambda (response) ...)."
  (github-clone-all--ensure-token)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "bearer " github-variable-token))
            ("Content-Type" . "application/json")))
         (payload (json-encode `((query . ,query)
                                 (variables . ,(or variables :null)))))
         (url-request-data (encode-coding-string payload 'utf-8)))
    (url-retrieve
     "https://api.github.com/graphql"
     (lambda (status cb)
       (if (plist-get status :error)
           (progn
             (github-clone-all--log "API request failed: %s"
                                    (plist-get status :error))
             (kill-buffer (current-buffer)))
         (let ((response (github-clone-all--parse-response (current-buffer))))
           (kill-buffer (current-buffer))
           (funcall cb response))))
     (list callback)
     t t)))

;;; ============================================================
;;; Async pagination: fetch all repositories
;;; ============================================================

(defun github-clone-all--fetch-all-repos-async (callback)
  "Fetch all repositories asynchronously with pagination.
Call CALLBACK with the list of repos when done."
  (github-clone-all--fetch-page-async nil '() callback))

(defun github-clone-all--fetch-page-async (cursor acc callback)
  "Fetch one page from CURSOR, accumulate in ACC, recurse if more pages.
Call CALLBACK when all pages are fetched."
  (let ((variables (if cursor `((cursor . ,cursor)) nil)))
    (github-clone-all--graphql-request-async
     github-clone-all--graphql-query
     variables
     (lambda (response)
       (let* ((data (alist-get 'viewer (alist-get 'data response)))
              (repos-data (alist-get 'repositories data))
              (page-info (alist-get 'pageInfo repos-data))
              (nodes (alist-get 'nodes repos-data))
              (new-acc (append acc (append nodes nil)))
              (has-next (eq (alist-get 'hasNextPage page-info) t))
              (end-cursor (alist-get 'endCursor page-info)))
         (github-clone-all--log "Fetching repositories... %d" (length new-acc))
         (if has-next
             (github-clone-all--fetch-page-async end-cursor new-acc callback)
           (funcall callback new-acc)))))))

;;; ============================================================
;;; Async git clone with concurrency limit
;;; ============================================================

(defun github-clone-all--start-clone-queue (state)
  "Start clone processes from STATE queue up to max parallel limit."
  (while (and (github-clone-all--state-pending state)
              (< (github-clone-all--state-active state)
                 github-variable-max-parallel))
    (let ((repo (pop (github-clone-all--state-pending state))))
      (github-clone-all--clone-repo-async repo state))))

(defun github-clone-all--clone-repo-async (repo state)
  "Clone REPO asynchronously and update STATE on completion."
  (let* ((name (alist-get 'name repo))
         (clone-url (if github-variable-use-ssh
                        (alist-get 'sshUrl repo)
                      (alist-get 'url repo)))
         (target-dir (github-clone-all--state-target-dir state))
         (dest (expand-file-name name target-dir)))
    (cond
     ;; Already cloned
     ((file-directory-p (expand-file-name ".git" dest))
      (github-clone-all--log "  Skip: %s (already cloned)" name)
      (push (cons name :skipped) (github-clone-all--state-results state))
      (github-clone-all--maybe-finish state))

     ;; Clone asynchronously
     (t
      (github-clone-all--log "  Cloning: %s" name)
      (cl-incf (github-clone-all--state-active state))
      (let ((proc (make-process
                   :name (format "git-clone-%s" name)
                   :command (list "git" "clone" clone-url dest)
                   :sentinel
                   (lambda (_process event)
                     (let ((repo-name name)
                           (st state))
                       (cl-decf (github-clone-all--state-active st))
                       (cond
                        ((string-match-p "finished" event)
                         (github-clone-all--log "  Done: %s" repo-name)
                         (push (cons repo-name :cloned)
                               (github-clone-all--state-results st)))
                        (t
                         (github-clone-all--log "  Failed: %s (%s)"
                                                repo-name
                                                (string-trim event))
                         (push (cons repo-name :failed)
                               (github-clone-all--state-results st))))
                       (github-clone-all--maybe-finish st))))))
        proc)))))

(defun github-clone-all--maybe-finish (state)
  "Check if all repos in STATE are done.  Show summary or continue queue."
  (let ((done (length (github-clone-all--state-results state)))
        (total (github-clone-all--state-total state)))
    (if (>= done total)
        (github-clone-all--show-summary state)
      (github-clone-all--start-clone-queue state))))

(defun github-clone-all--show-summary (state)
  "Display clone result summary from STATE."
  (let* ((results (github-clone-all--state-results state))
         (total (github-clone-all--state-total state))
         (cloned (cl-count :cloned results :key #'cdr))
         (skipped (cl-count :skipped results :key #'cdr))
         (failed (cl-count :failed results :key #'cdr)))
    (github-clone-all--log
     "\nDone! Cloned: %d / Skipped: %d / Failed: %d (Total: %d)"
     cloned skipped failed total)
    (when (> failed 0)
      (github-clone-all--log "\nFailed repositories:")
      (dolist (r results)
        (when (eq (cdr r) :failed)
          (github-clone-all--log "  - %s" (car r)))))
    (display-buffer (get-buffer "*github-clone-all*"))
    (setq github-clone-all--current-state nil)))

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun github-clone-all (&optional target-dir)
  "Clone all owned repositories to TARGET-DIR asynchronously.
If TARGET-DIR is omitted, use `github-variable-directory'.
If both are nil, prompt in the minibuffer."
  (interactive)
  (let ((target-dir (or target-dir
                        github-variable-directory
                        (read-directory-name "Clone directory: "))))
    (unless target-dir
      (error "Clone directory is not specified"))
    (when github-clone-all--current-state
      (error "Clone operation already in progress"))
    (github-clone-all--ensure-token)
    (with-current-buffer (get-buffer-create "*github-clone-all*")
      (erase-buffer))
    (github-clone-all--log "Fetching repository list from GitHub...")
    (github-clone-all--fetch-all-repos-async
     (lambda (repos)
       (let* ((total (length repos))
              (state (github-clone-all--state-create
                      :target-dir (expand-file-name target-dir)
                      :repos repos
                      :pending (append repos nil)
                      :active 0
                      :results '()
                      :total total)))
         (setq github-clone-all--current-state state)
         (github-clone-all--log
          "Found %d repositories.  Starting clone..." total)
         (github-clone-all--start-clone-queue state))))))

;;;###autoload
(defun github-clone-all-list ()
  "Display a list of all owned repositories (does not clone)."
  (interactive)
  (github-clone-all--ensure-token)
  (github-clone-all--log "Fetching repository list from GitHub...")
  (github-clone-all--fetch-all-repos-async
   (lambda (repos)
     (with-current-buffer (get-buffer-create "*github-repos*")
       (erase-buffer)
       (insert (format "Owned repositories (%d):\n\n" (length repos)))
       (dolist (repo repos)
         (let ((name (alist-get 'name repo))
               (archived (eq (alist-get 'isArchived repo) t))
               (fork (eq (alist-get 'isFork repo) t)))
           (insert (format "  %s%s%s\n"
                           name
                           (if archived " [archived]" "")
                           (if fork " [fork]" "")))))
       (goto-char (point-min))
       (display-buffer (current-buffer))))))

;;;###autoload
(defun github-clone-all-cancel ()
  "Cancel the running clone operation."
  (interactive)
  (unless github-clone-all--current-state
    (error "No clone operation is running"))
  (dolist (proc (process-list))
    (when (string-prefix-p "git-clone-" (process-name proc))
      (delete-process proc)))
  (github-clone-all--log "\nClone operation cancelled.")
  (display-buffer (get-buffer "*github-clone-all*"))
  (setq github-clone-all--current-state nil))

(provide 'github-clone-all)
;;; github-clone-all.el ends here

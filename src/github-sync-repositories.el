;;; github-sync-repositories.el --- Sync all owned GitHub repos via API v4 -*- lexical-binding: t; -*-

;; Author: yanqirenshi
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: github tools vc
;; URL: https://github.com/yanqirenshi/github-hara

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
;; すべて取得し、指定ディレクトリに同期する。
;;
;; 同期の挙動:
;;   - clone 済みのリポジトリ → 何もしない (スキップ)
;;   - 未 clone のリポジトリ → git clone を実行
;;
;; すべての処理は非同期で実行されるため、Emacs をブロックしない。
;;
;; 使い方:
;;   1. `github-variable-token' にパーソナルアクセストークンを設定する
;;   2. M-x github-sync-repositories を実行する
;;
;; トークンには `repo' スコープが必要。
;; 共通変数は `github-variables' で定義されている。

;;; Code:

(require 'cl-lib)
(require 'github-variables)
(require 'github-api)

(defconst github-sync-repositories--graphql-query
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

(cl-defstruct (github-sync-repositories--state
               (:constructor github-sync-repositories--state-create))
  "State of the entire sync operation."
  (target-dir nil)
  (repos nil)
  (pending nil)
  (active 0)
  (results nil)
  (total 0))

(defvar github-sync-repositories--current-state nil
  "Current running sync operation state.")

;;; ============================================================
;;; Log buffer
;;; ============================================================

(defun github-sync-repositories--log (format-string &rest args)
  "Log message to buffer and minibuffer using FORMAT-STRING and ARGS."
  (let ((msg (apply #'format format-string args)))
    (message "%s" msg)
    (with-current-buffer (get-buffer-create "*github-sync-repositories*")
      (goto-char (point-max))
      (insert msg "\n"))))

;;; ============================================================
;;; Async pagination: fetch all repositories
;;; ============================================================

(defun github-sync-repositories--fetch-all-repos-async (callback)
  "Fetch all repositories asynchronously with pagination.
Call CALLBACK with the list of repos when done."
  (github-sync-repositories--fetch-page-async nil '() callback))

(defun github-sync-repositories--fetch-page-async (cursor acc callback)
  "Fetch one page from CURSOR, accumulate in ACC, recurse if more pages.
Call CALLBACK when all pages are fetched."
  (let ((variables (if cursor `((cursor . ,cursor)) nil)))
    (github-api--graphql-request-async
     github-sync-repositories--graphql-query
     variables
     (lambda (response)
       (let* ((data (alist-get 'viewer (alist-get 'data response)))
              (repos-data (alist-get 'repositories data))
              (page-info (alist-get 'pageInfo repos-data))
              (nodes (alist-get 'nodes repos-data))
              (new-acc (append acc (append nodes nil)))
              (has-next (eq (alist-get 'hasNextPage page-info) t))
              (end-cursor (alist-get 'endCursor page-info)))
         (github-sync-repositories--log "Fetching repositories... %d" (length new-acc))
         (if has-next
             (github-sync-repositories--fetch-page-async end-cursor new-acc callback)
           (funcall callback new-acc))))
     (lambda (err)
       (github-sync-repositories--log "API request failed: %s" err)))))

;;; ============================================================
;;; Async git clone with concurrency limit
;;; ============================================================

(defun github-sync-repositories--start-clone-queue (state)
  "Start clone processes from STATE queue up to max parallel limit."
  (while (and (github-sync-repositories--state-pending state)
              (< (github-sync-repositories--state-active state)
                 github-variable-max-parallel))
    (let ((repo (pop (github-sync-repositories--state-pending state))))
      (github-sync-repositories--clone-repo-async repo state))))

(defun github-sync-repositories--clone-repo-async (repo state)
  "Clone REPO asynchronously and update STATE on completion."
  (let* ((name (alist-get 'name repo))
         (clone-url (if github-variable-use-ssh
                        (alist-get 'sshUrl repo)
                      (alist-get 'url repo)))
         (target-dir (github-sync-repositories--state-target-dir state))
         (dest (expand-file-name name target-dir)))
    (cond
     ;; clone 済み → スキップ
     ((file-directory-p (expand-file-name ".git" dest))
      (github-sync-repositories--log "  Skip: %s (already cloned)" name)
      (push (cons name :skipped) (github-sync-repositories--state-results state))
      (github-sync-repositories--maybe-finish state))

     ;; 未 clone → git clone を実行
     (t
      (github-sync-repositories--log "  Cloning: %s" name)
      (cl-incf (github-sync-repositories--state-active state))
      (let ((proc (make-process
                   :name (format "git-clone-%s" name)
                   :command (list "git" "clone" clone-url dest)
                   :sentinel
                   (lambda (_process event)
                     (let ((repo-name name)
                           (st state))
                       (cl-decf (github-sync-repositories--state-active st))
                       (cond
                        ((string-match-p "finished" event)
                         (github-sync-repositories--log "  Done: %s" repo-name)
                         (push (cons repo-name :cloned)
                               (github-sync-repositories--state-results st)))
                        (t
                         (github-sync-repositories--log "  Failed: %s (%s)"
                                                repo-name
                                                (string-trim event))
                         (push (cons repo-name :failed)
                               (github-sync-repositories--state-results st))))
                       (github-sync-repositories--maybe-finish st))))))
        proc)))))

(defun github-sync-repositories--maybe-finish (state)
  "Check if all repos in STATE are done.  Show summary or continue queue."
  (let ((done (length (github-sync-repositories--state-results state)))
        (total (github-sync-repositories--state-total state)))
    (if (>= done total)
        (github-sync-repositories--show-summary state)
      (github-sync-repositories--start-clone-queue state))))

(defun github-sync-repositories--show-summary (state)
  "Display sync result summary from STATE."
  (let* ((results (github-sync-repositories--state-results state))
         (total (github-sync-repositories--state-total state))
         (cloned (cl-count :cloned results :key #'cdr))
         (skipped (cl-count :skipped results :key #'cdr))
         (failed (cl-count :failed results :key #'cdr)))
    (github-sync-repositories--log
     "\nDone! Cloned: %d / Skipped: %d / Failed: %d (Total: %d)"
     cloned skipped failed total)
    (when (> failed 0)
      (github-sync-repositories--log "\nFailed repositories:")
      (dolist (r results)
        (when (eq (cdr r) :failed)
          (github-sync-repositories--log "  - %s" (car r)))))
    (display-buffer (get-buffer "*github-sync-repositories*"))
    (setq github-sync-repositories--current-state nil)))

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun github-sync-repositories (&optional target-dir)
  "Sync all owned repositories to TARGET-DIR asynchronously.
Clone missing repositories and skip already cloned ones.
If TARGET-DIR is omitted, use `github-variable-directory'.
If both are nil, prompt in the minibuffer."
  (interactive)
  (let ((target-dir (or target-dir
                        github-variable-directory
                        (read-directory-name "Sync directory: "))))
    (unless target-dir
      (error "Sync directory is not specified"))
    (when github-sync-repositories--current-state
      (error "Sync operation already in progress"))
    (github-api--ensure-token)
    (with-current-buffer (get-buffer-create "*github-sync-repositories*")
      (erase-buffer))
    (github-sync-repositories--log "Fetching repository list from GitHub...")
    (github-sync-repositories--fetch-all-repos-async
     (lambda (repos)
       (let* ((total (length repos))
              (state (github-sync-repositories--state-create
                      :target-dir (expand-file-name target-dir)
                      :repos repos
                      :pending (append repos nil)
                      :active 0
                      :results '()
                      :total total)))
         (setq github-sync-repositories--current-state state)
         (github-sync-repositories--log
          "Found %d repositories.  Starting sync..." total)
         (github-sync-repositories--start-clone-queue state))))))

;;;###autoload
(defun github-sync-repositories-list ()
  "Display a list of all owned repositories (does not clone)."
  (interactive)
  (github-api--ensure-token)
  (github-sync-repositories--log "Fetching repository list from GitHub...")
  (github-sync-repositories--fetch-all-repos-async
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
(defun github-sync-repositories-cancel ()
  "Cancel the running sync operation."
  (interactive)
  (unless github-sync-repositories--current-state
    (error "No sync operation is running"))
  (dolist (proc (process-list))
    (when (string-prefix-p "git-clone-" (process-name proc))
      (delete-process proc)))
  (github-sync-repositories--log "\nSync operation cancelled.")
  (display-buffer (get-buffer "*github-sync-repositories*"))
  (setq github-sync-repositories--current-state nil))

(provide 'github-sync-repositories)
;;; github-sync-repositories.el ends here

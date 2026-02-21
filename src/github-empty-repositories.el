;;; github-empty-repositories.el --- List empty repos on GitHub and local -*- lexical-binding: t; -*-

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

;; GitHub 上の空リポジトリ (コミットなし) と、ローカルの空リポジトリを
;; 検出する。
;;
;; 検出関数はリストを返す。表示は別関数で行う。
;;
;; 空リポジトリの判定:
;;   - GitHub: defaultBranchRef が null (ブランチ/コミットが存在しない)
;;   - ローカル: 空ディレクトリ / .git なし / コミットなし の3パターン
;;
;; 使い方:
;;   M-x github-empty-repositories       GitHub + ローカル両方をチェック
;;   M-x github-empty-repositories-local  ローカルのみチェック (トークン不要)

;;; Code:

(require 'cl-lib)
(require 'github-variables)
(require 'github-api)

;;; ============================================================
;;; GraphQL query
;;; ============================================================

(defconst github-empty-repositories--graphql-query
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
        defaultBranchRef {
          name
        }
      }
    }
  }
}"
  "GraphQL query to detect empty repositories.
When `defaultBranchRef' is null, the repository has no commits.")

;;; ============================================================
;;; GitHub remote: fetch and filter empty repositories
;;; ============================================================

(defun github-empty-repositories--fetch-all-repos-async (callback)
  "Fetch all repositories asynchronously with pagination.
Call CALLBACK with the list of repos when done."
  (github-empty-repositories--fetch-page-async nil '() callback))

(defun github-empty-repositories--fetch-page-async (cursor acc callback)
  "Fetch one page from CURSOR, accumulate in ACC, recurse if more pages.
Call CALLBACK when all pages are fetched."
  (let ((variables (if cursor `((cursor . ,cursor)) nil)))
    (github-api--graphql-request-async
     github-empty-repositories--graphql-query
     variables
     (lambda (response)
       (let* ((data (alist-get 'viewer (alist-get 'data response)))
              (repos-data (alist-get 'repositories data))
              (page-info (alist-get 'pageInfo repos-data))
              (nodes (alist-get 'nodes repos-data))
              (new-acc (append acc (append nodes nil)))
              (has-next (eq (alist-get 'hasNextPage page-info) t))
              (end-cursor (alist-get 'endCursor page-info)))
         (if has-next
             (github-empty-repositories--fetch-page-async end-cursor new-acc callback)
           (funcall callback new-acc))))
     (lambda (err)
       (error "GitHub API request failed: %s" err)))))

(defun github-empty-repositories-filter-empty (repos)
  "Filter REPOS to keep only empty ones (defaultBranchRef is null).
Return a list of alists, each containing repository information."
  (cl-remove-if-not
   (lambda (repo)
     (null (alist-get 'defaultBranchRef repo)))
   repos))

;;; ============================================================
;;; Local: detect empty repositories
;;; ============================================================

(defun github-empty-repositories-local-status (&optional dir)
  "Check the repository status of DIR.
If DIR is omitted, use `github-variable-directory'.
Return value is one of:
  :empty-dir   - directory is empty
  :no-git      - no .git directory (not a git repository)
  :no-commits  - .git exists but HEAD cannot be resolved (no commits)
  :has-commits - normal repository with commits
  nil          - directory does not exist"
  (let ((dir (or dir github-variable-directory)))
    (unless dir
      (error "Directory is not specified"))
    (cond
   ((not (file-directory-p dir)) nil)
   ((null (directory-files dir nil "^[^.]" t)) :empty-dir)
   ((not (file-directory-p (expand-file-name ".git" dir))) :no-git)
   (t
    ;; HEAD が解決できるかチェック
    (let ((default-directory dir))
      (if (= 0 (call-process "git" nil nil nil "rev-parse" "HEAD"))
          :has-commits
        :no-commits))))))

(defun github-empty-repositories-find-empty-local (&optional target-dir)
  "Scan subdirectories of TARGET-DIR and detect empty repositories.
If TARGET-DIR is omitted, use `github-variable-directory'.
Return a list of (name . status) where status is one of
`:empty-dir', `:no-git', or `:no-commits'."
  (let* ((target-dir (or target-dir github-variable-directory))
         (result '()))
    (unless target-dir
      (error "Directory is not specified"))
    (dolist (entry (directory-files target-dir t "^[^.]" t))
      (when (file-directory-p entry)
        (let ((status (github-empty-repositories-local-status entry)))
          (when (and status (not (eq status :has-commits)))
            (push (cons (file-name-nondirectory entry) status) result)))))
    (nreverse result)))

;;; ============================================================
;;; Async: fetch remote empty repositories
;;; ============================================================

(defun github-empty-repositories-find-empty-remote (callback)
  "Fetch all owned repositories from GitHub and filter empty ones.
Call CALLBACK with a list of empty repository alists.
Each alist contains `name', `sshUrl', `url', `isArchived', `isFork'."
  (github-api--ensure-token)
  (github-empty-repositories--fetch-all-repos-async
   (lambda (repos)
     (funcall callback (github-empty-repositories-filter-empty repos)))))

;;; ============================================================
;;; Display results (buffer output)
;;; ============================================================

(defconst github-empty-repositories--buffer-name "*github-empty-repositories*"
  "Buffer name for empty repository listing.")

(defun github-empty-repositories-display (remote-empty local-empty target-dir)
  "Display combined results of REMOTE-EMPTY and LOCAL-EMPTY in a buffer.
REMOTE-EMPTY is a list of repository alists (or nil if not checked).
LOCAL-EMPTY is a list of (name . status) pairs.
TARGET-DIR is the local directory that was checked."
  (with-current-buffer (get-buffer-create github-empty-repositories--buffer-name)
    (erase-buffer)
    ;; ヘッダー
    (insert "=== 空リポジトリ一覧 ===\n\n")

    ;; GitHub リモート
    (if remote-empty
        (progn
          (insert (format "■ GitHub 上の空リポジトリ (%d 件):\n\n"
                          (length remote-empty)))
          (dolist (repo remote-empty)
            (let ((name (alist-get 'name repo))
                  (archived (eq (alist-get 'isArchived repo) t))
                  (fork (eq (alist-get 'isFork repo) t)))
              (insert (format "  %s%s%s\n"
                              name
                              (if archived " [archived]" "")
                              (if fork " [fork]" ""))))))
      (insert "■ GitHub 上の空リポジトリ (0 件):\n\n  (なし)\n"))

    (insert "\n")

    ;; ローカル
    (if local-empty
        (progn
          (insert (format "■ ローカルの空リポジトリ (%s, %d 件):\n\n"
                          target-dir (length local-empty)))
          (dolist (entry local-empty)
            (let* ((name (car entry))
                   (status (cdr entry))
                   (label (pcase status
                            (:empty-dir  "空ディレクトリ")
                            (:no-git     ".git なし")
                            (:no-commits "コミットなし"))))
              (insert (format "  %s [%s]\n" name label)))))
      (insert (format "■ ローカルの空リポジトリ (%s, 0 件):\n\n  (なし)\n"
                      target-dir)))

    (insert "\n")

    ;; 両方に該当するリポジトリ
    (when remote-empty
      (let* ((remote-names (mapcar (lambda (r) (alist-get 'name r)) remote-empty))
             (local-names (mapcar #'car local-empty))
             (both (cl-intersection remote-names local-names :test #'string=)))
        (when both
          (insert (format "■ リモート・ローカル両方で空 (%d 件):\n\n" (length both)))
          (dolist (name both)
            (insert (format "  %s\n" name)))
          (insert "\n"))))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun github-empty-repositories (&optional target-dir)
  "List empty repositories on both GitHub and local directory.
Check GitHub for repos with no commits (defaultBranchRef is null),
and scan TARGET-DIR for empty local directories or repos.
If TARGET-DIR is omitted, use `github-variable-directory'.
If both are nil, prompt in the minibuffer."
  (interactive)
  (let ((target-dir (or target-dir
                        github-variable-directory
                        (read-directory-name "Check directory: "))))
    (unless target-dir
      (error "Directory is not specified"))
    ;; ローカルチェック (同期) を先に実行
    (let ((local-empty (github-empty-repositories-find-empty-local
                        (expand-file-name target-dir))))
      ;; GitHub API (非同期) を実行し、結果を表示
      (github-empty-repositories-find-empty-remote
       (lambda (remote-empty)
         (github-empty-repositories-display
          remote-empty local-empty (expand-file-name target-dir)))))))

;;;###autoload
(defun github-empty-repositories-local (&optional target-dir)
  "List empty repositories in local directory only.
Does not use GitHub API, so no token is required.
If TARGET-DIR is omitted, use `github-variable-directory'.
If both are nil, prompt in the minibuffer."
  (interactive)
  (let ((target-dir (or target-dir
                        github-variable-directory
                        (read-directory-name "Check directory: "))))
    (unless target-dir
      (error "Directory is not specified"))
    (let ((local-empty (github-empty-repositories-find-empty-local
                        (expand-file-name target-dir))))
      (github-empty-repositories-display
       nil local-empty (expand-file-name target-dir)))))

(provide 'github-empty-repositories)
;;; github-empty-repositories.el ends here

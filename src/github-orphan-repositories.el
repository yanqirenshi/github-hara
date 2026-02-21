;;; github-orphan-repositories.el --- Detect local repos not on GitHub -*- lexical-binding: t; -*-

;; Author: yanqirenshi
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: github tools vc
;; URL: https://github.com/yanqirenshi/github-sitter

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

;; ローカルにクローン済みだが、GitHub 上にもう存在しないリポジトリを
;; 検出する。
;;
;; 検出関数はリストを返す。表示は別関数で行う。
;;
;; 判定方法:
;;   GitHub GraphQL API から自分がオーナーのリポジトリ名一覧を取得し、
;;   ローカルの git リポジトリ名と比較する。ローカルにあるが GitHub に
;;   ないものを「孤立リポジトリ (orphaned)」として検出する。
;;
;; 注意:
;;   ownerAffiliations: [OWNER] でフィルタするため、コラボレータ権限のみ
;;   のリポジトリや Organization リポジトリは GitHub 側に含まれない。
;;
;; 使い方:
;;   M-x github-orphan-repositories  孤立リポジトリを一覧表示

;;; Code:

(require 'cl-lib)
(require 'github-variables)
(require 'github-api)

;;; ============================================================
;;; GraphQL query
;;; ============================================================

(defconst github-orphan-repositories--graphql-query
  "query($cursor: String) {
  viewer {
    repositories(first: 100, after: $cursor, ownerAffiliations: [OWNER], orderBy: {field: NAME, direction: ASC}) {
      pageInfo {
        hasNextPage
        endCursor
      }
      nodes {
        name
      }
    }
  }
}"
  "GraphQL query to fetch repository names for orphan detection.")

;;; ============================================================
;;; Internal: fetch repositories from GitHub
;;; ============================================================

(defun github-orphan-repositories--fetch-all-repos-async (callback)
  "Fetch all repository names asynchronously with pagination.
Call CALLBACK with a list of repository name strings when done."
  (github-orphan-repositories--fetch-page-async nil '() callback))

(defun github-orphan-repositories--fetch-page-async (cursor acc callback)
  "Fetch one page from CURSOR, accumulate names in ACC, recurse if more pages.
Call CALLBACK when all pages are fetched."
  (let ((variables (if cursor `((cursor . ,cursor)) nil)))
    (github-api--graphql-request-async
     github-orphan-repositories--graphql-query
     variables
     (lambda (response)
       (let* ((data (alist-get 'viewer (alist-get 'data response)))
              (repos-data (alist-get 'repositories data))
              (page-info (alist-get 'pageInfo repos-data))
              (nodes (alist-get 'nodes repos-data))
              (names (mapcar (lambda (n) (alist-get 'name n)) (append nodes nil)))
              (new-acc (append acc names))
              (has-next (eq (alist-get 'hasNextPage page-info) t))
              (end-cursor (alist-get 'endCursor page-info)))
         (if has-next
             (github-orphan-repositories--fetch-page-async end-cursor new-acc callback)
           (funcall callback new-acc))))
     (lambda (err)
       (error "GitHub API request failed: %s" err)))))

;;; ============================================================
;;; Internal: collect local git repositories
;;; ============================================================

(defun github-orphan-repositories--collect-local-repos (target-dir)
  "Collect names of git repositories in TARGET-DIR.
Return a list of directory names that contain a `.git' subdirectory."
  (let ((result '()))
    (dolist (entry (directory-files target-dir t "^[^.]" t))
      (when (and (file-directory-p entry)
                 (file-directory-p (expand-file-name ".git" entry)))
        (push (file-name-nondirectory entry) result)))
    (nreverse result)))

;;; ============================================================
;;; Internal: compare and find orphans
;;; ============================================================

(defun github-orphan-repositories--find-orphans (remote-names local-names)
  "Find local repos not present on GitHub.
REMOTE-NAMES is a list of GitHub repository name strings.
LOCAL-NAMES is a list of local directory name strings.
Return a list of orphaned repository name strings (sorted)."
  (let ((remote-set (make-hash-table :test 'equal)))
    ;; リモート名をハッシュテーブルに格納 (高速検索用)
    (dolist (name remote-names)
      (puthash name t remote-set))
    ;; ローカルにあるがリモートにないものを検出
    (sort (cl-remove-if (lambda (name) (gethash name remote-set))
                        local-names)
          #'string<)))

;;; ============================================================
;;; Public: find orphan repositories
;;; ============================================================

(defun github-orphan-repositories-find-orphans (callback &optional target-dir)
  "Find local repos that no longer exist on GitHub.
Fetch all owned repos from GitHub and compare with local repos in TARGET-DIR.
If TARGET-DIR is omitted, use `github-variable-directory'.
Call CALLBACK with a list of orphaned repository name strings."
  (let ((target-dir (or target-dir github-variable-directory)))
    (unless target-dir
      (error "Directory is not specified"))
    (github-api--ensure-token)
    (let ((local-names (github-orphan-repositories--collect-local-repos
                        (expand-file-name target-dir))))
      (github-orphan-repositories--fetch-all-repos-async
       (lambda (remote-names)
         (funcall callback
                  (github-orphan-repositories--find-orphans
                   remote-names local-names)))))))

;;; ============================================================
;;; Display results (buffer output)
;;; ============================================================

(defconst github-orphan-repositories--buffer-name "*github-orphan-repositories*"
  "Buffer name for orphan repository listing.")

(defun github-orphan-repositories-display (orphans target-dir)
  "Display ORPHANS in a buffer.
ORPHANS is a list of repository name strings.
TARGET-DIR is the local directory that was checked."
  (with-current-buffer (get-buffer-create github-orphan-repositories--buffer-name)
    (erase-buffer)
    (insert "=== GitHub 上に存在しないローカルリポジトリ一覧 ===\n\n")
    (insert (format "対象ディレクトリ: %s\n\n" target-dir))
    (if orphans
        (progn
          (insert (format "%d 件の孤立リポジトリが見つかりました:\n\n" (length orphans)))
          (dolist (name orphans)
            (insert (format "  %s\n" name))))
      (insert "すべてのローカルリポジトリは GitHub 上に存在します。\n"))
    (insert "\n")
    (insert "注意: ownerAffiliations: [OWNER] でフィルタしているため、\n")
    (insert "コラボレータ権限のみのリポジトリや Organization リポジトリは\n")
    (insert "GitHub 側に含まれません。\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun github-orphan-repositories (&optional target-dir)
  "List local repositories that no longer exist on GitHub.
Scan subdirectories of TARGET-DIR and compare with GitHub repos.
If TARGET-DIR is omitted, use `github-variable-directory'.
If both are nil, prompt in the minibuffer."
  (interactive)
  (let ((target-dir (or target-dir
                        github-variable-directory
                        (read-directory-name "Check directory: "))))
    (unless target-dir
      (error "Directory is not specified"))
    (let ((expanded (expand-file-name target-dir)))
      (github-orphan-repositories-find-orphans
       (lambda (orphans)
         (github-orphan-repositories-display orphans expanded))
       expanded))))

(provide 'github-orphan-repositories)
;;; github-orphan-repositories.el ends here

;;; github-behind-repositories.el --- Detect repos that need git pull -*- lexical-binding: t; -*-

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

;; ローカルにクローン済みのリポジトリのうち、リモート追跡ブランチより
;; 遅れている (pull が必要な) ものを検出する。
;;
;; 検出関数はリストを返す。表示は別関数で行う。
;;
;; 判定方法:
;;   `git rev-list --count HEAD..@{u}' で現在のブランチがリモート追跡
;;   ブランチに対して何コミット遅れているかをカウントする。
;;   ※ `git fetch' なしの場合、ローカルにキャッシュされたリモート情報に
;;   基づく判定となる。
;;
;; 使い方:
;;   M-x github-behind-repositories  pull が必要なリポジトリを一覧表示

;;; Code:

(require 'cl-lib)
(require 'github-variables)

;;; ============================================================
;;; Internal: parse git status
;;; ============================================================

(defun github-behind-repositories--parse-status (dir)
  "Parse git status of DIR and return an alist.
Return value is an alist with keys:
  `behind'   - number of commits behind upstream
  `ahead'    - number of commits ahead of upstream
  `branch'   - current branch name
  `upstream' - upstream tracking branch name
Return nil if DIR is not a git repository or has no upstream."
  (let ((default-directory (expand-file-name dir)))
    ;; git リポジトリかチェック
    (unless (= 0 (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree"))
      (cl-return-from github-behind-repositories--parse-status nil))
    ;; 現在のブランチ名を取得
    (let ((branch (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "git" nil t nil "rev-parse" "--abbrev-ref" "HEAD")))))
      (setq branch (string-trim branch))
      ;; detached HEAD の場合は nil を返す
      (when (string= branch "HEAD")
        (cl-return-from github-behind-repositories--parse-status nil))
      ;; upstream の追跡ブランチを取得 (まず存在チェック)
      (let ((exit-code
             (with-temp-buffer
               (call-process "git" nil t nil
                             "rev-parse" "--abbrev-ref"
                             (concat branch "@{upstream}")))))
        ;; upstream が設定されていない場合 (コマンド失敗)
        (unless (= 0 exit-code)
          (cl-return-from github-behind-repositories--parse-status nil))
        ;; upstream 名を取得
        (let ((upstream-result
               (with-output-to-string
                 (with-current-buffer standard-output
                   (call-process "git" nil t nil
                                 "rev-parse" "--abbrev-ref"
                                 (concat branch "@{upstream}"))))))
          (setq upstream-result (string-trim upstream-result))
          (when (string= upstream-result "")
            (cl-return-from github-behind-repositories--parse-status nil))
          ;; behind の数を取得
          (let ((behind-str (with-output-to-string
                              (with-current-buffer standard-output
                                (call-process "git" nil t nil
                                              "rev-list" "--count"
                                              (concat "HEAD.." branch "@{upstream}"))))))
            (setq behind-str (string-trim behind-str))
            ;; ahead の数を取得
            (let ((ahead-str (with-output-to-string
                               (with-current-buffer standard-output
                                 (call-process "git" nil t nil
                                               "rev-list" "--count"
                                               (concat branch "@{upstream}..HEAD"))))))
              (setq ahead-str (string-trim ahead-str))
              `((behind . ,(string-to-number behind-str))
                (ahead . ,(string-to-number ahead-str))
                (branch . ,branch)
                (upstream . ,upstream-result))))))))))

;;; ============================================================
;;; Public: status and detection functions
;;; ============================================================

(defun github-behind-repositories-local-status (&optional dir)
  "Return the behind/ahead status of the repository at DIR.
If DIR is omitted, use `github-variable-directory'.
Return value is an alist with keys `behind', `ahead', `branch', `upstream'.
Return nil if DIR is not a git repository or has no upstream."
  (let ((dir (or dir github-variable-directory)))
    (unless dir
      (error "Directory is not specified"))
    (unless (file-directory-p dir)
      (cl-return-from github-behind-repositories-local-status nil))
    (github-behind-repositories--parse-status (expand-file-name dir))))

(defun github-behind-repositories-find-behind-local (&optional target-dir)
  "Scan subdirectories of TARGET-DIR and detect repos that need pull.
If TARGET-DIR is omitted, use `github-variable-directory'.
Return a list of (name . status-alist) where status-alist contains
`behind', `ahead', `branch', `upstream'.
Only repositories with behind > 0 are included."
  (let* ((target-dir (or target-dir github-variable-directory))
         (result '()))
    (unless target-dir
      (error "Directory is not specified"))
    (dolist (entry (directory-files target-dir t "^[^.]" t))
      (when (file-directory-p entry)
        (let ((status (github-behind-repositories--parse-status entry)))
          (when (and status (> (alist-get 'behind status) 0))
            (push (cons (file-name-nondirectory entry) status) result)))))
    (nreverse result)))

;;; ============================================================
;;; Display results (buffer output)
;;; ============================================================

(defconst github-behind-repositories--buffer-name "*github-behind-repositories*"
  "Buffer name for behind repository listing.")

(defun github-behind-repositories-display (repos target-dir)
  "Display REPOS that need pull in a buffer.
REPOS is a list of (name . status-alist) pairs.
TARGET-DIR is the local directory that was checked."
  (with-current-buffer (get-buffer-create github-behind-repositories--buffer-name)
    (erase-buffer)
    (insert "=== pull が必要なリポジトリ一覧 ===\n\n")
    (insert (format "対象ディレクトリ: %s\n\n" target-dir))
    (if repos
        (progn
          (insert (format "%d 件のリポジトリが遅れています:\n\n" (length repos)))
          (dolist (entry repos)
            (let* ((name (car entry))
                   (status (cdr entry))
                   (behind (alist-get 'behind status))
                   (ahead (alist-get 'ahead status))
                   (branch (alist-get 'branch status))
                   (upstream (alist-get 'upstream status)))
              (insert (format "  %s\n" name))
              (insert (format "    ブランチ: %s → %s\n" branch upstream))
              (insert (format "    遅れ: %d コミット" behind))
              (when (> ahead 0)
                (insert (format " / 先行: %d コミット" ahead)))
              (insert "\n\n"))))
      (insert "すべてのリポジトリは最新です。\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; ============================================================
;;; Interactive commands
;;; ============================================================

;;;###autoload
(defun github-behind-repositories (&optional target-dir)
  "List local repositories that need git pull.
Scan subdirectories of TARGET-DIR and show repos behind upstream.
If TARGET-DIR is omitted, use `github-variable-directory'.
If both are nil, prompt in the minibuffer."
  (interactive)
  (let ((target-dir (or target-dir
                        github-variable-directory
                        (read-directory-name "Check directory: "))))
    (unless target-dir
      (error "Directory is not specified"))
    (let* ((expanded (expand-file-name target-dir))
           (repos (github-behind-repositories-find-behind-local expanded)))
      (github-behind-repositories-display repos expanded))))

(provide 'github-behind-repositories)
;;; github-behind-repositories.el ends here

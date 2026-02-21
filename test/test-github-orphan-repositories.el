;;; test-github-orphan-repositories.el --- Tests for github-orphan-repositories.el -*- lexical-binding: t; -*-

;;; Commentary:

;; github-orphan-repositories.el のユニットテスト。
;; ファイルシステム操作は一時ディレクトリを使ってテストする。

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((src-dir (expand-file-name "../src" (file-name-directory load-file-name))))
  (add-to-list 'load-path src-dir))

(require 'github-variables)
(require 'github-api)
(require 'github-orphan-repositories)

;;; ============================================================
;;; ヘルパー: テスト用一時ディレクトリの作成
;;; ============================================================

(defmacro with-test-directory (&rest body)
  "一時ディレクトリを作成し BODY を実行後、削除する。
BODY 内では `test-dir' で一時ディレクトリのパスを参照できる。"
  (declare (indent 0) (debug t))
  `(let ((test-dir (make-temp-file "github-orphan-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory test-dir t))))

(defun test-orphan--create-git-repo (dir)
  "DIR に git リポジトリを作成するヘルパー。"
  (make-directory dir t)
  (let ((default-directory dir))
    (call-process "git" nil nil nil "init")
    (with-temp-file (expand-file-name "file.txt" dir)
      (insert "hello"))
    (call-process "git" nil nil nil "add" ".")
    (call-process "git" nil nil nil
                  "-c" "user.name=Test"
                  "-c" "user.email=test@test.com"
                  "commit" "-m" "init")))

;;; ============================================================
;;; github-orphan-repositories--collect-local-repos
;;; ============================================================

(ert-deftest test-orphan-collect/git-repos-only ()
  "git リポジトリ (.git あり) のみを収集する。"
  (with-test-directory
    ;; git リポジトリ
    (test-orphan--create-git-repo (expand-file-name "repo-a" test-dir))
    (test-orphan--create-git-repo (expand-file-name "repo-b" test-dir))
    ;; .git なしのディレクトリ
    (make-directory (expand-file-name "not-a-repo" test-dir))
    ;; 空ディレクトリ
    (make-directory (expand-file-name "empty-dir" test-dir))
    (let ((result (github-orphan-repositories--collect-local-repos test-dir)))
      (should (= 2 (length result)))
      (should (member "repo-a" result))
      (should (member "repo-b" result))
      (should-not (member "not-a-repo" result))
      (should-not (member "empty-dir" result)))))

(ert-deftest test-orphan-collect/empty-parent ()
  "サブディレクトリがない場合は空リストを返す。"
  (with-test-directory
    (should (null (github-orphan-repositories--collect-local-repos test-dir)))))

(ert-deftest test-orphan-collect/ignores-dotfiles ()
  "ドットで始まるディレクトリは無視する。"
  (with-test-directory
    (let ((hidden (expand-file-name ".hidden-repo" test-dir)))
      (test-orphan--create-git-repo hidden))
    (test-orphan--create-git-repo (expand-file-name "visible-repo" test-dir))
    (let ((result (github-orphan-repositories--collect-local-repos test-dir)))
      (should (= 1 (length result)))
      (should (equal "visible-repo" (car result))))))

;;; ============================================================
;;; github-orphan-repositories--find-orphans
;;; ============================================================

(ert-deftest test-orphan-find/some-orphans ()
  "ローカルにあるがリモートにないリポジトリを検出する。"
  (let ((remote '("repo-a" "repo-b" "repo-c"))
        (local '("repo-a" "repo-d" "repo-e")))
    (let ((result (github-orphan-repositories--find-orphans remote local)))
      (should (= 2 (length result)))
      (should (equal '("repo-d" "repo-e") result)))))

(ert-deftest test-orphan-find/no-orphans ()
  "すべてのローカルリポジトリがリモートに存在する場合は空リストを返す。"
  (let ((remote '("repo-a" "repo-b" "repo-c"))
        (local '("repo-a" "repo-b")))
    (should (null (github-orphan-repositories--find-orphans remote local)))))

(ert-deftest test-orphan-find/all-orphans ()
  "すべてのローカルリポジトリがリモートに存在しない場合。"
  (let ((remote '("repo-x" "repo-y"))
        (local '("repo-a" "repo-b")))
    (let ((result (github-orphan-repositories--find-orphans remote local)))
      (should (= 2 (length result)))
      (should (equal '("repo-a" "repo-b") result)))))

(ert-deftest test-orphan-find/empty-local ()
  "ローカルリポジトリがない場合は空リストを返す。"
  (let ((remote '("repo-a" "repo-b"))
        (local '()))
    (should (null (github-orphan-repositories--find-orphans remote local)))))

(ert-deftest test-orphan-find/empty-remote ()
  "リモートリポジトリがない場合、すべてのローカルが孤立となる。"
  (let ((remote '())
        (local '("repo-a" "repo-b")))
    (let ((result (github-orphan-repositories--find-orphans remote local)))
      (should (= 2 (length result)))
      (should (equal '("repo-a" "repo-b") result)))))

(ert-deftest test-orphan-find/sorted-result ()
  "結果はアルファベット順にソートされる。"
  (let ((remote '("a"))
        (local '("z-repo" "a-repo" "m-repo")))
    (let ((result (github-orphan-repositories--find-orphans remote local)))
      (should (equal '("a-repo" "m-repo" "z-repo") result)))))

;;; ============================================================
;;; github-orphan-repositories-find-orphans
;;; ============================================================

(ert-deftest test-orphan-find-orphans/no-token-signals-error ()
  "トークン未設定で呼び出すとエラーになる。"
  (let ((github-variable-token nil))
    (should-error
     (github-orphan-repositories-find-orphans #'ignore "/tmp")
     :type 'error)))

(ert-deftest test-orphan-find-orphans/nil-dir-and-nil-variable-signals-error ()
  "引数も github-variable-directory も nil の場合エラーを投げる。"
  (let ((github-variable-directory nil))
    (should-error
     (github-orphan-repositories-find-orphans #'ignore)
     :type 'error)))

;;; ============================================================
;;; github-orphan-repositories-display
;;; ============================================================

(ert-deftest test-orphan-display/with-orphans ()
  "孤立リポジトリがある場合、バッファに正しく表示する。"
  (let ((orphans '("deleted-repo" "old-project"))
        (buf-name "*github-orphan-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-orphan-repositories-display orphans "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "GitHub 上に存在しないローカルリポジトリ一覧" content))
        (should (string-match-p "2 件の孤立リポジトリが見つかりました" content))
        (should (string-match-p "deleted-repo" content))
        (should (string-match-p "old-project" content))))
    (kill-buffer buf-name)))

(ert-deftest test-orphan-display/no-orphans ()
  "孤立リポジトリがない場合、適切なメッセージを表示する。"
  (let ((buf-name "*github-orphan-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-orphan-repositories-display nil "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "すべてのローカルリポジトリは GitHub 上に存在します" content))))
    (kill-buffer buf-name)))

(ert-deftest test-orphan-display/shows-target-dir ()
  "対象ディレクトリがバッファに表示される。"
  (let ((buf-name "*github-orphan-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-orphan-repositories-display nil "/home/user/repos"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "/home/user/repos" content))))
    (kill-buffer buf-name)))

(ert-deftest test-orphan-display/shows-warning ()
  "ownerAffiliations の注意事項が表示される。"
  (let ((buf-name "*github-orphan-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-orphan-repositories-display nil "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "ownerAffiliations" content))
        (should (string-match-p "Organization" content))))
    (kill-buffer buf-name)))

;;; ============================================================
;;; github-orphan-repositories (インタラクティブコマンド)
;;; ============================================================

(ert-deftest test-orphan-command/nil-dir-and-nil-variable-signals-error ()
  "引数も github-variable-directory も nil の場合エラーを投げる。"
  (let ((github-variable-directory nil))
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) nil)))
      (should-error (github-orphan-repositories) :type 'error))))

(provide 'test-github-orphan-repositories)
;;; test-github-orphan-repositories.el ends here

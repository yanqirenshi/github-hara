;;; test-github-empty-repositories.el --- Tests for github-empty-repositories.el -*- lexical-binding: t; -*-

;;; Commentary:

;; github-empty-repositories.el のユニットテスト。
;; ファイルシステム操作は一時ディレクトリを使ってテストする。

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((src-dir (expand-file-name "../src" (file-name-directory load-file-name))))
  (add-to-list 'load-path src-dir))

(require 'github-variables)
(require 'github-api)
(require 'github-empty-repositories)

;;; ============================================================
;;; ヘルパー: テスト用一時ディレクトリの作成
;;; ============================================================

(defmacro with-test-directory (&rest body)
  "一時ディレクトリを作成し BODY を実行後、削除する。
BODY 内では `test-dir' で一時ディレクトリのパスを参照できる。"
  (declare (indent 0) (debug t))
  `(let ((test-dir (make-temp-file "github-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory test-dir t))))

;;; ============================================================
;;; github-empty-repositories-filter-empty
;;; ============================================================

(ert-deftest test-filter-empty/empty-repos-only ()
  "defaultBranchRef が null のリポジトリだけを返す。"
  (let* ((repos `(((name . "empty-repo")
                   (defaultBranchRef))
                  ((name . "normal-repo")
                   (defaultBranchRef . ((name . "main"))))
                  ((name . "another-empty")
                   (defaultBranchRef))))
         (result (github-empty-repositories-filter-empty repos)))
    (should (= 2 (length result)))
    (should (equal "empty-repo" (alist-get 'name (nth 0 result))))
    (should (equal "another-empty" (alist-get 'name (nth 1 result))))))

(ert-deftest test-filter-empty/no-empty-repos ()
  "空リポジトリがない場合は空リストを返す。"
  (let* ((repos `(((name . "repo-a")
                   (defaultBranchRef . ((name . "main"))))
                  ((name . "repo-b")
                   (defaultBranchRef . ((name . "master"))))))
         (result (github-empty-repositories-filter-empty repos)))
    (should (= 0 (length result)))))

(ert-deftest test-filter-empty/all-empty ()
  "すべて空の場合、すべてを返す。"
  (let* ((repos `(((name . "a") (defaultBranchRef))
                  ((name . "b") (defaultBranchRef))))
         (result (github-empty-repositories-filter-empty repos)))
    (should (= 2 (length result)))))

(ert-deftest test-filter-empty/empty-list ()
  "空リストを渡すと空リストを返す。"
  (should (null (github-empty-repositories-filter-empty '()))))

;;; ============================================================
;;; github-empty-repositories-local-status
;;; ============================================================

(ert-deftest test-local-status/nonexistent-dir ()
  "存在しないディレクトリの場合 nil を返す。"
  (should (null (github-empty-repositories-local-status "/nonexistent/path/xyz"))))

(ert-deftest test-local-status/empty-directory ()
  "空ディレクトリの場合 :empty-dir を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "empty-sub" test-dir)))
      (make-directory sub)
      (should (eq :empty-dir (github-empty-repositories-local-status sub))))))

(ert-deftest test-local-status/no-git-directory ()
  "ファイルはあるが .git がないディレクトリの場合 :no-git を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "no-git-sub" test-dir)))
      (make-directory sub)
      (with-temp-file (expand-file-name "file.txt" sub)
        (insert "hello"))
      (should (eq :no-git (github-empty-repositories-local-status sub))))))

(ert-deftest test-local-status/no-commits ()
  "git init 直後 (コミットなし) の場合 :no-commits を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "no-commit-sub" test-dir)))
      (make-directory sub)
      (let ((default-directory sub))
        (call-process "git" nil nil nil "init"))
      (should (eq :no-commits (github-empty-repositories-local-status sub))))))

(ert-deftest test-local-status/has-commits ()
  "コミットがあるリポジトリの場合 :has-commits を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "has-commit-sub" test-dir)))
      (make-directory sub)
      (let ((default-directory sub))
        (call-process "git" nil nil nil "init")
        (with-temp-file (expand-file-name "file.txt" sub)
          (insert "hello"))
        (call-process "git" nil nil nil "add" ".")
        (call-process "git" nil nil nil
                      "-c" "user.name=Test"
                      "-c" "user.email=test@test.com"
                      "commit" "-m" "init"))
      (should (eq :has-commits (github-empty-repositories-local-status sub))))))

(ert-deftest test-local-status/default-dir-from-variable ()
  "引数省略時に github-variable-directory を使う。"
  (with-test-directory
    (let ((sub (expand-file-name "default-sub" test-dir)))
      (make-directory sub)
      (let ((github-variable-directory sub))
        (should (eq :empty-dir (github-empty-repositories-local-status)))))))

(ert-deftest test-local-status/nil-dir-and-nil-variable-signals-error ()
  "引数も github-variable-directory も nil の場合エラーを投げる。"
  (let ((github-variable-directory nil))
    (should-error (github-empty-repositories-local-status) :type 'error)))

;;; ============================================================
;;; github-empty-repositories-find-empty-local
;;; ============================================================

(ert-deftest test-find-empty-local/mixed-repos ()
  "様々な状態のサブディレクトリを正しく検出する。"
  (with-test-directory
    ;; 空ディレクトリ
    (make-directory (expand-file-name "empty" test-dir))
    ;; .git なし (ファイルだけ)
    (let ((no-git (expand-file-name "no-git" test-dir)))
      (make-directory no-git)
      (with-temp-file (expand-file-name "file.txt" no-git)
        (insert "hello")))
    ;; コミットなし (git init のみ)
    (let ((no-commit (expand-file-name "no-commit" test-dir)))
      (make-directory no-commit)
      (let ((default-directory no-commit))
        (call-process "git" nil nil nil "init")))
    ;; 正常なリポジトリ (コミットあり)
    (let ((normal (expand-file-name "normal" test-dir)))
      (make-directory normal)
      (let ((default-directory normal))
        (call-process "git" nil nil nil "init")
        (with-temp-file (expand-file-name "file.txt" normal)
          (insert "hello"))
        (call-process "git" nil nil nil "add" ".")
        (call-process "git" nil nil nil
                      "-c" "user.name=Test"
                      "-c" "user.email=test@test.com"
                      "commit" "-m" "init")))
    (let ((result (github-empty-repositories-find-empty-local test-dir)))
      ;; 正常リポジトリ以外の3つが検出されるべき
      (should (= 3 (length result)))
      (should (assoc "empty" result))
      (should (assoc "no-git" result))
      (should (assoc "no-commit" result))
      (should (eq :empty-dir (cdr (assoc "empty" result))))
      (should (eq :no-git (cdr (assoc "no-git" result))))
      (should (eq :no-commits (cdr (assoc "no-commit" result)))))))

(ert-deftest test-find-empty-local/no-empty-repos ()
  "空リポジトリがない場合は空リストを返す。"
  (with-test-directory
    (let ((normal (expand-file-name "normal" test-dir)))
      (make-directory normal)
      (let ((default-directory normal))
        (call-process "git" nil nil nil "init")
        (with-temp-file (expand-file-name "file.txt" normal)
          (insert "hello"))
        (call-process "git" nil nil nil "add" ".")
        (call-process "git" nil nil nil
                      "-c" "user.name=Test"
                      "-c" "user.email=test@test.com"
                      "commit" "-m" "init")))
    (should (null (github-empty-repositories-find-empty-local test-dir)))))

(ert-deftest test-find-empty-local/empty-parent-dir ()
  "サブディレクトリがない場合は空リストを返す。"
  (with-test-directory
    (should (null (github-empty-repositories-find-empty-local test-dir)))))

(ert-deftest test-find-empty-local/default-dir-from-variable ()
  "引数省略時に github-variable-directory を使う。"
  (with-test-directory
    (make-directory (expand-file-name "empty-sub" test-dir))
    (let ((github-variable-directory test-dir))
      (let ((result (github-empty-repositories-find-empty-local)))
        (should (= 1 (length result)))
        (should (equal "empty-sub" (car (nth 0 result))))))))

(ert-deftest test-find-empty-local/nil-dir-and-nil-variable-signals-error ()
  "引数も github-variable-directory も nil の場合エラーを投げる。"
  (let ((github-variable-directory nil))
    (should-error (github-empty-repositories-find-empty-local) :type 'error)))

(ert-deftest test-find-empty-local/ignores-dotfiles ()
  "ドットで始まるディレクトリは無視する。"
  (with-test-directory
    (make-directory (expand-file-name ".hidden" test-dir))
    (make-directory (expand-file-name "visible" test-dir))
    (let ((result (github-empty-repositories-find-empty-local test-dir)))
      (should (= 1 (length result)))
      (should (equal "visible" (car (nth 0 result)))))))

;;; ============================================================
;;; github-empty-repositories-find-empty-remote
;;; ============================================================

(ert-deftest test-find-empty-remote/no-token-signals-error ()
  "トークン未設定で呼び出すとエラーになる。"
  (let ((github-variable-token nil))
    (should-error
     (github-empty-repositories-find-empty-remote #'ignore)
     :type 'error)))

;;; ============================================================
;;; github-empty-repositories-display
;;; ============================================================

(ert-deftest test-display/remote-and-local ()
  "リモートとローカル両方の結果をバッファに表示する。"
  (let ((remote `(((name . "empty-remote") (isArchived . :json-false) (isFork . :json-false))))
        (local '(("empty-local" . :empty-dir)))
        (buf-name "*github-empty-repositories*"))
    ;; display-buffer をモックして実際のウィンドウ表示を抑制
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-empty-repositories-display remote local "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "空リポジトリ一覧" content))
        (should (string-match-p "GitHub 上の空リポジトリ (1 件)" content))
        (should (string-match-p "empty-remote" content))
        (should (string-match-p "ローカルの空リポジトリ" content))
        (should (string-match-p "empty-local \\[空ディレクトリ\\]" content))))
    (kill-buffer buf-name)))

(ert-deftest test-display/no-remote ()
  "リモート結果が nil (ローカルのみチェック) の場合。"
  (let ((local '(("no-git-repo" . :no-git)))
        (buf-name "*github-empty-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-empty-repositories-display nil local "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "GitHub 上の空リポジトリ (0 件)" content))
        (should (string-match-p "(なし)" content))
        (should (string-match-p "no-git-repo \\[\\.git なし\\]" content))))
    (kill-buffer buf-name)))

(ert-deftest test-display/no-results ()
  "リモートもローカルも空の場合。"
  (let ((buf-name "*github-empty-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-empty-repositories-display nil nil "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "(0 件)" content))
        (should (string-match-p "(なし)" content))))
    (kill-buffer buf-name)))

(ert-deftest test-display/both-remote-and-local-empty ()
  "リモートとローカル両方で空のリポジトリがある場合、交差セクションを表示する。"
  (let ((remote `(((name . "shared-empty") (isArchived . :json-false) (isFork . :json-false))))
        (local '(("shared-empty" . :no-commits)))
        (buf-name "*github-empty-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-empty-repositories-display remote local "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "リモート・ローカル両方で空 (1 件)" content))
        (should (string-match-p "shared-empty" content))))
    (kill-buffer buf-name)))

(ert-deftest test-display/archived-and-fork-labels ()
  "アーカイブ済み・フォークのラベルが表示される。"
  (let ((remote `(((name . "archived-repo") (isArchived . t) (isFork . :json-false))
                  ((name . "fork-repo") (isArchived . :json-false) (isFork . t))))
        (buf-name "*github-empty-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-empty-repositories-display remote nil "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "archived-repo \\[archived\\]" content))
        (should (string-match-p "fork-repo \\[fork\\]" content))))
    (kill-buffer buf-name)))

(ert-deftest test-display/local-status-labels ()
  "ローカルの各ステータスラベルが正しく表示される。"
  (let ((local '(("a" . :empty-dir) ("b" . :no-git) ("c" . :no-commits)))
        (buf-name "*github-empty-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-empty-repositories-display nil local "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "a \\[空ディレクトリ\\]" content))
        (should (string-match-p "b \\[\\.git なし\\]" content))
        (should (string-match-p "c \\[コミットなし\\]" content))))
    (kill-buffer buf-name)))

(provide 'test-github-empty-repositories)
;;; test-github-empty-repositories.el ends here

;;; test-github-behind-repositories.el --- Tests for github-behind-repositories.el -*- lexical-binding: t; -*-

;;; Commentary:

;; github-behind-repositories.el のユニットテスト。
;; ファイルシステム操作は一時ディレクトリを使ってテストする。

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((src-dir (expand-file-name "../src" (file-name-directory load-file-name))))
  (add-to-list 'load-path src-dir))

(require 'github-variables)
(require 'github-behind-repositories)

;;; ============================================================
;;; ヘルパー: テスト用一時ディレクトリの作成
;;; ============================================================

(defmacro with-test-directory (&rest body)
  "一時ディレクトリを作成し BODY を実行後、削除する。
BODY 内では `test-dir' で一時ディレクトリのパスを参照できる。"
  (declare (indent 0) (debug t))
  `(let ((test-dir (make-temp-file "github-behind-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory test-dir t))))

(defun test-behind--create-repo-with-upstream (dir)
  "DIR にリモート付きの git リポジトリを作成するヘルパー。
bare リポジトリを origin として設定し、1コミットを作成する。"
  (let* ((bare-dir (concat dir "-bare"))
         (default-directory dir))
    ;; bare リポジトリを作成
    (make-directory bare-dir t)
    (let ((default-directory bare-dir))
      (call-process "git" nil nil nil "init" "--bare"))
    ;; ワークリポジトリを作成
    (make-directory dir t)
    (call-process "git" nil nil nil "init")
    (call-process "git" nil nil nil "remote" "add" "origin" bare-dir)
    ;; 初期コミット
    (with-temp-file (expand-file-name "file.txt" dir)
      (insert "hello"))
    (call-process "git" nil nil nil "add" ".")
    (call-process "git" nil nil nil
                  "-c" "user.name=Test"
                  "-c" "user.email=test@test.com"
                  "commit" "-m" "init")
    ;; デフォルトブランチ名を取得して push
    (let ((branch (string-trim
                   (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "git" nil t nil
                                     "rev-parse" "--abbrev-ref" "HEAD"))))))
      (call-process "git" nil nil nil "push" "-u" "origin" branch))
    bare-dir))

;;; ============================================================
;;; github-behind-repositories--parse-status
;;; ============================================================

(ert-deftest test-behind-parse/not-git-repo ()
  "git リポジトリでないディレクトリは nil を返す。"
  (with-test-directory
    (should (null (github-behind-repositories--parse-status test-dir)))))

(ert-deftest test-behind-parse/no-upstream ()
  "upstream が設定されていないリポジトリは nil を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "no-upstream" test-dir)))
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
      (should (null (github-behind-repositories--parse-status sub))))))

(ert-deftest test-behind-parse/up-to-date ()
  "リモートと同期済みのリポジトリは behind=0 を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "synced" test-dir)))
      (make-directory sub)
      (let ((bare-dir (test-behind--create-repo-with-upstream sub)))
        (unwind-protect
            (let ((status (github-behind-repositories--parse-status sub)))
              (should (not (null status)))
              (should (= 0 (alist-get 'behind status)))
              (should (= 0 (alist-get 'ahead status)))
              (should (stringp (alist-get 'branch status)))
              (should (stringp (alist-get 'upstream status))))
          (delete-directory bare-dir t))))))

(ert-deftest test-behind-parse/behind-upstream ()
  "リモートより遅れているリポジトリは behind>0 を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "behind" test-dir)))
      (make-directory sub)
      (let* ((bare-dir (test-behind--create-repo-with-upstream sub))
             ;; bare にもう一つコミットを追加するため、別の clone を作成
             (clone2 (expand-file-name "clone2" test-dir)))
        (unwind-protect
            (progn
              ;; 別 clone でコミットして push
              (call-process "git" nil nil nil "clone" bare-dir clone2)
              (let ((default-directory clone2))
                (with-temp-file (expand-file-name "file2.txt" clone2)
                  (insert "world"))
                (call-process "git" nil nil nil "add" ".")
                (call-process "git" nil nil nil
                              "-c" "user.name=Test"
                              "-c" "user.email=test@test.com"
                              "commit" "-m" "second")
                (call-process "git" nil nil nil "push"))
              ;; sub で fetch して behind を検出
              (let ((default-directory sub))
                (call-process "git" nil nil nil "fetch"))
              (let ((status (github-behind-repositories--parse-status sub)))
                (should (not (null status)))
                (should (= 1 (alist-get 'behind status)))
                (should (= 0 (alist-get 'ahead status)))))
          (delete-directory bare-dir t)
          (delete-directory clone2 t))))))

(ert-deftest test-behind-parse/ahead-of-upstream ()
  "リモートより先行しているリポジトリは ahead>0 を返す。"
  (with-test-directory
    (let ((sub (expand-file-name "ahead" test-dir)))
      (make-directory sub)
      (let ((bare-dir (test-behind--create-repo-with-upstream sub)))
        (unwind-protect
            (progn
              ;; ローカルでコミットするが push しない
              (let ((default-directory sub))
                (with-temp-file (expand-file-name "file2.txt" sub)
                  (insert "world"))
                (call-process "git" nil nil nil "add" ".")
                (call-process "git" nil nil nil
                              "-c" "user.name=Test"
                              "-c" "user.email=test@test.com"
                              "commit" "-m" "local-only"))
              (let ((status (github-behind-repositories--parse-status sub)))
                (should (not (null status)))
                (should (= 0 (alist-get 'behind status)))
                (should (= 1 (alist-get 'ahead status)))))
          (delete-directory bare-dir t))))))

;;; ============================================================
;;; github-behind-repositories-local-status
;;; ============================================================

(ert-deftest test-behind-local-status/nonexistent-dir ()
  "存在しないディレクトリの場合 nil を返す。"
  (should (null (github-behind-repositories-local-status "/nonexistent/path/xyz"))))

(ert-deftest test-behind-local-status/default-dir-from-variable ()
  "引数省略時に github-variable-directory を使う。"
  (with-test-directory
    (let ((github-variable-directory test-dir))
      ;; git リポジトリでないので nil が返る
      (should (null (github-behind-repositories-local-status))))))

(ert-deftest test-behind-local-status/nil-dir-and-nil-variable-signals-error ()
  "引数も github-variable-directory も nil の場合エラーを投げる。"
  (let ((github-variable-directory nil))
    (should-error (github-behind-repositories-local-status) :type 'error)))

;;; ============================================================
;;; github-behind-repositories-find-behind-local
;;; ============================================================

(ert-deftest test-behind-find/no-behind-repos ()
  "遅れているリポジトリがない場合は空リストを返す。"
  (with-test-directory
    (let ((sub (expand-file-name "synced" test-dir)))
      (make-directory sub)
      (let ((bare-dir (test-behind--create-repo-with-upstream sub)))
        (unwind-protect
            (should (null (github-behind-repositories-find-behind-local test-dir)))
          (delete-directory bare-dir t))))))

(ert-deftest test-behind-find/with-behind-repos ()
  "遅れているリポジトリがある場合はリストに含まれる。"
  (with-test-directory
    (let ((sub (expand-file-name "behind-repo" test-dir)))
      (make-directory sub)
      (let* ((bare-dir (test-behind--create-repo-with-upstream sub))
             (clone2 (expand-file-name "clone2" test-dir)))
        (unwind-protect
            (progn
              ;; 別 clone でコミットして push
              (call-process "git" nil nil nil "clone" bare-dir clone2)
              (let ((default-directory clone2))
                (with-temp-file (expand-file-name "file2.txt" clone2)
                  (insert "world"))
                (call-process "git" nil nil nil "add" ".")
                (call-process "git" nil nil nil
                              "-c" "user.name=Test"
                              "-c" "user.email=test@test.com"
                              "commit" "-m" "second")
                (call-process "git" nil nil nil "push"))
              ;; sub で fetch
              (let ((default-directory sub))
                (call-process "git" nil nil nil "fetch"))
              (let ((result (github-behind-repositories-find-behind-local test-dir)))
                (should (= 1 (length result)))
                (should (equal "behind-repo" (car (nth 0 result))))
                (should (= 1 (alist-get 'behind (cdr (nth 0 result)))))))
          (delete-directory bare-dir t)
          (delete-directory clone2 t))))))

(ert-deftest test-behind-find/default-dir-from-variable ()
  "引数省略時に github-variable-directory を使う。"
  (with-test-directory
    (let ((github-variable-directory test-dir))
      (should (null (github-behind-repositories-find-behind-local))))))

(ert-deftest test-behind-find/nil-dir-and-nil-variable-signals-error ()
  "引数も github-variable-directory も nil の場合エラーを投げる。"
  (let ((github-variable-directory nil))
    (should-error (github-behind-repositories-find-behind-local) :type 'error)))

(ert-deftest test-behind-find/ignores-non-git-dirs ()
  "git リポジトリでないディレクトリは無視する。"
  (with-test-directory
    (make-directory (expand-file-name "not-a-repo" test-dir))
    (should (null (github-behind-repositories-find-behind-local test-dir)))))

;;; ============================================================
;;; github-behind-repositories-display
;;; ============================================================

(ert-deftest test-behind-display/with-repos ()
  "遅れているリポジトリがある場合、バッファに正しく表示する。"
  (let ((repos '(("my-repo" . ((behind . 3) (ahead . 0) (branch . "main") (upstream . "origin/main")))))
        (buf-name "*github-behind-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-behind-repositories-display repos "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "pull が必要なリポジトリ一覧" content))
        (should (string-match-p "1 件のリポジトリが遅れています" content))
        (should (string-match-p "my-repo" content))
        (should (string-match-p "main → origin/main" content))
        (should (string-match-p "遅れ: 3 コミット" content))))
    (kill-buffer buf-name)))

(ert-deftest test-behind-display/with-ahead ()
  "ahead がある場合、先行情報も表示する。"
  (let ((repos '(("my-repo" . ((behind . 2) (ahead . 1) (branch . "main") (upstream . "origin/main")))))
        (buf-name "*github-behind-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-behind-repositories-display repos "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "遅れ: 2 コミット / 先行: 1 コミット" content))))
    (kill-buffer buf-name)))

(ert-deftest test-behind-display/no-repos ()
  "遅れているリポジトリがない場合、適切なメッセージを表示する。"
  (let ((buf-name "*github-behind-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-behind-repositories-display nil "/tmp/test"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "すべてのリポジトリは最新です" content))))
    (kill-buffer buf-name)))

(ert-deftest test-behind-display/shows-target-dir ()
  "対象ディレクトリがバッファに表示される。"
  (let ((buf-name "*github-behind-repositories*"))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-behind-repositories-display nil "/home/user/repos"))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "/home/user/repos" content))))
    (kill-buffer buf-name)))

;;; ============================================================
;;; github-behind-repositories (インタラクティブコマンド)
;;; ============================================================

(ert-deftest test-behind-command/nil-dir-and-nil-variable-signals-error ()
  "引数も github-variable-directory も nil の場合エラーを投げる。"
  (let ((github-variable-directory nil))
    ;; read-directory-name のプロンプトを避けるためモック
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) nil)))
      (should-error (github-behind-repositories) :type 'error))))

(provide 'test-github-behind-repositories)
;;; test-github-behind-repositories.el ends here

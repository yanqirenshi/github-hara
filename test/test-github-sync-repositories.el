;;; test-github-sync-repositories.el --- Tests for github-sync-repositories.el -*- lexical-binding: t; -*-

;;; Commentary:

;; github-sync-repositories.el のユニットテスト。
;; 非同期処理 (API / git clone) はモックを使ってテストする。

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((src-dir (expand-file-name "../src" (file-name-directory load-file-name))))
  (add-to-list 'load-path src-dir))

(require 'github-variables)
(require 'github-api)
(require 'github-sync-repositories)

;;; ============================================================
;;; ヘルパー
;;; ============================================================

(defmacro with-test-directory (&rest body)
  "一時ディレクトリを作成し BODY を実行後、削除する。
BODY 内では `test-dir' で一時ディレクトリのパスを参照できる。"
  (declare (indent 0) (debug t))
  `(let ((test-dir (make-temp-file "github-sync-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory test-dir t))))

;;; ============================================================
;;; State management (cl-defstruct)
;;; ============================================================

(ert-deftest test-sync-state/create ()
  "状態構造体を正しく作成できる。"
  (let ((state (github-sync-repositories--state-create
                :target-dir "/tmp/test"
                :repos '(a b c)
                :pending '(a b c)
                :active 0
                :results '()
                :total 3)))
    (should (equal "/tmp/test" (github-sync-repositories--state-target-dir state)))
    (should (= 3 (github-sync-repositories--state-total state)))
    (should (= 0 (github-sync-repositories--state-active state)))
    (should (null (github-sync-repositories--state-results state)))))

(ert-deftest test-sync-state/mutate ()
  "状態構造体のフィールドを変更できる。"
  (let ((state (github-sync-repositories--state-create
                :active 0
                :results '()
                :total 2)))
    (cl-incf (github-sync-repositories--state-active state))
    (should (= 1 (github-sync-repositories--state-active state)))
    (push '("repo" . :cloned) (github-sync-repositories--state-results state))
    (should (= 1 (length (github-sync-repositories--state-results state))))))

;;; ============================================================
;;; Log buffer
;;; ============================================================

(ert-deftest test-sync-log/writes-to-buffer ()
  "ログメッセージがバッファに書き込まれる。"
  (let ((buf-name "*github-sync-repositories*"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (github-sync-repositories--log "Test: %d repos" 5)
    (with-current-buffer buf-name
      (should (string-match-p "Test: 5 repos" (buffer-string))))
    (kill-buffer buf-name)))

;;; ============================================================
;;; Clone URL selection
;;; ============================================================

(ert-deftest test-clone-repo/ssh-url-selected ()
  "github-variable-use-ssh が t なら sshUrl を使う。"
  (with-test-directory
    (let* ((github-variable-use-ssh t)
           (github-variable-max-parallel 4)
           (repo `((name . "test-repo")
                   (sshUrl . "git@github.com:user/test-repo.git")
                   (url . "https://github.com/user/test-repo")))
           (state (github-sync-repositories--state-create
                   :target-dir test-dir
                   :repos (list repo)
                   :pending (list repo)
                   :active 0
                   :results '()
                   :total 1))
           (called-with-url nil))
      ;; make-process をモックして実際の git clone を防ぐ
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq called-with-url (nth 2 (plist-get args :command)))
                   ;; 即座に結果を記録
                   (push (cons "test-repo" :cloned)
                         (github-sync-repositories--state-results state))
                   nil))
                ((symbol-function 'github-sync-repositories--maybe-finish)
                 #'ignore))
        (github-sync-repositories--clone-repo-async repo state)
        (should (equal "git@github.com:user/test-repo.git" called-with-url))))))

(ert-deftest test-clone-repo/https-url-selected ()
  "github-variable-use-ssh が nil なら url (HTTPS) を使う。"
  (with-test-directory
    (let* ((github-variable-use-ssh nil)
           (github-variable-max-parallel 4)
           (repo `((name . "test-repo")
                   (sshUrl . "git@github.com:user/test-repo.git")
                   (url . "https://github.com/user/test-repo")))
           (state (github-sync-repositories--state-create
                   :target-dir test-dir
                   :repos (list repo)
                   :pending (list repo)
                   :active 0
                   :results '()
                   :total 1))
           (called-with-url nil))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq called-with-url (nth 2 (plist-get args :command)))
                   (push (cons "test-repo" :cloned)
                         (github-sync-repositories--state-results state))
                   nil))
                ((symbol-function 'github-sync-repositories--maybe-finish)
                 #'ignore))
        (github-sync-repositories--clone-repo-async repo state)
        (should (equal "https://github.com/user/test-repo" called-with-url))))))

;;; ============================================================
;;; Skip already cloned
;;; ============================================================

(ert-deftest test-clone-repo/skip-already-cloned ()
  "clone 済みのリポジトリはスキップし :skipped を記録する。"
  (with-test-directory
    (let* ((repo-dir (expand-file-name "existing-repo" test-dir))
           (git-dir (expand-file-name ".git" repo-dir)))
      (make-directory git-dir t)
      (let* ((repo `((name . "existing-repo")))
             (state (github-sync-repositories--state-create
                     :target-dir test-dir
                     :repos (list repo)
                     :pending nil
                     :active 0
                     :results '()
                     :total 1)))
        (cl-letf (((symbol-function 'github-sync-repositories--maybe-finish) #'ignore))
          (github-sync-repositories--clone-repo-async repo state)
          (should (= 1 (length (github-sync-repositories--state-results state))))
          (should (eq :skipped
                      (cdr (car (github-sync-repositories--state-results state))))))))))

;;; ============================================================
;;; Queue management
;;; ============================================================

(ert-deftest test-start-clone-queue/respects-max-parallel ()
  "並列数の上限を超えてプロセスを起動しない。"
  (with-test-directory
    (let* ((github-variable-max-parallel 2)
           (github-variable-use-ssh t)
           (repos (cl-loop for i from 1 to 5
                           collect `((name . ,(format "repo-%d" i))
                                     (sshUrl . ,(format "git@github.com:user/repo-%d.git" i)))))
           (state (github-sync-repositories--state-create
                   :target-dir test-dir
                   :repos repos
                   :pending (copy-sequence repos)
                   :active 0
                   :results '()
                   :total 5))
           (process-count 0))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _args)
                   (cl-incf process-count)
                   (cl-incf (github-sync-repositories--state-active state))
                   nil)))
        (github-sync-repositories--start-clone-queue state)
        ;; 最大 2 プロセスしか起動されないこと
        (should (= 2 process-count))
        (should (= 2 (github-sync-repositories--state-active state)))))))

;;; ============================================================
;;; Show summary
;;; ============================================================

(ert-deftest test-show-summary/counts ()
  "サマリーに cloned / skipped / failed の件数が表示される。"
  (let ((buf-name "*github-sync-repositories*")
        (state (github-sync-repositories--state-create
                :total 4
                :results '(("a" . :cloned)
                           ("b" . :skipped)
                           ("c" . :cloned)
                           ("d" . :failed)))))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-sync-repositories--show-summary state))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "Cloned: 2" content))
        (should (string-match-p "Skipped: 1" content))
        (should (string-match-p "Failed: 1" content))))
    ;; current-state がクリアされていること
    (should (null github-sync-repositories--current-state))
    (kill-buffer buf-name)))

(ert-deftest test-show-summary/failed-list ()
  "失敗したリポジトリ名がサマリーに表示される。"
  (let ((buf-name "*github-sync-repositories*")
        (state (github-sync-repositories--state-create
                :total 2
                :results '(("ok-repo" . :cloned)
                           ("bad-repo" . :failed)))))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (github-sync-repositories--show-summary state))
    (with-current-buffer buf-name
      (let ((content (buffer-string)))
        (should (string-match-p "Failed repositories:" content))
        (should (string-match-p "bad-repo" content))))
    (kill-buffer buf-name)))

;;; ============================================================
;;; Interactive command guards
;;; ============================================================

(ert-deftest test-sync-repositories/no-token-signals-error ()
  "トークン未設定で呼び出すとエラーになる。"
  (with-test-directory
    (let ((github-variable-token nil)
          (github-sync-repositories--current-state nil))
      (should-error (github-sync-repositories test-dir) :type 'error))))

(ert-deftest test-sync-repositories/already-running-signals-error ()
  "既に実行中のときエラーになる。"
  (with-test-directory
    (let ((github-variable-token "ghp_test")
          (github-sync-repositories--current-state
           (github-sync-repositories--state-create)))
      (should-error (github-sync-repositories test-dir) :type 'error))))

(ert-deftest test-sync-cancel/not-running-signals-error ()
  "実行中でないときキャンセルするとエラーになる。"
  (let ((github-sync-repositories--current-state nil))
    (should-error (github-sync-repositories-cancel) :type 'error)))

(provide 'test-github-sync-repositories)
;;; test-github-sync-repositories.el ends here

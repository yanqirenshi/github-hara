;;; test-github-api.el --- Tests for github-api.el -*- lexical-binding: t; -*-

;;; Commentary:

;; github-api.el のユニットテスト。

;;; Code:

(require 'ert)

(let ((src-dir (expand-file-name "../src" (file-name-directory load-file-name))))
  (add-to-list 'load-path src-dir))

(require 'github-variables)
(require 'github-api)

;;; ============================================================
;;; github-api--ensure-token
;;; ============================================================

(ert-deftest test-github-api--ensure-token/nil-token-signals-error ()
  "トークンが nil のときエラーを投げる。"
  (let ((github-variable-token nil))
    (should-error (github-api--ensure-token) :type 'error)))

(ert-deftest test-github-api--ensure-token/valid-token-succeeds ()
  "トークンが設定されていればエラーにならない。"
  (let ((github-variable-token "ghp_test123"))
    (should (eq nil (github-api--ensure-token)))))

;;; ============================================================
;;; github-api--parse-response
;;; ============================================================

(ert-deftest test-github-api--parse-response/valid-json ()
  "正常な JSON レスポンスをパースできる。"
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
    (insert (json-encode '((data . ((viewer . ((login . "testuser"))))))))
    (let ((result (github-api--parse-response (current-buffer))))
      (should (equal "testuser"
                     (alist-get 'login
                                (alist-get 'viewer
                                           (alist-get 'data result))))))))

(ert-deftest test-github-api--parse-response/api-error ()
  "errors フィールドがある場合エラーを投げる。"
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\n\n")
    (insert (json-encode '((errors . [((message . "Bad credentials"))]))))
    (should-error (github-api--parse-response (current-buffer)) :type 'error)))

;;; ============================================================
;;; github-api--graphql-request-async
;;; ============================================================

(ert-deftest test-github-api--graphql-request-async/no-token-signals-error ()
  "トークン未設定で呼び出すとエラーになる。"
  (let ((github-variable-token nil))
    (should-error
     (github-api--graphql-request-async "query { viewer { login } }" nil #'ignore)
     :type 'error)))

(provide 'test-github-api)
;;; test-github-api.el ends here

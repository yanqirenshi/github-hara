;;; test-github-variables.el --- Tests for github-variables.el -*- lexical-binding: t; -*-

;;; Commentary:

;; github-variables.el のユニットテスト。

;;; Code:

(require 'ert)

(let ((src-dir (expand-file-name "../src" (file-name-directory load-file-name))))
  (add-to-list 'load-path src-dir))

(require 'github-variables)

;;; ============================================================
;;; defcustom のデフォルト値
;;; ============================================================

(ert-deftest test-variables/token-default-nil ()
  "github-variable-token のデフォルトは nil。"
  (should (custom-variable-p 'github-variable-token))
  (should (eq nil (eval (car (get 'github-variable-token 'standard-value))))))

(ert-deftest test-variables/directory-default-nil ()
  "github-variable-directory のデフォルトは nil。"
  (should (custom-variable-p 'github-variable-directory))
  (should (eq nil (eval (car (get 'github-variable-directory 'standard-value))))))

(ert-deftest test-variables/use-ssh-default-t ()
  "github-variable-use-ssh のデフォルトは t。"
  (should (custom-variable-p 'github-variable-use-ssh))
  (should (eq t (eval (car (get 'github-variable-use-ssh 'standard-value))))))

(ert-deftest test-variables/max-parallel-default-4 ()
  "github-variable-max-parallel のデフォルトは 4。"
  (should (custom-variable-p 'github-variable-max-parallel))
  (should (= 4 (eval (car (get 'github-variable-max-parallel 'standard-value))))))

;;; ============================================================
;;; defgroup
;;; ============================================================

(ert-deftest test-variables/group-exists ()
  "github グループが定義されている。"
  (should (get 'github 'custom-group)))

;;; ============================================================
;;; 値の設定・参照
;;; ============================================================

(ert-deftest test-variables/token-can-be-set ()
  "github-variable-token に値を設定して参照できる。"
  (let ((github-variable-token "ghp_test_token"))
    (should (equal "ghp_test_token" github-variable-token))))

(ert-deftest test-variables/directory-can-be-set ()
  "github-variable-directory に値を設定して参照できる。"
  (let ((github-variable-directory "~/my-repos/"))
    (should (equal "~/my-repos/" github-variable-directory))))

(ert-deftest test-variables/use-ssh-can-be-toggled ()
  "github-variable-use-ssh を切り替えできる。"
  (let ((github-variable-use-ssh nil))
    (should (eq nil github-variable-use-ssh)))
  (let ((github-variable-use-ssh t))
    (should (eq t github-variable-use-ssh))))

(ert-deftest test-variables/max-parallel-can-be-set ()
  "github-variable-max-parallel に値を設定して参照できる。"
  (let ((github-variable-max-parallel 8))
    (should (= 8 github-variable-max-parallel))))

(provide 'test-github-variables)
;;; test-github-variables.el ends here

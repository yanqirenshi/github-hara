;;; init.el --- github.el パッケージのエントリポイント -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; このファイルが github.el のソースコードルートとなる。
;; Emacs の init.el から (load "~/.emacs.d/dist/github.el/init.el") で読み込む。

;;; Code:

;; 共通変数
(use-package github-variables
  :ensure nil
  :load-path "~/.emacs.d/dist/github.el/src/"
  :custom
  (github-variable-token (auth-source-pick-first-password :host "api.github.com"))
  (github-variable-directory "~/repos/")
  (github-variable-use-ssh t)
  (github-variable-max-parallel 4))

;; リポジトリ同期機能
(use-package github-sync-repositories
  :ensure nil
  :load-path "~/.emacs.d/dist/github.el/src/"
  :commands (github-sync-repositories
             github-sync-repositories-list
             github-sync-repositories-cancel))

;; 空リポジトリ検出機能
(use-package github-empty-repositories
  :ensure nil
  :load-path "~/.emacs.d/dist/github.el/src/"
  :commands (github-empty-repositories
             github-empty-repositories-local))

;; pull が必要なリポジトリ検出機能
(use-package github-behind-repositories
  :ensure nil
  :load-path "~/.emacs.d/dist/github.el/src/"
  :commands (github-behind-repositories))

;;; init.el ends here

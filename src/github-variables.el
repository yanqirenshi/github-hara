;;; github-variables.el --- Shared variables for github.el package -*- lexical-binding: t; -*-

;; Author: yanqirenshi
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: github tools vc
;; URL: https://github.com/yanqirenshi/github.el

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

;; github.el パッケージ全体で共有するカスタマイズ変数を定義する。
;; 各機能モジュール (github-clone-all 等) からこのファイルを require して使う。

;;; Code:

(defgroup github nil
  "GitHub 操作のための Emacs パッケージ。"
  :group 'tools
  :prefix "github-variable-"
  :link '(url-link "https://github.com/yanqirenshi/github.el"))

(defcustom github-variable-token nil
  "GitHub personal access token.
`repo' スコープが必要。"
  :type '(choice (const nil) string)
  :group 'github)

(defcustom github-variable-directory nil
  "Default directory for repository operations.
`github-clone-all' ではクローン先として使われる。"
  :type '(choice (const nil) directory)
  :group 'github)

(defcustom github-variable-use-ssh t
  "If non-nil, use SSH URL.  Otherwise use HTTPS URL."
  :type 'boolean
  :group 'github)

(defcustom github-variable-max-parallel 4
  "Maximum number of parallel processes."
  :type 'integer
  :group 'github)

(provide 'github-variables)
;;; github-variables.el ends here

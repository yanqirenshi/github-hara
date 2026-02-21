# github-sitter

GitHub を操作するための Emacs パッケージ。

GitHub GraphQL API (v4) を使い、リポジトリの同期や空リポジトリの検出などの機能を提供する。すべての API 通信は非同期で実行されるため、Emacs を通常通り操作できる。

## 必要なもの

- Emacs 25.1 以上
- Git
- GitHub パーソナルアクセストークン (`repo` スコープ)

## ディレクトリ構造

```
github.el/
├── init.el                              # エントリポイント (use-package 設定)
├── src/
│   ├── github-api.el                    # GraphQL API 共通基盤
│   ├── github-variables.el              # 共通変数 (github-variable-*)
│   ├── github-sync-repositories.el      # リポジトリ同期機能
│   ├── github-empty-repositories.el     # 空リポジトリ検出機能
│   ├── github-behind-repositories.el    # pull が必要なリポジトリ検出機能
│   └── github-orphan-repositories.el    # 孤立リポジトリ検出機能
├── README.md
└── LICENSE
```

## インストール

### init.el から読み込む (推奨)

`github.el/init.el` をエントリポイントとして読み込む。

```elisp
;; ~/.emacs.d/init.el に追加
(load "~/.emacs.d/dist/github.el/init.el")
```

`init.el` の中身:

```elisp
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

;; 孤立リポジトリ検出機能
(use-package github-orphan-repositories
  :ensure nil
  :load-path "~/.emacs.d/dist/github.el/src/"
  :commands (github-orphan-repositories))
```

### require を使う場合

```elisp
(add-to-list 'load-path "~/.emacs.d/dist/github.el/src/")
(require 'github-variables)
(require 'github-api)
(require 'github-sync-repositories)
(require 'github-empty-repositories)
(require 'github-behind-repositories)
(require 'github-orphan-repositories)
```

## 設定

共通変数はすべて `github-variable-*` という名前で `github-variables.el` に定義されている。

### トークン

`~/.authinfo.gpg` (または `~/.authinfo`) に以下の行を追加する:

```
machine api.github.com password ghp_xxxxxxxxxxxx
```

`use-package` の `:custom` で `auth-source-pick-first-password` を使うことで、起動時に自動取得される。

```elisp
(github-variable-token (auth-source-pick-first-password :host "api.github.com"))
```

**注意**: トークンをバージョン管理にコミットしないこと。`~/.authinfo.gpg` で GPG 暗号化して管理することを推奨する。

直接設定する場合:

```elisp
(setq github-variable-token "ghp_xxxxxxxxxxxx")
```

### 操作先ディレクトリ

```elisp
(setq github-variable-directory "~/repos/")
```

設定しておくと各コマンド実行時にミニバッファで入力せずにそのまま使われる。未設定の場合は毎回ミニバッファで入力を求められる。

### SSH / HTTPS の切り替え

```elisp
;; SSH (デフォルト)
(setq github-variable-use-ssh t)

;; HTTPS
(setq github-variable-use-ssh nil)
```

### 並列数

```elisp
;; 同時に実行するプロセスの最大数 (デフォルト: 4)
(setq github-variable-max-parallel 4)
```

## 使い方

### リポジトリを同期

```
M-x github-sync-repositories
```

自分がオーナーのリポジトリを指定ディレクトリに同期する。

- 未 clone のリポジトリ → git clone を実行
- clone 済みのリポジトリ → スキップ (何もしない)

進捗は `*github-sync-repositories*` バッファとミニバッファにリアルタイム表示される。

### リポジトリ一覧を確認

```
M-x github-sync-repositories-list
```

同期せずにリポジトリ一覧を `*github-repos*` バッファに表示する。アーカイブ済み・フォークにはラベルが付く。

### 同期をキャンセル

```
M-x github-sync-repositories-cancel
```

実行中の同期処理をすべて中止する。

### 空リポジトリを検出

```
M-x github-empty-repositories
```

GitHub 上とローカルディレクトリの両方で空リポジトリを検出し、`*github-empty-repositories*` バッファに一覧表示する。

**GitHub 上の空リポジトリ**: コミットが一つもないリポジトリ (`defaultBranchRef` が null)。

**ローカルの空リポジトリ**: 以下の3パターンを検出する。
- `[空ディレクトリ]` — ディレクトリ内にファイルがない
- `[.git なし]` — ディレクトリは存在するが git リポジトリではない
- `[コミットなし]` — `.git` は存在するが HEAD が解決できない

リモートとローカルの両方で空のリポジトリがある場合は、別セクションにも表示される。

### ローカルのみ空リポジトリを検出

```
M-x github-empty-repositories-local
```

ローカルディレクトリのみをチェックする。GitHub API を使用しないため、トークンが不要。

### pull が必要なリポジトリを検出

```
M-x github-behind-repositories
```

ローカルにクローン済みのリポジトリのうち、リモート追跡ブランチより遅れている (pull が必要な) ものを検出し、`*github-behind-repositories*` バッファに一覧表示する。

各リポジトリについて以下の情報を表示する:
- ブランチ名と追跡ブランチ名
- リモートより遅れているコミット数
- リモートより先行しているコミット数 (ある場合)

**注意**: `git fetch` を実行しない場合、ローカルにキャッシュされたリモート情報に基づく判定となる。最新のリモート状態を反映するには、事前に `git fetch --all` を実行すること。

GitHub API は使用しないため、トークンが不要。

### 孤立リポジトリを検出

```
M-x github-orphan-repositories
```

ローカルにクローン済みだが、GitHub 上にもう存在しないリポジトリ (削除済み・移譲済みなど) を検出し、`*github-orphan-repositories*` バッファに一覧表示する。

GitHub GraphQL API から自分がオーナーのリポジトリ名一覧を取得し、ローカルの git リポジトリと比較する。

**注意**: `ownerAffiliations: [OWNER]` でフィルタしているため、コラボレータ権限のみのリポジトリや Organization リポジトリは GitHub 側に含まれない。これらは孤立と誤判定される可能性がある。

## カスタマイズ変数

共通変数 (`github-variables.el`):

| 変数 | デフォルト | 説明 |
|------|-----------|------|
| `github-variable-token` | `nil` | GitHub パーソナルアクセストークン |
| `github-variable-directory` | `nil` | 操作先ディレクトリ |
| `github-variable-use-ssh` | `t` | `t` なら SSH、`nil` なら HTTPS |
| `github-variable-max-parallel` | `4` | 同時実行するプロセスの最大数 |

## モジュール構成

| モジュール | 説明 |
|------------|------|
| `github-variables` | 全機能で共有する defcustom 変数 |
| `github-api` | GraphQL API の非同期リクエスト基盤 |
| `github-sync-repositories` | リポジトリ同期 (clone 済みスキップ、未 clone を clone) |
| `github-empty-repositories` | 空リポジトリ検出 (GitHub + ローカル) |
| `github-behind-repositories` | pull が必要なリポジトリ検出 (ローカル) |
| `github-orphan-repositories` | 孤立リポジトリ検出 (GitHub + ローカル比較) |

## 開発者向け

### Claude Code スキル

このプロジェクトには Claude Code のカスタムスキル (スラッシュコマンド) が含まれる。

#### /update-operators

`OPERATORS.md` (オペレータマニュアル) をソースコードから自動生成・更新する。

```
/update-operators                                           # src/ 内の全ファイルを走査して更新
/update-operators src/github-api.el                         # 指定ファイルのみ更新
/update-operators github-empty-repositories-filter-empty    # 指定関数のみ更新
```

ソースコード内の `defun`, `defcustom`, `defvar`, `defconst`, `cl-defstruct` を解析し、Common Lisp HyperSpec スタイルのドキュメントを OPERATORS.md に反映する。新規関数は追記、既存は更新、削除された関数は除去される。

### テスト

`test/` ディレクトリに ERT (Emacs Lisp Regression Testing) によるユニットテストがある。

```elisp
;; Emacs 内で全テストを実行
(dolist (f '("test/test-github-variables.el"
             "test/test-github-api.el"
             "test/test-github-empty-repositories.el"
             "test/test-github-sync-repositories.el"
             "test/test-github-behind-repositories.el"
             "test/test-github-orphan-repositories.el"))
  (load (expand-file-name f "~/.emacs.d/dist/github.el/")))
(ert-run-tests-interactively t)
```

### ドキュメント

| ファイル | 内容 |
|----------|------|
| `README.md` | ユーザー向けドキュメント |
| `OPERATORS.md` | オペレータ (関数・変数) マニュアル |
| `CLAUDE.md` | Claude Code への指示 |

## ライセンス

GPL-3.0

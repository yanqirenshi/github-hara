# github-hara

GitHub を操作するための Emacs パッケージ。

現在は GitHub GraphQL API (v4) を使ったリポジトリの同期機能を提供する。未 clone のリポジトリを clone し、既に clone 済みのリポジトリはスキップする。すべての処理は非同期で実行されるため、Emacs を通常通り操作できる。

## 必要なもの

- Emacs 25.1 以上
- Git
- GitHub パーソナルアクセストークン (`repo` スコープ)

## ディレクトリ構造

```
github.el/
├── init.el                          # エントリポイント (use-package 設定)
├── src/
│   ├── github-variables.el          # 共通変数 (github-variable-*)
│   └── github-sync-repositories.el  # リポジトリ同期機能
├── README.md
└── CLAUDE.md
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
```

- `github-variables` — 共通変数を定義。`:custom` で値を設定する
- `github-sync-repositories` — リポジトリ同期機能。`:commands` で遅延読み込みされる

### require を使う場合

```elisp
(add-to-list 'load-path "~/.emacs.d/dist/github.el/src/")
(require 'github-variables)
(require 'github-sync-repositories)
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

設定しておくと `M-x github-sync-repositories` 実行時にミニバッファで入力せずにそのまま使われる。未設定の場合は毎回ミニバッファで入力を求められる。

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

`github-variable-directory` が設定済みならそのディレクトリに、未設定ならミニバッファで入力したディレクトリに、自分がオーナーのリポジトリを同期する。

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

## カスタマイズ変数

共通変数 (`github-variables.el`):

| 変数 | デフォルト | 説明 |
|------|-----------|------|
| `github-variable-token` | `nil` | GitHub パーソナルアクセストークン |
| `github-variable-directory` | `nil` | 操作先ディレクトリ |
| `github-variable-use-ssh` | `t` | `t` なら SSH、`nil` なら HTTPS |
| `github-variable-max-parallel` | `4` | 同時実行するプロセスの最大数 |

## 動作の流れ

1. `url-retrieve` で GraphQL API にリポジトリ一覧を非同期リクエスト (100件ずつページネーション)
2. 全ページ取得完了後、同期キューを開始
3. 各リポジトリについて:
   - `.git` ディレクトリが存在する → スキップ
   - 存在しない → `make-process` で git clone を非同期実行
4. 最大 `github-variable-max-parallel` 件を並列処理し、完了時に次のリポジトリをキューから取り出して起動
5. 全件完了後、結果サマリー (クローン数 / スキップ数 / 失敗数) を `*github-sync-repositories*` バッファに表示

## ライセンス

GPL-3.0

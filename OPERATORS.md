# OPERATORS.md

github-hara パッケージのオペレータ (関数・変数) マニュアル。

`--` 付きの関数は内部用。`--` なしの関数が公開 API。

---

## src/github-variables.el

共通変数を定義するモジュール。全機能モジュールから参照される。

### github-variable-token

`defcustom` / 型: `string` or `nil` / デフォルト: `nil`

GitHub パーソナルアクセストークン。`repo` スコープが必要。`~/.authinfo.gpg` から `auth-source-pick-first-password` で取得することを推奨する。

### github-variable-directory

`defcustom` / 型: `directory` or `nil` / デフォルト: `nil`

リポジトリ操作の対象ディレクトリ。各関数でディレクトリ引数を省略した場合のデフォルト値として使われる。

### github-variable-use-ssh

`defcustom` / 型: `boolean` / デフォルト: `t`

`t` なら SSH URL (`git@github.com:...`)、`nil` なら HTTPS URL (`https://github.com/...`) を使用する。

### github-variable-max-parallel

`defcustom` / 型: `integer` / デフォルト: `4`

git clone を同時に実行するプロセスの最大数。

---

## src/github-api.el

GitHub GraphQL API (v4) の非同期リクエスト基盤。各機能モジュールから `(require 'github-api)` して利用する。

### github-api--ensure-token

#### Syntax:

```elisp
(github-api--ensure-token)
```

#### Arguments and Values:

なし。

#### Description:

`github-variable-token` が非 nil であることを確認する。設定されていれば `nil` を返す。

#### Examples:

```elisp
(let ((github-variable-token "ghp_xxxx"))
  (github-api--ensure-token))  ; => nil (正常)

(let ((github-variable-token nil))
  (github-api--ensure-token))  ; => error
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

`github-variable-token` が `nil` の場合、エラーを signal する。

#### See Also:

`github-variable-token`

#### Notes:

API リクエストを行うすべての関数の先頭で呼ばれる。

---

### github-api--parse-response

#### Syntax:

```elisp
(github-api--parse-response buffer) → response
```

#### Arguments and Values:

- **buffer** — HTTP レスポンスを含むバッファ。ヘッダーとボディが空行 (`\n\n`) で区切られている必要がある
- **response** — パースされた JSON の alist

#### Description:

HTTP レスポンスバッファからヘッダー部分をスキップし、ボディの JSON をパースして alist として返す。レスポンスに GraphQL の `errors` フィールドが含まれる場合はエラーを signal する。

#### Examples:

```elisp
(with-temp-buffer
  (insert "HTTP/1.1 200 OK\n\n{\"data\":{\"viewer\":{\"login\":\"user\"}}}")
  (github-api--parse-response (current-buffer)))
;; => ((data . ((viewer . ((login . "user"))))))
```

#### Affected By:

なし。

#### Exceptional Situations:

- JSON パースに失敗した場合、`json-read` のエラーが signal される
- レスポンスに `errors` フィールドが含まれる場合、エラーメッセージを連結してエラーを signal する

#### See Also:

`github-api--graphql-request-async`

#### Notes:

内部関数。`github-api--graphql-request-async` のコールバック内で使用される。

---

### github-api--graphql-request-async

#### Syntax:

```elisp
(github-api--graphql-request-async query variables callback &optional error-callback)
```

#### Arguments and Values:

- **query** — GraphQL クエリ文字列
- **variables** — GraphQL 変数の alist。不要なら `nil`
- **callback** — 成功時コールバック関数。引数はレスポンス alist: `(lambda (response) ...)`
- **error-callback** — 失敗時コールバック関数 (省略可)。引数はエラー情報: `(lambda (err) ...)`

#### Description:

GitHub GraphQL API エンドポイント (`https://api.github.com/graphql`) に `url-retrieve` で非同期 POST リクエストを送信する。`github-variable-token` を Bearer トークンとして Authorization ヘッダーに設定する。

成功時は `callback` にパース済みレスポンスを渡す。失敗時は `error-callback` が指定されていればそれを呼び、未指定ならエラーを signal する。

#### Examples:

```elisp
(github-api--graphql-request-async
 "query { viewer { login } }"
 nil
 (lambda (response)
   (message "Login: %s"
            (alist-get 'login (alist-get 'viewer (alist-get 'data response)))))
 (lambda (err)
   (message "Error: %s" err)))
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

- `github-variable-token` が `nil` の場合、リクエスト前にエラーを signal する
- HTTP リクエストが失敗し `error-callback` が `nil` の場合、エラーを signal する
- レスポンスに GraphQL `errors` が含まれる場合、エラーを signal する

#### See Also:

`github-api--ensure-token`, `github-api--parse-response`

#### Notes:

すべての API 通信はこの関数を経由する。レスポンスバッファは自動的に kill される。

---

## src/github-sync-repositories.el

GitHub リポジトリの同期機能。clone 済みはスキップし、未 clone のみ clone する。すべて非同期で実行される。

### github-sync-repositories

#### Syntax:

```elisp
(github-sync-repositories &optional target-dir)
```

#### Arguments and Values:

- **target-dir** — クローン先ディレクトリのパス (省略可)

#### Description:

`M-x github-sync-repositories`

自分がオーナーのリポジトリを target-dir に同期する。clone 済みのリポジトリはスキップし、未 clone のリポジトリのみ git clone を実行する。target-dir 省略時は `github-variable-directory` を使用し、それも `nil` ならミニバッファで入力を求める。

処理はすべて非同期で実行される。進捗は `*github-sync-repositories*` バッファとミニバッファにリアルタイム表示され、完了時にサマリー (クローン数 / スキップ数 / 失敗数) を表示する。

#### Examples:

```elisp
;; ディレクトリを指定して実行
(github-sync-repositories "~/repos/")

;; github-variable-directory を使用
(let ((github-variable-directory "~/repos/"))
  (github-sync-repositories))

;; M-x で対話的に実行 (ミニバッファでディレクトリ入力)
M-x github-sync-repositories
```

#### Affected By:

`github-variable-token`, `github-variable-directory`, `github-variable-use-ssh`, `github-variable-max-parallel`

#### Exceptional Situations:

- target-dir が未指定かつ `github-variable-directory` も `nil` でミニバッファ入力もない場合、エラーを signal する
- `github-sync-repositories--current-state` が非 nil (既に実行中) の場合、エラーを signal する
- `github-variable-token` が `nil` の場合、エラーを signal する

#### See Also:

`github-sync-repositories-list`, `github-sync-repositories-cancel`, `github-variable-directory`

#### Notes:

インタラクティブコマンド。`;;;###autoload` 付き。

---

### github-sync-repositories-list

#### Syntax:

```elisp
(github-sync-repositories-list)
```

#### Arguments and Values:

なし。

#### Description:

`M-x github-sync-repositories-list`

自分がオーナーのリポジトリ一覧を GitHub API から非同期取得し、`*github-repos*` バッファに表示する。clone は行わない。アーカイブ済みリポジトリには `[archived]`、フォークには `[fork]` のラベルが付く。

#### Examples:

```elisp
M-x github-sync-repositories-list
;; => *github-repos* バッファに一覧が表示される
;;    Owned repositories (42):
;;
;;      my-project
;;      old-project [archived]
;;      some-fork [fork]
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

`github-variable-token` が `nil` の場合、エラーを signal する。

#### See Also:

`github-sync-repositories`

#### Notes:

インタラクティブコマンド。`;;;###autoload` 付き。

---

### github-sync-repositories-cancel

#### Syntax:

```elisp
(github-sync-repositories-cancel)
```

#### Arguments and Values:

なし。

#### Description:

`M-x github-sync-repositories-cancel`

実行中の同期処理をキャンセルする。`git-clone-` プレフィックスを持つすべてのプロセスを停止し、`github-sync-repositories--current-state` を `nil` にリセットする。

#### Examples:

```elisp
M-x github-sync-repositories-cancel
;; => "Sync operation cancelled." が表示される
```

#### Affected By:

`github-sync-repositories--current-state`

#### Exceptional Situations:

`github-sync-repositories--current-state` が `nil` (同期処理が実行中でない) の場合、エラーを signal する。

#### See Also:

`github-sync-repositories`

#### Notes:

インタラクティブコマンド。`;;;###autoload` 付き。

---

### github-sync-repositories--log

#### Syntax:

```elisp
(github-sync-repositories--log format-string &rest args)
```

#### Arguments and Values:

- **format-string** — `format` 関数に渡す書式文字列
- **args** — 書式文字列に対応する引数

#### Description:

format-string と args でメッセージを生成し、`*github-sync-repositories*` バッファの末尾に挿入し、同時にミニバッファにも `message` で表示する。

#### Examples:

```elisp
(github-sync-repositories--log "Found %d repositories" 42)
;; => ミニバッファ: "Found 42 repositories"
;; => *github-sync-repositories* バッファに "Found 42 repositories\n" が追加
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

なし。

#### Notes:

内部関数。バッファが存在しない場合は自動作成される。

---

### github-sync-repositories--fetch-all-repos-async

#### Syntax:

```elisp
(github-sync-repositories--fetch-all-repos-async callback)
```

#### Arguments and Values:

- **callback** — 全リポジトリ取得完了時のコールバック。引数はリポジトリ alist のリスト: `(lambda (repos) ...)`

#### Description:

GitHub GraphQL API からオーナーのリポジトリ一覧をページネーション (100 件/ページ) 付きで非同期取得する。全ページ取得完了後、callback にリポジトリ alist のリストを渡す。

#### Examples:

```elisp
(github-sync-repositories--fetch-all-repos-async
 (lambda (repos)
   (message "Total: %d repos" (length repos))))
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

API リクエスト失敗時は `*github-sync-repositories*` バッファにエラーログを出力する。

#### See Also:

`github-sync-repositories--fetch-page-async`, `github-api--graphql-request-async`

#### Notes:

内部関数。`github-sync-repositories--fetch-page-async` に委譲する。

---

### github-sync-repositories--fetch-page-async

#### Syntax:

```elisp
(github-sync-repositories--fetch-page-async cursor acc callback)
```

#### Arguments and Values:

- **cursor** — ページネーションカーソル文字列。最初のページは `nil`
- **acc** — これまでに蓄積したリポジトリのリスト
- **callback** — 全ページ取得完了時のコールバック

#### Description:

cursor 位置から 1 ページ (最大 100 件) のリポジトリを取得し、acc に蓄積する。`hasNextPage` が `t` なら `endCursor` で再帰し、`nil` なら callback を呼ぶ。

#### Examples:

```elisp
;; 最初のページから取得開始
(github-sync-repositories--fetch-page-async nil '() #'my-callback)
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

API リクエスト失敗時は error-callback でログ出力する。

#### See Also:

`github-sync-repositories--fetch-all-repos-async`, `github-api--graphql-request-async`

#### Notes:

内部関数。再帰的にページを取得する。

---

### github-sync-repositories--start-clone-queue

#### Syntax:

```elisp
(github-sync-repositories--start-clone-queue state)
```

#### Arguments and Values:

- **state** — `github-sync-repositories--state` 構造体

#### Description:

state の pending キューからリポジトリを取り出し、`github-variable-max-parallel` を上限として `github-sync-repositories--clone-repo-async` でクローンプロセスを起動する。active 数が上限に達するか、pending が空になるまで繰り返す。

#### Examples:

```elisp
(github-sync-repositories--start-clone-queue state)
;; => pending からリポジトリを取り出してクローン開始
```

#### Affected By:

`github-variable-max-parallel`

#### Exceptional Situations:

なし。

#### See Also:

`github-sync-repositories--clone-repo-async`, `github-sync-repositories--maybe-finish`

#### Notes:

内部関数。各クローン完了時に `--maybe-finish` 経由で再度呼ばれ、キューの次を処理する。

---

### github-sync-repositories--clone-repo-async

#### Syntax:

```elisp
(github-sync-repositories--clone-repo-async repo state)
```

#### Arguments and Values:

- **repo** — リポジトリ情報の alist (`name`, `sshUrl`, `url` 等を含む)
- **state** — `github-sync-repositories--state` 構造体

#### Description:

repo を非同期で git clone する。対象ディレクトリに `.git` が既に存在する場合はスキップし、state の results に `(name . :skipped)` を記録する。存在しない場合は `make-process` で `git clone` を起動し、完了時に `(name . :cloned)` または `(name . :failed)` を記録する。

`github-variable-use-ssh` が `t` なら `sshUrl`、`nil` なら `url` をクローン URL として使用する。

#### Examples:

```elisp
(github-sync-repositories--clone-repo-async
 '((name . "my-repo") (sshUrl . "git@github.com:user/my-repo.git"))
 state)
```

#### Affected By:

`github-variable-use-ssh`

#### Exceptional Situations:

git clone プロセスが異常終了した場合、`:failed` として記録される (エラーは signal しない)。

#### See Also:

`github-sync-repositories--start-clone-queue`, `github-sync-repositories--maybe-finish`

#### Notes:

内部関数。プロセスの sentinel でクローン完了を検知する。

---

### github-sync-repositories--maybe-finish

#### Syntax:

```elisp
(github-sync-repositories--maybe-finish state)
```

#### Arguments and Values:

- **state** — `github-sync-repositories--state` 構造体

#### Description:

state の results 数と total を比較する。全件完了なら `github-sync-repositories--show-summary` を呼ぶ。未完了なら `github-sync-repositories--start-clone-queue` を呼んでキューの次を処理する。

#### Examples:

```elisp
(github-sync-repositories--maybe-finish state)
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-sync-repositories--show-summary`, `github-sync-repositories--start-clone-queue`

#### Notes:

内部関数。各クローンプロセスの完了時に呼ばれる。

---

### github-sync-repositories--show-summary

#### Syntax:

```elisp
(github-sync-repositories--show-summary state)
```

#### Arguments and Values:

- **state** — `github-sync-repositories--state` 構造体

#### Description:

state の results からクローン数 (`:cloned`)・スキップ数 (`:skipped`)・失敗数 (`:failed`) を集計し、`*github-sync-repositories*` バッファにサマリーを表示する。失敗したリポジトリがあれば名前を個別に表示する。表示後、`github-sync-repositories--current-state` を `nil` にリセットする。

#### Examples:

```elisp
;; バッファ出力例:
;; Done! Cloned: 3 / Skipped: 10 / Failed: 1 (Total: 14)
;;
;; Failed repositories:
;;   - broken-repo
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-sync-repositories--maybe-finish`

#### Notes:

内部関数。`display-buffer` でバッファを表示する。

---

### github-sync-repositories--state

#### Syntax:

```elisp
(github-sync-repositories--state-create &key target-dir repos pending active results total) → state
```

#### Arguments and Values:

- **target-dir** — クローン先ディレクトリ (string)
- **repos** — 全リポジトリのリスト (list)
- **pending** — 未処理リポジトリのキュー (list)
- **active** — 実行中のプロセス数 (integer、デフォルト `0`)
- **results** — `(name . status)` の結果リスト (list、デフォルト `nil`)
- **total** — リポジトリの総数 (integer、デフォルト `0`)
- **state** — 作成された構造体

#### Description:

`cl-defstruct` で定義された同期操作の状態構造体。アクセサは `github-sync-repositories--state-target-dir`, `github-sync-repositories--state-repos` 等。すべてのフィールドは `setf` で変更可能。

#### Examples:

```elisp
(let ((state (github-sync-repositories--state-create
              :target-dir "/tmp/repos"
              :total 5
              :pending repos-list)))
  (github-sync-repositories--state-total state))  ; => 5
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-sync-repositories`

#### Notes:

`cl-defstruct` により自動生成されるコンストラクタ・アクセサ・述語を持つ。

---

### github-sync-repositories--current-state

`defvar` / デフォルト: `nil`

実行中の同期操作の状態を保持する。同期処理中は `github-sync-repositories--state` 構造体が入り、完了またはキャンセル時に `nil` にリセットされる。

---

## src/github-empty-repositories.el

GitHub 上とローカルの空リポジトリを検出する機能。検出関数はリストを返し、表示は別関数で行う。

### github-empty-repositories

#### Syntax:

```elisp
(github-empty-repositories &optional target-dir)
```

#### Arguments and Values:

- **target-dir** — ローカルチェック対象ディレクトリのパス (省略可)

#### Description:

`M-x github-empty-repositories`

GitHub 上とローカルディレクトリの両方で空リポジトリを検出し、結果をバッファに表示する。ローカルチェック (同期) を先に実行し、その後 GitHub API (非同期) を実行する。両方の結果を `github-empty-repositories-display` で統合表示する。

target-dir 省略時は `github-variable-directory` を使用し、それも `nil` ならミニバッファで入力を求める。

#### Examples:

```elisp
;; ディレクトリを指定して実行
(github-empty-repositories "~/repos/")

;; M-x で対話的に実行
M-x github-empty-repositories
```

#### Affected By:

`github-variable-token`, `github-variable-directory`

#### Exceptional Situations:

- target-dir が未指定かつ `github-variable-directory` も `nil` でミニバッファ入力もない場合、エラーを signal する
- `github-variable-token` が `nil` の場合、エラーを signal する

#### See Also:

`github-empty-repositories-local`, `github-empty-repositories-display`, `github-empty-repositories-find-empty-local`, `github-empty-repositories-find-empty-remote`

#### Notes:

インタラクティブコマンド。`;;;###autoload` 付き。

---

### github-empty-repositories-local

#### Syntax:

```elisp
(github-empty-repositories-local &optional target-dir)
```

#### Arguments and Values:

- **target-dir** — ローカルチェック対象ディレクトリのパス (省略可)

#### Description:

`M-x github-empty-repositories-local`

ローカルディレクトリのみで空リポジトリを検出し、結果をバッファに表示する。GitHub API を使用しないため、トークン不要。target-dir 省略時は `github-variable-directory` を使用し、それも `nil` ならミニバッファで入力を求める。

#### Examples:

```elisp
;; トークンなしで実行可能
(github-empty-repositories-local "~/repos/")
```

#### Affected By:

`github-variable-directory`

#### Exceptional Situations:

target-dir が未指定かつ `github-variable-directory` も `nil` でミニバッファ入力もない場合、エラーを signal する。

#### See Also:

`github-empty-repositories`, `github-empty-repositories-find-empty-local`, `github-empty-repositories-display`

#### Notes:

インタラクティブコマンド。`;;;###autoload` 付き。

---

### github-empty-repositories-filter-empty

#### Syntax:

```elisp
(github-empty-repositories-filter-empty repos) → empty-repos
```

#### Arguments and Values:

- **repos** — リポジトリ alist のリスト。各 alist には `name`, `defaultBranchRef` 等を含む
- **empty-repos** — 空リポジトリのみの alist リスト

#### Description:

repos から `defaultBranchRef` が `nil` (コミットが存在しない) のリポジトリだけをフィルタして返す。

#### Examples:

```elisp
(github-empty-repositories-filter-empty
 '(((name . "empty") (defaultBranchRef))
   ((name . "normal") (defaultBranchRef . ((name . "main"))))))
;; => (((name . "empty") (defaultBranchRef)))
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-empty-repositories-find-empty-remote`

#### Notes:

純粋関数。副作用なし。

---

### github-empty-repositories-local-status

#### Syntax:

```elisp
(github-empty-repositories-local-status &optional dir) → status
```

#### Arguments and Values:

- **dir** — チェック対象ディレクトリのパス (省略可)。省略時は `github-variable-directory`
- **status** — 以下のいずれか:

| 値 | 意味 |
|---|---|
| `:empty-dir` | ディレクトリが空 |
| `:no-git` | `.git` ディレクトリが存在しない |
| `:no-commits` | `.git` は存在するが HEAD が解決できない |
| `:has-commits` | 正常なリポジトリ (コミットあり) |
| `nil` | ディレクトリが存在しない |

#### Description:

dir のリポジトリ状態を判定する。ドットで始まるファイルのみのディレクトリは「空」と判定する。`.git` の存在確認と `git rev-parse HEAD` の終了コードで状態を分類する。

#### Examples:

```elisp
(github-empty-repositories-local-status "/tmp/empty-dir/")
;; => :empty-dir

(github-empty-repositories-local-status "/nonexistent/")
;; => nil

;; github-variable-directory を使用
(let ((github-variable-directory "/tmp/some-repo/"))
  (github-empty-repositories-local-status))
;; => :has-commits
```

#### Affected By:

`github-variable-directory` (dir 省略時)

#### Exceptional Situations:

dir が `nil` かつ `github-variable-directory` も `nil` の場合、エラーを signal する。

#### See Also:

`github-empty-repositories-find-empty-local`

#### Notes:

`call-process` で `git rev-parse HEAD` を同期実行するが、処理は瞬時に完了する。

---

### github-empty-repositories-find-empty-local

#### Syntax:

```elisp
(github-empty-repositories-find-empty-local &optional target-dir) → result
```

#### Arguments and Values:

- **target-dir** — 走査対象の親ディレクトリ (省略可)。省略時は `github-variable-directory`
- **result** — `(name . status)` の alist リスト。status は `:empty-dir`, `:no-git`, `:no-commits` のいずれか

#### Description:

target-dir 直下のサブディレクトリを走査し、空リポジトリを検出して返す。ドットで始まるディレクトリは無視する。正常リポジトリ (`:has-commits`) は結果に含まれない。

#### Examples:

```elisp
(github-empty-repositories-find-empty-local "~/repos/")
;; => (("abandoned-project" . :no-commits)
;;     ("empty-folder" . :empty-dir)
;;     ("random-files" . :no-git))

;; 空リポジトリがない場合
(github-empty-repositories-find-empty-local "~/repos/")
;; => nil

;; github-variable-directory を使用
(let ((github-variable-directory "~/repos/"))
  (github-empty-repositories-find-empty-local))
```

#### Affected By:

`github-variable-directory` (target-dir 省略時)

#### Exceptional Situations:

target-dir が `nil` かつ `github-variable-directory` も `nil` の場合、エラーを signal する。

#### See Also:

`github-empty-repositories-local-status`, `github-empty-repositories-display`

#### Notes:

同期関数。各サブディレクトリに対して `github-empty-repositories-local-status` を呼ぶ。

---

### github-empty-repositories-find-empty-remote

#### Syntax:

```elisp
(github-empty-repositories-find-empty-remote callback)
```

#### Arguments and Values:

- **callback** — 空リポジトリ取得完了時のコールバック。引数は空リポジトリ alist のリスト: `(lambda (repos) ...)`。各 alist には `name`, `sshUrl`, `url`, `isArchived`, `isFork` を含む

#### Description:

GitHub API から全リポジトリを非同期取得し、`github-empty-repositories-filter-empty` でフィルタした結果を callback に渡す。

#### Examples:

```elisp
(github-empty-repositories-find-empty-remote
 (lambda (repos)
   (dolist (r repos)
     (message "Empty: %s" (alist-get 'name r)))))
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

`github-variable-token` が `nil` の場合、エラーを signal する。API リクエスト失敗時もエラーを signal する。

#### See Also:

`github-empty-repositories-filter-empty`, `github-empty-repositories-display`

#### Notes:

非同期関数。結果はコールバックで返される。

---

### github-empty-repositories-display

#### Syntax:

```elisp
(github-empty-repositories-display remote-empty local-empty target-dir)
```

#### Arguments and Values:

- **remote-empty** — GitHub 上の空リポジトリ alist リスト。チェックしなかった場合は `nil`
- **local-empty** — ローカルの空リポジトリ `(name . status)` リスト
- **target-dir** — ローカルチェック対象ディレクトリのパス (表示用)

#### Description:

検出結果を `*github-empty-repositories*` バッファに統合表示する。バッファは毎回初期化される。

表示セクション:
1. **GitHub 上の空リポジトリ** — リポジトリ名にアーカイブ `[archived]`・フォーク `[fork]` のラベル付き
2. **ローカルの空リポジトリ** — リポジトリ名に状態ラベル (`[空ディレクトリ]`, `[.git なし]`, `[コミットなし]`) 付き
3. **リモート・ローカル両方で空** — 両方に該当するリポジトリがある場合のみ表示

#### Examples:

```elisp
(github-empty-repositories-display
 '(((name . "empty-repo") (isArchived . :json-false) (isFork . :json-false)))
 '(("local-empty" . :empty-dir))
 "~/repos/")
;; => *github-empty-repositories* バッファに以下を表示:
;;    === 空リポジトリ一覧 ===
;;
;;    ■ GitHub 上の空リポジトリ (1 件):
;;
;;      empty-repo
;;
;;    ■ ローカルの空リポジトリ (~/repos/, 1 件):
;;
;;      local-empty [空ディレクトリ]
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-empty-repositories`, `github-empty-repositories-local`, `github-empty-repositories-find-empty-local`, `github-empty-repositories-find-empty-remote`

#### Notes:

`display-buffer` でバッファを表示する。検出ロジックとは分離されている。

---

### github-empty-repositories--fetch-all-repos-async

#### Syntax:

```elisp
(github-empty-repositories--fetch-all-repos-async callback)
```

#### Arguments and Values:

- **callback** — 全リポジトリ取得完了時のコールバック。引数はリポジトリ alist のリスト: `(lambda (repos) ...)`

#### Description:

GitHub GraphQL API からリポジトリ一覧を `defaultBranchRef` フィールド付きで非同期取得する。ページネーション (100 件/ページ) で全ページ取得完了後、callback を呼ぶ。

#### Examples:

```elisp
(github-empty-repositories--fetch-all-repos-async
 (lambda (repos)
   (message "Fetched %d repos" (length repos))))
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

API リクエスト失敗時はエラーを signal する。

#### See Also:

`github-empty-repositories--fetch-page-async`, `github-api--graphql-request-async`

#### Notes:

内部関数。`github-empty-repositories--fetch-page-async` に委譲する。

---

### github-empty-repositories--fetch-page-async

#### Syntax:

```elisp
(github-empty-repositories--fetch-page-async cursor acc callback)
```

#### Arguments and Values:

- **cursor** — ページネーションカーソル文字列。最初のページは `nil`
- **acc** — これまでに蓄積したリポジトリのリスト
- **callback** — 全ページ取得完了時のコールバック

#### Description:

cursor 位置から 1 ページ (最大 100 件) のリポジトリを取得し、acc に蓄積する。`hasNextPage` が `t` なら `endCursor` で再帰し、`nil` なら callback を呼ぶ。

#### Examples:

```elisp
(github-empty-repositories--fetch-page-async nil '() #'my-callback)
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

API リクエスト失敗時はエラーを signal する。

#### See Also:

`github-empty-repositories--fetch-all-repos-async`, `github-api--graphql-request-async`

#### Notes:

内部関数。再帰的にページを取得する。

---

## src/github-behind-repositories.el

ローカルにクローン済みのリポジトリのうち、リモート追跡ブランチより遅れている (pull が必要な) ものを検出する機能。検出関数はリストを返し、表示は別関数で行う。

### github-behind-repositories

#### Syntax:

```elisp
(github-behind-repositories &optional target-dir)
```

#### Arguments and Values:

- **target-dir** — チェック対象ディレクトリのパス (省略可)

#### Description:

`M-x github-behind-repositories`

target-dir 直下のサブディレクトリを走査し、リモート追跡ブランチより遅れている (pull が必要な) リポジトリを検出して `*github-behind-repositories*` バッファに一覧表示する。

target-dir 省略時は `github-variable-directory` を使用し、それも `nil` ならミニバッファで入力を求める。

#### Examples:

```elisp
;; ディレクトリを指定して実行
(github-behind-repositories "~/repos/")

;; github-variable-directory を使用
(let ((github-variable-directory "~/repos/"))
  (github-behind-repositories))

;; M-x で対話的に実行
M-x github-behind-repositories
```

#### Affected By:

`github-variable-directory`

#### Exceptional Situations:

target-dir が未指定かつ `github-variable-directory` も `nil` でミニバッファ入力もない場合、エラーを signal する。

#### See Also:

`github-behind-repositories-find-behind-local`, `github-behind-repositories-display`, `github-behind-repositories-local-status`

#### Notes:

インタラクティブコマンド。`;;;###autoload` 付き。GitHub API は使用しない (トークン不要)。`git fetch` なしの場合、ローカルにキャッシュされたリモート情報に基づく判定となる。

---

### github-behind-repositories-local-status

#### Syntax:

```elisp
(github-behind-repositories-local-status &optional dir) → status
```

#### Arguments and Values:

- **dir** — チェック対象のリポジトリディレクトリ (省略可)。省略時は `github-variable-directory`
- **status** — 以下のキーを持つ alist。git リポジトリでない場合や upstream 未設定の場合は `nil`

| キー | 値 |
|---|---|
| `behind` | upstream より遅れているコミット数 (integer) |
| `ahead` | upstream より先行しているコミット数 (integer) |
| `branch` | 現在のブランチ名 (string) |
| `upstream` | 追跡ブランチ名 (string) |

#### Description:

dir のリポジトリについて、現在のブランチとリモート追跡ブランチの差分を `git rev-list --count` で取得し、alist で返す。

#### Examples:

```elisp
(github-behind-repositories-local-status "~/repos/my-project/")
;; => ((behind . 3) (ahead . 0) (branch . "main") (upstream . "origin/main"))

;; upstream がない場合
(github-behind-repositories-local-status "~/repos/local-only/")
;; => nil

;; github-variable-directory を使用
(let ((github-variable-directory "~/repos/my-project/"))
  (github-behind-repositories-local-status))
```

#### Affected By:

`github-variable-directory` (dir 省略時)

#### Exceptional Situations:

dir が `nil` かつ `github-variable-directory` も `nil` の場合、エラーを signal する。

#### See Also:

`github-behind-repositories-find-behind-local`, `github-behind-repositories--parse-status`

#### Notes:

`call-process` で git コマンドを同期実行するが、処理は瞬時に完了する。

---

### github-behind-repositories-find-behind-local

#### Syntax:

```elisp
(github-behind-repositories-find-behind-local &optional target-dir) → result
```

#### Arguments and Values:

- **target-dir** — 走査対象の親ディレクトリ (省略可)。省略時は `github-variable-directory`
- **result** — `(name . status-alist)` のリスト。`behind > 0` のもののみ含む

#### Description:

target-dir 直下のサブディレクトリを走査し、リモート追跡ブランチより遅れているリポジトリを検出して返す。ドットで始まるディレクトリは無視する。git リポジトリでないディレクトリや upstream 未設定のリポジトリも結果に含まれない。

#### Examples:

```elisp
(github-behind-repositories-find-behind-local "~/repos/")
;; => (("project-a" . ((behind . 3) (ahead . 0) (branch . "main") (upstream . "origin/main")))
;;     ("project-b" . ((behind . 1) (ahead . 2) (branch . "develop") (upstream . "origin/develop"))))

;; 遅れているリポジトリがない場合
(github-behind-repositories-find-behind-local "~/repos/")
;; => nil

;; github-variable-directory を使用
(let ((github-variable-directory "~/repos/"))
  (github-behind-repositories-find-behind-local))
```

#### Affected By:

`github-variable-directory` (target-dir 省略時)

#### Exceptional Situations:

target-dir が `nil` かつ `github-variable-directory` も `nil` の場合、エラーを signal する。

#### See Also:

`github-behind-repositories-local-status`, `github-behind-repositories-display`, `github-behind-repositories--parse-status`

#### Notes:

同期関数。各サブディレクトリに対して `github-behind-repositories--parse-status` を呼ぶ。

---

### github-behind-repositories-display

#### Syntax:

```elisp
(github-behind-repositories-display repos target-dir)
```

#### Arguments and Values:

- **repos** — `(name . status-alist)` のリスト。各 status-alist には `behind`, `ahead`, `branch`, `upstream` を含む
- **target-dir** — チェック対象ディレクトリのパス (表示用)

#### Description:

検出結果を `*github-behind-repositories*` バッファに表示する。バッファは毎回初期化される。

各リポジトリについて、リポジトリ名、ブランチ名と追跡ブランチ名、遅れコミット数を表示する。ahead がある場合は先行コミット数も表示する。遅れているリポジトリがない場合は「すべてのリポジトリは最新です。」と表示する。

#### Examples:

```elisp
(github-behind-repositories-display
 '(("my-repo" . ((behind . 3) (ahead . 0) (branch . "main") (upstream . "origin/main"))))
 "~/repos/")
;; => *github-behind-repositories* バッファに以下を表示:
;;    === pull が必要なリポジトリ一覧 ===
;;
;;    対象ディレクトリ: ~/repos/
;;
;;    1 件のリポジトリが遅れています:
;;
;;      my-repo
;;        ブランチ: main → origin/main
;;        遅れ: 3 コミット
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-behind-repositories`, `github-behind-repositories-find-behind-local`

#### Notes:

`display-buffer` でバッファを表示する。検出ロジックとは分離されている。

---

### github-behind-repositories--parse-status

#### Syntax:

```elisp
(github-behind-repositories--parse-status dir) → status
```

#### Arguments and Values:

- **dir** — チェック対象のリポジトリディレクトリ
- **status** — `(behind . N)`, `(ahead . N)`, `(branch . "name")`, `(upstream . "name")` を含む alist。git リポジトリでない場合や upstream 未設定の場合は `nil`

#### Description:

dir の git 状態を解析する。以下の手順で判定を行う:

1. `git rev-parse --is-inside-work-tree` で git リポジトリか確認
2. `git rev-parse --abbrev-ref HEAD` で現在のブランチ名を取得 (detached HEAD なら `nil`)
3. `git rev-parse --abbrev-ref BRANCH@{upstream}` で upstream を取得 (未設定なら `nil`)
4. `git rev-list --count HEAD..BRANCH@{upstream}` で遅れコミット数を取得
5. `git rev-list --count BRANCH@{upstream}..HEAD` で先行コミット数を取得

#### Examples:

```elisp
(github-behind-repositories--parse-status "~/repos/my-project/")
;; => ((behind . 0) (ahead . 0) (branch . "main") (upstream . "origin/main"))
```

#### Affected By:

なし。

#### Exceptional Situations:

なし (git コマンド失敗時は `nil` を返す)。

#### See Also:

`github-behind-repositories-local-status`, `github-behind-repositories-find-behind-local`

#### Notes:

内部関数。`call-process` で git コマンドを同期実行する。`cl-return-from` で早期リターンを行う。

---

### github-behind-repositories--buffer-name

`defconst` / 値: `"*github-behind-repositories*"`

結果表示用バッファの名前。

---

## src/github-orphan-repositories.el

ローカルにクローン済みだが、GitHub 上にもう存在しないリポジトリ (孤立リポジトリ) を検出する機能。GitHub GraphQL API でオーナーのリポジトリ名一覧を取得し、ローカルの git リポジトリと比較する。検出関数はリストを返し、表示は別関数で行う。

### github-orphan-repositories

#### Syntax:

```elisp
(github-orphan-repositories &optional target-dir)
```

#### Arguments and Values:

- **target-dir** — チェック対象ディレクトリのパス (省略可)

#### Description:

`M-x github-orphan-repositories`

GitHub API から自分がオーナーのリポジトリ名一覧を非同期取得し、target-dir 直下のローカル git リポジトリと比較する。ローカルにあるが GitHub 上に存在しないリポジトリを `*github-orphan-repositories*` バッファに一覧表示する。

target-dir 省略時は `github-variable-directory` を使用し、それも `nil` ならミニバッファで入力を求める。

#### Examples:

```elisp
;; ディレクトリを指定して実行
(github-orphan-repositories "~/repos/")

;; github-variable-directory を使用
(let ((github-variable-directory "~/repos/"))
  (github-orphan-repositories))

;; M-x で対話的に実行
M-x github-orphan-repositories
```

#### Affected By:

`github-variable-token`, `github-variable-directory`

#### Exceptional Situations:

- target-dir が未指定かつ `github-variable-directory` も `nil` でミニバッファ入力もない場合、エラーを signal する
- `github-variable-token` が `nil` の場合、エラーを signal する

#### See Also:

`github-orphan-repositories-find-orphans`, `github-orphan-repositories-display`

#### Notes:

インタラクティブコマンド。`;;;###autoload` 付き。`ownerAffiliations: [OWNER]` でフィルタするため、コラボレータ権限のみのリポジトリや Organization リポジトリは GitHub 側に含まれず、孤立と誤判定される可能性がある。

---

### github-orphan-repositories-find-orphans

#### Syntax:

```elisp
(github-orphan-repositories-find-orphans callback &optional target-dir)
```

#### Arguments and Values:

- **callback** — 孤立リポジトリ検出完了時のコールバック。引数は名前文字列のリスト: `(lambda (orphans) ...)`
- **target-dir** — チェック対象ディレクトリのパス (省略可)。省略時は `github-variable-directory`

#### Description:

GitHub API から全リポジトリ名を非同期取得し、target-dir 直下のローカル git リポジトリ名と比較する。ローカルにあるが GitHub 上にないリポジトリ名のリストを callback に渡す。

#### Examples:

```elisp
(github-orphan-repositories-find-orphans
 (lambda (orphans)
   (dolist (name orphans)
     (message "Orphan: %s" name)))
 "~/repos/")

;; github-variable-directory を使用
(let ((github-variable-directory "~/repos/"))
  (github-orphan-repositories-find-orphans
   (lambda (orphans)
     (message "%d orphans found" (length orphans)))))
```

#### Affected By:

`github-variable-token`, `github-variable-directory` (target-dir 省略時)

#### Exceptional Situations:

- target-dir が `nil` かつ `github-variable-directory` も `nil` の場合、エラーを signal する
- `github-variable-token` が `nil` の場合、エラーを signal する

#### See Also:

`github-orphan-repositories--collect-local-repos`, `github-orphan-repositories--find-orphans`, `github-orphan-repositories-display`

#### Notes:

非同期関数。結果はコールバックで返される。

---

### github-orphan-repositories-display

#### Syntax:

```elisp
(github-orphan-repositories-display orphans target-dir)
```

#### Arguments and Values:

- **orphans** — 孤立リポジトリ名の文字列リスト
- **target-dir** — チェック対象ディレクトリのパス (表示用)

#### Description:

検出結果を `*github-orphan-repositories*` バッファに表示する。バッファは毎回初期化される。孤立リポジトリがない場合は「すべてのローカルリポジトリは GitHub 上に存在します。」と表示する。末尾に `ownerAffiliations` による制限の注意事項を表示する。

#### Examples:

```elisp
(github-orphan-repositories-display '("deleted-repo" "old-project") "~/repos/")
;; => *github-orphan-repositories* バッファに以下を表示:
;;    === GitHub 上に存在しないローカルリポジトリ一覧 ===
;;
;;    対象ディレクトリ: ~/repos/
;;
;;    2 件の孤立リポジトリが見つかりました:
;;
;;      deleted-repo
;;      old-project
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-orphan-repositories`, `github-orphan-repositories-find-orphans`

#### Notes:

`display-buffer` でバッファを表示する。検出ロジックとは分離されている。

---

### github-orphan-repositories--fetch-all-repos-async

#### Syntax:

```elisp
(github-orphan-repositories--fetch-all-repos-async callback)
```

#### Arguments and Values:

- **callback** — 全リポジトリ名取得完了時のコールバック。引数は名前文字列のリスト: `(lambda (names) ...)`

#### Description:

GitHub GraphQL API からオーナーのリポジトリ名一覧をページネーション (100 件/ページ) 付きで非同期取得する。全ページ取得完了後、名前のリストを callback に渡す。

#### Examples:

```elisp
(github-orphan-repositories--fetch-all-repos-async
 (lambda (names)
   (message "Total: %d repos" (length names))))
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

API リクエスト失敗時はエラーを signal する。

#### See Also:

`github-orphan-repositories--fetch-page-async`, `github-api--graphql-request-async`

#### Notes:

内部関数。`github-orphan-repositories--fetch-page-async` に委譲する。

---

### github-orphan-repositories--fetch-page-async

#### Syntax:

```elisp
(github-orphan-repositories--fetch-page-async cursor acc callback)
```

#### Arguments and Values:

- **cursor** — ページネーションカーソル文字列。最初のページは `nil`
- **acc** — これまでに蓄積した名前のリスト
- **callback** — 全ページ取得完了時のコールバック

#### Description:

cursor 位置から 1 ページ (最大 100 件) のリポジトリ名を取得し、acc に蓄積する。`hasNextPage` が `t` なら `endCursor` で再帰し、`nil` なら callback を呼ぶ。

#### Examples:

```elisp
(github-orphan-repositories--fetch-page-async nil '() #'my-callback)
```

#### Affected By:

`github-variable-token`

#### Exceptional Situations:

API リクエスト失敗時はエラーを signal する。

#### See Also:

`github-orphan-repositories--fetch-all-repos-async`, `github-api--graphql-request-async`

#### Notes:

内部関数。再帰的にページを取得する。

---

### github-orphan-repositories--collect-local-repos

#### Syntax:

```elisp
(github-orphan-repositories--collect-local-repos target-dir) → names
```

#### Arguments and Values:

- **target-dir** — 走査対象の親ディレクトリ
- **names** — `.git` を含むサブディレクトリ名の文字列リスト

#### Description:

target-dir 直下のサブディレクトリを走査し、`.git` ディレクトリを含むもの (git リポジトリ) の名前を返す。ドットで始まるディレクトリは無視する。

#### Examples:

```elisp
(github-orphan-repositories--collect-local-repos "~/repos/")
;; => ("my-project" "another-repo" "old-tool")
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-orphan-repositories-find-orphans`

#### Notes:

内部関数。純粋な同期関数。

---

### github-orphan-repositories--find-orphans

#### Syntax:

```elisp
(github-orphan-repositories--find-orphans remote-names local-names) → orphans
```

#### Arguments and Values:

- **remote-names** — GitHub 上のリポジトリ名文字列リスト
- **local-names** — ローカルのリポジトリ名文字列リスト
- **orphans** — ローカルにあるが remote にない名前のリスト (アルファベット順ソート済み)

#### Description:

remote-names をハッシュテーブルに格納し、local-names のうち remote にないものを抽出して返す。結果はアルファベット順にソートされる。

#### Examples:

```elisp
(github-orphan-repositories--find-orphans
 '("repo-a" "repo-b" "repo-c")
 '("repo-a" "repo-d" "repo-e"))
;; => ("repo-d" "repo-e")
```

#### Affected By:

なし。

#### Exceptional Situations:

なし。

#### See Also:

`github-orphan-repositories-find-orphans`

#### Notes:

内部関数。純粋関数。副作用なし。

---

### github-orphan-repositories--graphql-query

`defconst` / GraphQL クエリ文字列

孤立リポジトリ検出用のリポジトリ名取得クエリ。`name` フィールドのみを取得する軽量なクエリ。

---

### github-orphan-repositories--buffer-name

`defconst` / 値: `"*github-orphan-repositories*"`

結果表示用バッファの名前。

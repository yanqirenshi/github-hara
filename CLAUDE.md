## Conversation Guidelines

- 常に日本語で会話する
- 技術的な説明も日本語で行う
- コード内のコメントは日本語で記述
- エラーメッセージの解説は日本語で
- README.mdなどのドキュメントも日本語で作成

## OPERATORS.md の維持

- `defun`, `defcustom`, `defvar`, `defconst`, `cl-defstruct` を追加・変更・削除した場合は、必ず OPERATORS.md も同時に更新すること
- OPERATORS.md は Common Lisp HyperSpec スタイルで記述する (Syntax / Arguments and Values / Description / Examples / Affected By / Exceptional Situations / See Also / Notes)
- ソースコードと OPERATORS.md の内容は常に一致していなければならない


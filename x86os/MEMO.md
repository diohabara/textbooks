# Memo

Personal notes.

## NASM

- `$$`
  - セクションの開始位置
- `$`
  - セクションの先頭からのオフセット
- `ORG`
  - BIOSによってアドレスの`0x7C00`に番地にロードされることをアセンブラに知らせる

## 全般

- バグっていたのだが，原因が`cmd.init`で`c\n`を`c`と書いていたことだった…
- `../../env/mk.sh`でアセンブルして，`../../env/box.sh`で実行．その後に`Debugger`を押す
- `0b`と`0x`を間違えてコンパイルしたときに失敗することがあるので気を付ける．
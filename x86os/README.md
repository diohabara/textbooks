# x86-OS

- the implementation of [this book](https://gihyo.jp/book/2019/978-4-297-10847-2)(book about x86-ox)
- written only in x86

## Environment

- OS: Ubuntu18:04
- Assembly: NASM 2.13.02
- Emulator: Bochs 2.6

### Setting for Ubuntu

```sh
sudo apt install nasm
sudo apt install bochs
```

### Memo(JPN)

### 12章

- `env.bat`と`dev.bat`は作らなかった．
- `bat`は全て`sh`とした
- `boot.bat`を使わず`Bochs`で行った．
  - `.bochsrc`は`env`ディレクトリ内に作った．
  - `box.sh`を使って起動するようにした．
- Bochsでエラーが出たのだが，完全にこの[SO](https://stackoverflow.com/questions/42453852/assembly-problems-running-a-bootloader-in-bochs)と同じで，これを見たら直った

### 章

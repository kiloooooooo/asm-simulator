# asm-simulator
i8080 Simulator

# インストール

## Linux

Linux向けの実行ファイルをリリースの添付ファイルとして公開しています（asm-simulator-linux）。
これを適当な場所にダウンロードし、直接実行できます。

Ubuntu 16.04LTS で動作確認していますが、Ubuntuのそれ以降のバージョンや他のディストリビューションでも多分動きます。

自前でビルドする場合は、Rustをインストールし、リポジトリをクローンするか、リリースに添付されたソースコードをダウンロードして、
次のコマンドを実行してください。

```
$ cargo build --release
```

## Windows

Rustをインストールし、ビルドしてください。
リポジトリをクローンするか、リリースに添付されたソースコードをダウンロードして、次のコマンドを実行します。

```
PS> cargo build --release
```

完了すると、`target/release/asm-simulator.exe`として実行ファイルが作成されます。
コマンドプロンプトやPowerShellで `target/release/`に移動し、実行してください。

## macOS

Windowsと同様です。Rustをインストールし、ビルドします。
リポジトリをクローンするか、リリースに添付されたソースコードをダウンロードし、次のコマンドを実行します。

```
$ cargo build --release
```

# 実行

実行の際、コマンドライン引数として実行対象のファイルを与えてください。

```
$ ./target/release/asm-simulator source.asm
```

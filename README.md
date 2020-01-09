# netoraji
Netoraji(ねとらじ) client for Emacs

## 概要
ねとらじヘッドライン取得＆視聴が出来るEmacsメジャーモードです。MPlayerとs.elが必要です。

## スクリーンショット
![screenshot](Screenshot.png) 

## インストール＆起動
netoraji.elをロードパスの通ったディレクトリに配置し、(require 'netoraji)を評価します。M-x netoraji とすることで起動します。

## キーバインディング
| Key              | Description             |
|------------------|-------------------------|
| <kbd>g</kbd>     | ヘッドラインを更新する      |
| <kbd>l</kbd>     | 番組を視聴する             |
| <kbd>k</kbd>     | 視聴を停止する             |
| <kbd>+</kbd>     | 視聴中にボリュームを上げる   |
| <kbd>-</kbd>     | 視聴中にボリュームを下げる   |
| <kbd>n</kbd>     | 次の番組へカーソル移動      |
| <kbd>p</kbd>     | 前の番組へカーソル移動      |
| <kbd>t</kbd>     | 関連URLをewwブラウザで開く  |
| <kbd>q</kbd>     | netorajiを終了する       |

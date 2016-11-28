# simplenote2.el [![MELPA](http://melpa.org/packages/simplenote2-badge.svg)](http://melpa.org/#/simplenote2) [![MELPA Stable](http://stable.melpa.org/packages/simplenote2-badge.svg)](http://stable.melpa.org/#/simplenote2)

simplenote2.elは、オンラインメモサービス[Simplenote](http://simplenoteapp.com/)のEmacsクライアントである[simplenote.el](https://github.com/dotemacs/simplenote.el)の新バージョンです。旧バージョンからの主な改良点は、以下になります。

* Simplenote API ver.2を使用することにより、タグ、自動マージ等の機能が利用可能になったこと
* サーバーへのアクセスを非同期かつ並列に行うことにより、同期の高速化とブロックしないUIを実現したこと

**バージョン3.0.0以降、tabulated listベースの新しいブラウザ画面が導入されました。下記「リスト画面」を参照して下さい。**

## 必要なもの

Emacs 23以降が必要です。Linux上のEmacs 23.3、及びEmacs 24.3でテストしています。

## インストール

### MELPAからインストール

<kbd>M-x package-install [RET] simplenote2 [RET]</kbd>

### 手動インストール

[request-deferred](https://github.com/tkf/emacs-request)に依存しているため、まずこれをインストールして下さい。その後、`simplenote2.el`をダウンロードして、ロードパスの通っているディレクトリに置いて下さい。

## 設定

以下の設定を、`.emacs`に入れてください。

```.emacs
(require 'simplenote2)
(setq simplenote2-email "email@example.com")
(setq simplenote2-password "yourpassword")
(simplenote2-setup)
```

`simplenote2-email`と`simplenote2-password`は、Simplenoteのアカウントのものになります。これら (どちらか、または両方) は設定しないことも可能です。その場合は、最初にサーバにアクセスする際に対話的に入力を求められます。

サーバから取得したデータは、`simplenote2-directory`で指定されたディレクトリにキャッシュされます。デフォルトは`~/.simplenote2`です。

## 使用方法

主に、ブラウザ (ノート一覧) 画面を使う方法と、個々のノートを個別に同期させる方法があります。これらは排他的なものではなく、状況に応じて使用可能です。

### ブラウザ画面

`M-x simplenote2-browse`でブラウザ画面を表示します。最初に起動したときは、何も表示されません。`[Sync with server]`ボタン上で[Enter]を押す (または、`M-x simplenote2-sync-notes`を実行する) ことで、サーバから全てのノートを取得して一覧表示されます (ノート数に応じて、ある程度時間がかかります)。

各ノートのリンク上で[Enter]を押すことで、そのノートを編集できます。また、`[Create new note]`ボタンを押すことで、新規ノートを作成できます。編集した結果は、`[Sync with server]`ボタンを押すことでサーバと同期されます。また、このとき前回の同期以降のサーバ側の更新もローカルキャッシュに反映されます。

### リスト画面

tabulated listベースのもう1つのブラウザ画面で、`M-x simplenote2-list`で起動します。ブラウザ画面に比べ、シンプルですっきりとしたUIです。以下のキーバインドが利用可能です。

* `g`: サーバと同期 (`M-x simplenote2-sync-notes`と同じ)
* `a`: 新しいノートを作成
* `Enter`: 現在の行のノートを開く
* `d`: 現在の行のノートに削除マークを付ける
* `u`: 現在の行のノートの削除マークを外す
* `t`: タグによるフィルタリングを行う (`M-x simplenote2-filter-note-by-tag`と同じ)
* `^`: タグフィルタリングの条件 ("AND"/"OR") の切り替え
* `/`: 正規表現によるフィルタリングを行う

### 個別のノートの同期

個別のノートの同期には、以下のコマンドが利用可能です。これらはすべて、ブラウザ画面ではなく個別のノートを表示している状態で使用します。

`M-x simplenote2-create-note-from-buffer`

現在表示しているバッファのファイルを元に、サーバ上に新規ノートを作成します。成功すると、表示中のファイルは`simplenote2-directory`の下に移動され、元のファイルは削除されるので注意して下さい。

`M-x simplenote2-push-buffer`

現在表示しているバッファのノートの変更を、サーバ側に反映させます。同時に、サーバ側の変更も取得してローカルに反映します。`simplenote2-directory`以下の、既存のノートか新規ノートに対してのみ有効です。新規ノートの場合は、`M-x simplenote2-create-note-from-buffer`と同じ動作となります。

`M-x simplenote2-pull-buffer`

現在表示しているバッファのノートの、サーバ側の変更を取得してローカルに反映します。ローカルに変更がある場合は、先にそれをサーバ側に反映するかの問い合わせが表示されます。yesと答えると、`M-x simplenote2-push-buffer`と同じ動作となります。noの場合は、サーバ側の変更を反映する際にローカルの変更は破棄されるので注意して下さい。

## simplenote2の新規機能

### タグのサポート

各ノートに付いているタグは、ブラウザ画面に表示されます。`M-x simplenote2-filter-notes-by-tag`でタグによるノートのフィルタリングができます。このコマンドを実行すると、対話的にタグの入力を求められます。入力なしで[Enter]を押すまで、複数のタグを入力できます。既存のタグによる入力補完もサポートされます。入力を完了すると、入力したタグのいずれかを含むノートだけが表示されます。`C-u M-x simplenote2-filter-notes-by-tag`でフィルタリングを解除します。

フィルタをデフォルトで設定することも可能です。その場合は、カスタマイズ変数`simplenote2-filter-note-tag-list`を`.emacs`内で以下のように設定して下さい。

```.emacs
(setq simplenote2-filter-note-tag-list '("tag1" "tag2" "tag3"))
```

### Pinned to top

"Pinned to top"属性がセットされたノートは、ブラウザ画面のノート一覧の一番上に表示されます。

`M-x simplenote2-set-pinned`で、現在表示しているノートに"Pinned to top"属性をセットできます。`C-u M-x simplenote2-set-pinned`で解除します。

### Markdown formatted

"Markdown formatted"属性がセットされたノートを開くときは、カスタマイズ変数`simplenote2-markdown-notes-mode`で指定されたメジャーモードが使用されます。デフォルトは、(通常のノートと同じ) `text-mode`となっています。`markdown-mode`で開きたいときは、この変数を`markdown-mode`に設定して下さい。

`M-x simplenote2-set-markdown`で、現在表示しているノートに"Markdown formatted"属性をセットできます。`C-u M-x simplenote2-set-markdown`で解除します。

### 新規ノートへのタグ・属性の設定

以下のように、カスタマイズ変数 `simplenote2-create-note-hook` を用いて、新規ノートを作成する際にデフォルトでタグや属性をセットすることができます。

```.emacs
(add-hook 'simplenote2-create-note-hook
	  (lambda ()
	    (simplenote2-set-markdown)
	    (simplenote2-add-tag "tag1")))
```

### Simplenoteノートバッファのマイナーモード

ブラウザ画面からノートを開いたとき、マイナーモード `simplenote2-note-mode` が設定されます。このマイナーモードはデフォルトでは何もしませんが、以下のようにキーバインドなどを設定するのに使用できます。

```.emacs
(add-hook 'simplenote2-note-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-t") 'simplenote2-add-tag)
            (local-set-key (kbd "C-c C-c") 'simplenote2-push-buffer)
            (local-set-key (kbd "C-c C-d") 'simplenote2-pull-buffer)))
```

## 更新履歴

version 3.0.0 (2016-11-27)

* New: tabulated listベースの新しいブラウザ画面を導入
* Fix: 同期が必要なノートが多いときに全てのノートの同期が行えない不具合を修正

version 2.2.2 (2015-04-05)

* New: Simplenoteノートのバッファにマイナーモード `simplenote2-note-mode` が設定されるようにした
* Fix: 特定の文字を含むノートの更新が失敗する不具合を修正 (JSONデータのURIエンコードの方法が間違っていた)

version 2.2.1 (2015-03-17)

* Fix: 空のノートをブラウザ画面で表示しようとしたときにエラーが発生するのを修正

version 2.2.0 (2015-03-04)

* New: 新規ノートへの (サーバーに同期する前の) タグやその他の属性の設定をサポート
* New: カスタマイズ変数 `simplenote2-create-note-hook` を追加
* Modify: 新規ノートをサーバーと同期する際のバッファ制御を改善
* Fix: `M-x simplenote2-set-markdown` 実行時、即座にメジャーモードが切り替わらなかった不具合を修正

version 2.1.1 (2015-02-25)

* Fix: URIエンコードを行っていなかったことにより、いくつかの文字が正しく同期できなかった問題を修正

version 2.1.0 (2015-02-22)

* New: タグ、その他の属性の編集をサポート
* New: カスタマイズ変数 `simplenote2-markdown-notes-mode` を追加
* Modify: ブラウザ画面の表示とUIを少し改善
* Fix: いくつかのマイナーバグの修正

version 2.0.0 (2015-02-16)

* simplenote2.elとしての初期リリース

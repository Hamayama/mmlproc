# mmlproc

## 概要
- Gauche で MML(Music Macro Language) の文字列を解釈して、  
  PCMデータに変換するためのモジュールです。  
  結果をwavファイルとして出力可能です。


## インストール方法
- mmlproc.scm を Gauche でロード可能なフォルダにコピーします。  
  (例えば (gauche-site-library-directory) で表示されるフォルダ等)

- C言語の開発環境(WindowsではMinGW(32bit)のGCCが必要)があれば、  
  以下の手順でDLLを作成してインストールすることもできます。  
  (Windowsではコマンドプロンプトでbashを起動して、cdでmmlprocのフォルダに  
   移動してから実行します)
  ```
     ./configure     (Makefileを生成します)
     make            (コンパイルしてDLLを作成します)
     make install    (Gaucheのライブラリフォルダにインストールします)
     make check      (テストを実行します)
  ```
  DLLがあると高速に変換が行えるようになります。


## 使い方
```
  (use mmlproc)                       ; モジュールをロードします
  (define pcmdata (mml->pcm "cdefg")) ; MML文字列をPCMデータ(s16vector)に
                                      ; 変換します
  (call-with-output-file "test.wav" (cut write-wav pcmdata <>))
                                      ; PCMデータ(s16vector)をwavファイルに
                                      ; 変換して出力ポートに書き出します

  サンプリングレートは変数 mml-sample-rate で取得/設定できます。
  (デフォルトは22050(Hz)です)

  DLLがインストールされていれば、高速に変換が行えます。
  DLLがロードされているかどうかは、mml-dll-loaded? でチェックできます。
```


## MML(Music Macro Language)について
- MMLは "t120 !c0 @0 v120 cdefg" のように文字列で音楽を記述します。  
  本モジュールで使用可能なMMLの命令は、以下の通りです。  
  (MMLには方言が多数あり、本モジュールも独自文法になっています)  
  アルファベット、数字、記号は、すべて半角です。  
  アルファベットの大文字小文字の区別はありません。  
  割り当てのない記号や空白は、無視されます。
```
(1)テンポ(全チャンネル共通)
   テンポは、t120 のように指定します。数字は20-1200までです。デフォルトは120です。
   数字は 1分間に演奏する4分音符の数になります。

(2)チャンネル
   チャンネルは、!c0 のように指定します。数字は0-7までです。デフォルトは0です。
   複数のチャンネルは同時に演奏されます。

(3)音色(チャンネルごと)
   音色は、@0 のように指定します。指定できる数字は以下の通りです。デフォルトは0です。
     =0:方形波
     =1:正弦波
     =2:のこぎり波
     =3:三角波
     =4:ホワイトノイズ
     =500:ピアノ(仮)
     =501:オルガン(仮)
     =502:ギター(仮)

(4)音符(チャンネルごと)
   音符は、c+4. のように 音の高さ、変化記号(シャープ/フラット/ナチュラル)、
   音の長さ、符点(音長を1.5倍します) の順に記述します。
   音の高さ以外は省略可能です。

   音の高さは、cdefgabの各文字が、ドレミファソラシに対応します。休符は r になります。
   変化記号は、シャープが + になります。フラットが - になります。
   ナチュラルが = になります。( + のかわりに #、 = のかわりに * を使うこともできます)

   音の長さは、4分音符が4、8分音符が8 のようになります。
   符点(.)をつけると音長を1.5倍します。
   数字の前に%をつけると絶対音長の指定となり、
   4分音符が48、8分音符が24 のようになります(c%48とc4は同じ意味)。
   音の長さを省略した場合は、後述のデフォルト音長(Lコマンド)で指定した長さになります。
   (デフォルトでは4分音符の長さになります)

   また、タイ/スラーは、& 記号で音符をつないで表現します(c4&c4等)。
   また、タイは、^ 記号で音長だけを連結することもできます(c4^4等)。

(5)オクターブ(チャンネルごと)
   オクターブは、o4のように指定します。数字は0-8までです。デフォルトは4です。
   また、< で 1オクターブ下げます。 > で 1オクターブ上げます。
   また、!o で < と > の意味を逆にします(全チャンネル共通)。
   (現状は、ある程度高い音や低い音が うまく再生できません
    (オクターブがおおよそ3～6の間である必要があります))

(6)調号(チャンネルごと)
   調号は、!+cfg のように シャープをつける音符を複数指定します。
   また、!-eab のように フラットをつける音符を複数指定します。
   また、!=cfg のように ナチュラルに戻す音符を複数指定します。
   ( + のかわりに #、 = のかわりに * を使うこともできます)
   (演奏の音符と区別するため、この命令の終わりには空白を入れてください)

(7)ボリューム最大値(全チャンネル共通)
   ボリューム最大値は、!v127 のように指定します。数字は1-1000までです。デフォルトは127です。
   例えば、!v16 とすると チャンネル音量が 0-16 までであるようなMMLを実行できます。

(8)チャンネル音量(チャンネルごと)
   チャンネル音量は、v120 のように指定します。
   数字は0-ボリューム最大値(!vで指定した値)までです。デフォルトは120です。

(9)ベロシティ(チャンネルごと)
   ベロシティ(音の強さ)は、k100 のように指定します。数字は0-127までです。デフォルトは100です。
   (現状は、ベロシティは未対応です。記述しても無視されます)

(10)デフォルト音長(チャンネルごと)
    デフォルト音長は、L4 のように指定します。音符の音の長さを省略した場合に この値が使われます。
    数字は、4分音符が4、8分音符が8 のようになります。デフォルトは4です。
    符点(.)をつけると音長を1.5倍します。
    また、数字の前に%をつけると絶対音長の指定となり、
    4分音符が48、8分音符が24 のようになります(L%48とL4は同じ意味)。

(11)発音割合指定(チャンネルごと)
    発音割合指定は、q8 のように指定します。
    数字は1-8で、音長の8分のいくつまで発音するかという意味になります。デフォルトは8です。

(12)ループ指定(チャンネルごと)
    ループ指定は、[ abc : de ]2 のように指定します。
    [ ] の中を、数字の回数だけ実行します。数字を省略した場合は 2回実行します。
    (2より小さい数を指定した場合も 2回実行します)
    また、: があると、最後の1回だけは : のところでループを終了します。

    また、ループは入れ子にすることが可能です。
    例えば、[cd:e [fg]3 ] を実行すると cde fg fg fg cd が実行されます。

    注意点として、ループ指定はチャンネルごとであるため、
    複数のチャンネルをまたぐようなループは実行できません。
```


## 注意事項
1. 演奏1秒あたり22050個の音声データを計算するため それなりに時間がかかります。


## 参考情報
1. Schemeコードバトン  
   https://gist.github.com/koguro/297312  
   (wavファイル出力の部分を参考にしました)


## 環境等
- 以下の環境で動作を確認しました。
  - OS
    - Windows XP Home SP3
    - Windows 8 (64bit)
  - 言語
    - Gauche v0.9.4

## 履歴
- 2014-11-1 v1.00 (初版)
- 2014-11-3 v1.01 音符追加処理見直し等
- 2014-11-3 v1.02 サンプリングレートを設定可能に
- 2014-11-3 v1.03 mml-sample-rateのrenameをやめた(test-moduleで検出)
- 2014-11-3 v1.04 音声データ計算処理見直し
- 2014-11-5 v1.05 音声データ計算処理見直し  
  (sin → %sin のように実数用関数に変更。doループをvector-tabulateに変更)
- 2014-11-6 v1.06 タイ記号(&)の前の 空白、タブ、改行 を許可
- 2014-11-7 v1.07 コメント修正のみ
- 2014-11-8 v1.08 C言語のDLLを使用して計算できるようにした  
  (DLLが存在しない場合は今まで通り計算)
- 2014-11-8 v1.09 コメント修正のみ


(2014-11-8)

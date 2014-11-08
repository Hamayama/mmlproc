;; -*- coding: utf-8 -*-
;;
;; mmlproc.scm
;; 2014-11-8 v1.09
;;
;; ＜内容＞
;;   Gauche で MML(Music Macro Language) の文字列を解釈して、
;;   PCMデータに変換するためのモジュールです。
;;   結果をwavファイルとして出力可能です。
;;
;; ＜インストール方法＞
;;   mmlproc.scm を Gauche でロード可能なフォルダにコピーします。
;;   (例えば (gauche-site-library-directory) で表示されるフォルダ等)
;;
;;   C言語の開発環境(WindowsではMinGW(32bit)のGCCが必要)があれば、
;;   以下の手順でDLLを作成してインストールすることもできます。
;;   (Windowsではコマンドプロンプトでbashを起動して、cdでmmlprocのフォルダに
;;    移動してから実行します)
;;
;;      ./configure     (Makefileを生成します)
;;      make            (コンパイルしてDLLを作成します)
;;      make install    (Gaucheのライブラリフォルダにインストールします)
;;      make check      (テストを実行します)
;;
;;   DLLがあると高速に変換が行えるようになります。
;;
;; ＜使い方＞
;;   (use mmlproc)                       ; モジュールをロードします
;;   (define pcmdata (mml->pcm "cdefg")) ; MML文字列をPCMデータ(s16vector)に
;;                                       ; 変換します
;;   (call-with-output-file "test.wav" (cut write-wav pcmdata <>))
;;                                       ; PCMデータ(s16vector)をwavファイルに
;;                                       ; 変換して出力ポートに書き出します
;;
;;   サンプリングレートは変数 mml-sample-rate で取得/設定できます。
;;   (デフォルトは22050(Hz)です)
;;
;;   DLLがインストールされていれば、高速に変換が行えます。
;;   DLLの有無は、mml-dll-loaded? でチェックできます。
;;
;; ＜注意事項＞
;;   (1)演奏1秒あたり22050個の音声データを計算するため それなりに時間がかかります。
;;
(define-module mmlproc
  (use gauche.sequence) ; find-index用
  (use gauche.uvector)
  (use math.const)
  (use math.mt-random)
  (use srfi-13)         ; string-downcase用
  (use binary.pack)
  (export
    test-mmlproc
    mml-dll-loaded?
    mml-sample-rate
    mml->pcm
    write-wav))
(select-module mmlproc)

;; DLLのロード
(define dll-loaded
  (guard (exc ((<error> exc) #f))
    (dynamic-load "mmlproc")
    #t))

;; DLLがロードされているかのチェック
(define (mml-dll-loaded?) dll-loaded)

;; 定数
(define max-ch 8)              ; 最大チャンネル数(増やすと音が小さくなる)
(define mml-sample-rate 22050) ; サンプリングレート(Hz)

;; 乱数
(define mr-twister (make <mersenne-twister> :seed (sys-time)))

;; テンポ変更情報
;;    pos      音声データ位置(単位は絶対音長(4分音符が48になる)
;;    rtime    実時間(sec)
;;    val      テンポの値(1分間に演奏する4分音符の数)
(define-class <tempochg> () (pos rtime val))

;; タイ情報
;;    flag     タイのフラグ
;;    note     タイの音符
;;    length   タイの音長
(define-class <tieinfo>  () (flag note length))

;; ループ情報
;;    begin    ループ開始(リスト)
;;    end      ループ終了(リスト)
;;    counter  ループ回数(リスト)
(define-class <loopinfo> () (begin end counter))


;; MML文字列をPCMデータ(s16vector)に変換する
(define (mml->pcm mmlstr)
  (define mmlvec    #f)               ; MML(ベクタ)
  (define pcmdata   #f)               ; PCMデータ(s16vector)(これが戻り値になる)
  (define pos (make-vector max-ch 0)) ; 音声データ位置(ベクタ)(チャンネルごと)(単位は絶対音長(4分音符が48になる))
  (define tempolist '())              ; テンポ変更情報(リスト)(全チャンネル共通)

  ;; 実時間テーブル作成(内部処理用)
  ;; (「テンポ変更位置の実時間」を計算して保存しておく)
  (define (make-time-table)
    (let ((rtime    0)    ; 実時間(sec)
          (poslast  0)    ; 音声データ位置保存用(単位は絶対音長(4分音符が48になる))
          (tempo    120)) ; テンポ(1分間に演奏する4分音符の数)
      (for-each
       (lambda (tchg)
         (set! rtime           (+ rtime (/. (/. (* (- (~ tchg 'pos) poslast) 60) 48) tempo)))
         (set! (~ tchg 'rtime) rtime)
         (set! poslast         (~ tchg 'pos))
         (set! tempo           (~ tchg 'val)))
       tempolist)))

  ;; 実時間取得(内部処理用)
  ;; (音声データ位置から実時間を計算して返す)
  ;; (事前に計算した「テンポ変更位置の実時間」を利用する)
  (define (get-real-time pos)
    (let ((rtime    0)    ; 実時間(sec)
          (poslast  0)    ; 音声データ位置保存用(単位は絶対音長(4分音符が48になる))
          (tempo    120)  ; テンポ(1分間に演奏する4分音符の数)
          (tchglast #f))
      ;; 直前のテンポ変更位置を探す
      (set! tchglast (find (lambda (tchg) (< (~ tchg 'pos) pos)) (reverse tempolist)))
      (when tchglast
        (set! rtime   (~ tchglast 'rtime))
        (set! poslast (~ tchglast 'pos))
        (set! tempo   (~ tchglast 'val)))
      ;; 実時間を計算
      (set! rtime (+ rtime (/. (/. (* (- pos poslast) 60) 48) tempo)))
      rtime))

  ;; 音色生成関数(内部処理用)(DLLありのときは未使用)
  (define (make-progfunc prog)
    (case prog
      ;; 方形波
      ((0)   (lambda (t phase) (if (> (%sin phase) 0) 1 -1)))
      ;; 正弦波
      ((1)   (lambda (t phase) (%sin phase)))
      ;; のこぎり波
      ((2)   (lambda (t phase) (- (* (mod phase (* 2 pi)) 1/pi) 1)))
      ;; 三角波
      ((3)   (lambda (t phase) (* 2 (%asin (%sin phase)) 1/pi)))
      ;; ホワイトノイズ
      ((4)   (lambda (t phase) (- (* (mt-random-real0 mr-twister) 2) 1)))
      ;; ピアノ(仮)
      ((500) (lambda (t phase) (* 1.3 (if (> (%sin phase) 0) 1 -1) (%exp (* -5 t)))))
      ;; オルガン(仮)
      ((501) (lambda (t phase) (* (if (> (%sin phase) 0) 1 -1) 13 t (%exp (* -5 t)))))
      ;; ギター(仮)
      ((502) (lambda (t phase) (* 5 (%cos (+ phase (%cos (* phase 0.5)) (%cos (* phase 2)))) (%exp (* -5 t)))))
      ;; 方形波
      (else  (lambda (t phase) (if (> (%sin phase) 0) 1 -1)))))

  ;; 音符追加(内部処理用)
  (define (add-note ch note nlength1 nlength2 prog volume pass-no)
    (when (= pass-no 2)
      ;; DLLの有無で場合分け
      (if (mml-dll-loaded?)
        ;; DLLありのとき
        (calc-pcmdata pcmdata mml-sample-rate max-ch
                      note nlength1 nlength2 prog volume
                      (get-real-time (~ pos ch))
                      (get-real-time (+ (~ pos ch) nlength1)))
        ;; DLLなしのとき
        (begin
          (let ((rtime1   0)   ; 音符開始位置の実時間(sec)
                (rtime2   0)   ; 音符終了位置の実時間(sec)
                (nlen1    0)   ; 音長  (単位は実時間(sec)xサンプリングレート(Hz))
                (nlen2    0)   ; 発音長(単位は実時間(sec)xサンプリングレート(Hz))
                (freq     0)   ; 音符の周波数(Hz)
                (phase-c  0)   ; 定数キャッシュ用
                (amp-c    0)   ; 定数キャッシュ用
                (pos-int  0)   ; 定数キャッシュ用
                (len-int  0)   ; 定数キャッシュ用
                (rsample  0)   ; 定数キャッシュ用
                (rnlen2   0)   ; 定数キャッシュ用
                (progfunc #f)) ; 音色生成関数
            ;; 音長計算
            ;; (音符の途中でテンポが変わることを考慮する)
            (set! rtime1   (get-real-time (~ pos ch)))
            (set! rtime2   (get-real-time (+ (~ pos ch) nlength1)))
            (set! nlen1    (* mml-sample-rate (- rtime2 rtime1)))
            ;; 発音長計算
            (set! nlen2    (if (> nlength1 0) (/. (* nlen1 nlength2) nlength1) 0))
            ;; 音符の周波数計算
            (set! freq     (* 13.75 (%expt 2 (/. (- note 9) 12))))
            ;; 定数を先に計算しておく
            (set! phase-c  (* 2 pi freq))
            (set! amp-c    (/. (/. (* 32767 volume) 127) max-ch))
            (set! pos-int  (floor->exact (* mml-sample-rate rtime1)))
            (set! len-int  (floor->exact nlen1))
            (set! rsample  (/. 1 mml-sample-rate))
            (set! rnlen2   (/. 1 nlen2))
            ;; 音色生成関数を取得
            (set! progfunc (make-progfunc prog))
            ;; 音声データの値を計算
            ($ s16vector-copy!   pcmdata pos-int
               $ s16vector-add!  (uvector-alias <s16vector> pcmdata pos-int (+ pos-int len-int))
               $ vector-tabulate len-int
               (lambda (i)
                 (let* ((t     (* i rsample))                   ; 時間(sec)
                        (phase (* phase-c t))                   ; 位相(ラジアン)
                        (wave  (clamp (progfunc t phase) -1 1)) ; 波形(-1～1まで)
                        (fade  (cond ((= nlen2 0)         1)    ; フェードアウト割合(0-1まで)
                                     ((< i (* 0.8 nlen2)) 1)
                                     ((< i nlen2)         (* 5 (- 1 (* i rnlen2))))
                                     (else                0))))
                   (floor->exact (* amp-c wave fade)))))))))
    ;; 音声データ位置を計算
    (set! (~ pos ch) (+ (~ pos ch) nlength1)))

  ;; 休符追加(内部処理用)
  (define (add-rest ch nlength1)
    ;; 音声データ位置を計算
    (set! (~ pos ch) (+ (~ pos ch) nlength1)))

  ;; コンパイル(内部処理用)
  (define (compile mmlvec pass-no)
    (let ((mmllen     (vector-length mmlvec))
          ;; 全体状態
          (ch         0)   ; チャンネル選択
          (tempo      120) ; テンポ(1分間に演奏する4分音符の数)
          (octchg     0)   ; オクターブ記号変更
          (volmax     127) ; ボリューム最大値
          ;; 各チャンネルの状態
          (prog       (make-vector max-ch 0)) ; 音色      (ベクタ)
          (volume     (make-vector max-ch 0)) ; 音量      (ベクタ)
          (velocity   (make-vector max-ch 0)) ; ベロシティ(ベクタ)(将来用)
          (alength    (make-vector max-ch 0)) ; 音長      (ベクタ)
          (qtime      (make-vector max-ch 0)) ; 発音割合  (ベクタ)
          (octave     (make-vector max-ch 0)) ; オクターブ(ベクタ)
          (sharp      (make-vector max-ch 0)) ; 調号      (2次元ベクタ)
          (tie        (make-vector max-ch 0)) ; タイ状態  (ベクタ)
          (loop       (make-vector max-ch 0)) ; ループ状態(ベクタ)
          ;; その他の変数
          (c          0)   ; 文字
          (c2         0)   ; 文字
          (note       0)   ; 音符
          (nlength1   0)   ; 音長
          (nlength2   0))  ; 発音長
      ;; 全体状態の初期化
      (if (= pass-no 1) (set! tempolist '())) ; テンポ変更情報(リスト)
      ;; 各チャンネルの状態の初期化
      (do ((i 0 (+ i 1)))
          ((>= i max-ch) #f)
        (set! (~ prog     i) 0)
        (set! (~ volume   i) 120)
        (set! (~ velocity i) 100)
        (set! (~ alength  i) 48)
        (set! (~ qtime    i) 8)
        (set! (~ octave   i) 4)
        (set! (~ sharp    i) (make-vector 7 0)) ; cdefgabの7個分の調号
        (set! (~ tie      i) (make <tieinfo>))
        (set! (~ tie      i  'flag)    #f)      ; タイのフラグ
        (set! (~ tie      i  'note)    0)       ; タイの音符
        (set! (~ tie      i  'length)  0)       ; タイの音長
        (set! (~ loop     i) (make <loopinfo>))
        (set! (~ loop     i  'begin)   '())     ; ループ開始(リスト)
        (set! (~ loop     i  'end)     '())     ; ループ終了(リスト)
        (set! (~ loop     i  'counter) '())     ; ループ回数(リスト)
        (set! (~ pos      i) 0))                ; 音声データ位置(ベクタ)
      ;; MMLの解析
      (do ((i 0 i))
          ((>= i mmllen) #f)
        ;; 1文字取り出す
        (set! c (~ mmlvec i))
        (inc! i)
        (case c
          ;; 音符または休符のとき
          ((#\c #\d #\e #\f #\g #\a #\b #\r)
           ;; 音の高さを計算
           (if (eqv? c #\r)
             ;; 休符は音の高さなし
             (set! note 0)
             (begin
               ;; 音符の音の高さを数値化
               (set! note (find-index (cut eqv? c <>) #(#\c 0 #\d 0 #\e #\f 0 #\g 0 #\a 0 #\b)))
               ;; オクターブを加算
               (set! note (+ note (* (+ (~ octave ch) 1) 12)))
               ;; シャープ、フラット、ナチュラルがあるときはそれを計算
               (case (~ mmlvec i)
                 ((#\+ #\#) (inc! i) (inc! note))
                 ((#\-)     (inc! i) (dec! note))
                 ((#\= #\*) (inc! i))
                 ;; その他のときは調号の分を計算
                 (else
                  (let1 val (find-index (cut eqv? c <>) #(#\c #\d #\e #\f #\g #\a #\b))
                    (if val (set! note (+ note (~ sharp ch val)))))))
               ;; 音の高さ範囲チェック
               (set! note (clamp note 0 127))))
           ;; 音長の計算
           (case (~ mmlvec i)
             ;; 絶対音長があるときは絶対音長を取得
             ((#\%)
              (inc! i)
              (receive (val i2) (get-value mmlvec i 0)
                (set! i i2)
                (set! nlength1 (clamp val 0 1000))))
             (else
              ;; 音長があるときは音長を取得
              (receive (val i2) (get-value mmlvec i -1)
                (set! i i2)
                (if (> val 1000) (set! val 1000))
                (cond
                 ((= val 0) (set! nlength1 0))
                 ((> val 0) (set! nlength1 (/. (* 48 4) val)))
                 (else      (set! nlength1 (~ alength ch)))))))
           ;; 付点があるときは音長を1.5倍
           (when (eqv? (~ mmlvec i) #\.)
             (inc! i)
             (set! nlength1 (/. (* nlength1 3) 2)))
           ;; 発音長の計算
           (set! nlength2 (/. (* nlength1 (~ qtime ch)) 8))
           ;; スラーの処理
           (when (and (~ tie ch 'flag) (not (= (~ tie ch 'note) note)) (> note 0))
             ;; 音符または休符を追加
             (if (> (~ tie ch 'note) 0)
               ;; 音符追加
               (add-note ch (~ tie ch 'note) (~ tie ch 'length) 0 (~ prog ch) (~ volume ch) pass-no)
               ;; 休符追加
               (add-rest ch (~ tie ch 'length)))
             ;; タイを解除
             (set! (~ tie ch 'flag)   #f)
             (set! (~ tie ch 'note)   0)
             (set! (~ tie ch 'length) 0))
           ;; タイ記号(&)の前の 空白、タブ、改行 をスキップ
           (set! i (skip-space mmlvec i))
           (case (~ mmlvec i)
             ;; タイまたはスラーのとき
             ((#\&)
              (inc! i)
              ;; タイのフラグを立てて、処理は次回にまわす
              (set! (~ tie ch 'flag)   #t)
              (if (> note 0) (set! (~ tie ch 'note) note))
              (set! (~ tie ch 'length) (+ (~ tie ch 'length) nlength1)))
             (else
              ;; タイの処理
              (when (~ tie ch 'flag)
                ;; 音符を確定する
                (set! note (~ tie ch 'note))
                (set! nlength1 (+ (~ tie ch 'length) nlength1))
                (set! nlength2 (+ (~ tie ch 'length) nlength2))
                ;; タイを解除
                (set! (~ tie ch 'flag)   #f)
                (set! (~ tie ch 'note)   0)
                (set! (~ tie ch 'length) 0))
              ;; 音符または休符を追加
              (if (> note 0)
                ;; 音符追加
                (add-note ch note nlength1 nlength2 (~ prog ch) (~ volume ch) pass-no)
                ;; 休符追加
                (add-rest ch nlength1)))))

          ;; 拡張コマンドのとき
          ((#\!)
           (set! c2 (~ mmlvec i))
           (case c2
             ;; チャンネル切替(0-(MAX_CH-1))
             ((#\c)
              (inc! i)
              (receive (val i2) (get-value mmlvec i 0)
                (set! i i2)
                (set! ch (clamp val 0 (- max-ch 1)))))
             ;; オクターブ記号変更(トグル)
             ((#\o)
              (inc! i)
              (set! octchg (if (= octchg 0) 1 0)))
             ;; ボリューム最大値(1-1000)
             ((#\v)
              (inc! i)
              (receive (val i2) (get-value mmlvec i 0)
                (set! i i2)
                (set! volmax (clamp val 1 1000))))
             ;; 調号指定
             ((#\+ #\# #\- #\= #\*)
              (inc! i)
              (let ((done #f)
                    (val  0))
                (while (not done)
                  (set! val (find-index (cut eqv? (~ mmlvec i) <>) #(#\c #\d #\e #\f #\g #\a #\b)))
                  (if val
                    (begin
                      (inc! i)
                      (case c2
                        ((#\+ #\#) (set! (~ sharp ch val) 1))   ; シャープ
                        ((#\-)     (set! (~ sharp ch val) -1))  ; フラット
                        ((#\= #\*) (set! (~ sharp ch val) 0)))) ; ナチュラル
                    (set! done #t)))))))

          ;; 通常コマンドのとき
          ;; テンポ切替(20-1200)
          ((#\t)
           (receive (val i2) (get-value mmlvec i 0)
             (set! i i2)
             (set! tempo (clamp val 20 1200)))
           (when (= pass-no 1)
             (let1 tchg (make <tempochg>)
               (set! (~ tchg 'pos)   (~ pos ch)) ; テンポ変更位置(単位は絶対音長(4分音符が48になる))
               (set! (~ tchg 'val)   tempo)      ; テンポ変更値(1分間に演奏する4分音符の数)
               (set! (~ tchg 'rtime) 0)          ; テンポ変更位置の実時間(sec)(これは後で計算する)
               (push! tempolist tchg))))
          ;; チャンネル音量(0-vol_max)
          ((#\v)
           (receive (val i2) (get-value mmlvec i 0)
             (set! i i2)
             (set! (~ volume ch) (/. (* (clamp val 0 volmax) 127) volmax))))
          ;; ベロシティ(0-127)
          ((#\k)
           (receive (val i2) (get-value mmlvec i 0)
             (set! i i2)
             (set! (~ velocity ch) (clamp val 0 127))))
          ;; 音色切替(0-1000)
          ((#\@)
           (receive (val i2) (get-value mmlvec i 0)
             (set! i i2)
             (set! (~ prog ch) (clamp val 0 1000))))
          ;; 音長指定(0-1000)
          ((#\l)
           (case (~ mmlvec i)
             ;; 絶対音長があるときは絶対音長を取得
             ((#\%)
              (inc! i)
              (receive (val i2) (get-value mmlvec i 0)
                (set! i i2)
                (set! (~ alength ch) (clamp val 0 1000))))
             (else
              ;; 音長があるときは音長を取得
              (receive (val i2) (get-value mmlvec i 0)
                (set! i i2)
                (if (= val 0)
                  (set! (~ alength ch) 0)
                  (set! (~ alength ch) (/. (* 48 4) (clamp val 0 1000)))))))
           ;; 付点があるときは音長を1.5倍
           (when (eqv? (~ mmlvec i) #\.)
             (inc! i)
             (set! (~ alength ch) (/. (* (~ alength ch) 3) 2))))
          ;; 発音割合指定(1-8)
          ((#\q)
           (receive (val i2) (get-value mmlvec i 0)
             (set! i i2)
             (set! (~ qtime ch) (clamp val 1 8))))
          ;; オクターブ指定(0-8)
          ((#\o)
           (receive (val i2) (get-value mmlvec i 0)
             (set! i i2)
             (set! (~ octave ch) (clamp val 0 8))))
          ;; オクターブ下げ(0-8)
          ((#\<)
           (if (= octchg 0)
             (if (> (~ octave ch) 0) (dec! (~ octave ch)))
             (if (< (~ octave ch) 8) (inc! (~ octave ch)))))
          ;; オクターブ上げ(0-8)
          ((#\>)
           (if (= octchg 0)
             (if (< (~ octave ch) 8) (inc! (~ octave ch)))
             (if (> (~ octave ch) 0) (dec! (~ octave ch)))))
          ;; ループ開始
          ((#\[)
           ;; ループ情報を生成
           (push! (~ loop ch 'begin)   i)
           (push! (~ loop ch 'end)     0)
           (push! (~ loop ch 'counter) -1))
          ;; ループ終了
          ((#\])
           ;; ループ有無のチェック
           (if (not (null? (~ loop ch 'begin)))
             (let1 loop-count (car (~ loop ch 'counter))
               (cond
                ;; ループ最終回のとき
                ((= loop-count -2)
                 ;; ループ終了位置へジャンプ
                 (set! i (car (~ loop ch 'end)))
                 ;; ループ情報を破棄
                 (pop! (~ loop ch 'begin))
                 (pop! (~ loop ch 'end))
                 (pop! (~ loop ch 'counter)))
                (else
                 ;; ループ初回のとき
                 (when (= loop-count -1)
                   ;; ループ回数取得
                   (receive (val i2) (get-value mmlvec i 0)
                     (set! i i2)
                     (set! loop-count (- (clamp val 2 100) 1)))
                   ;; ループ終了位置を保存
                   (set-car! (~ loop ch 'end) i))
                 ;; ループ先頭位置へジャンプ
                 (set! i (car (~ loop ch 'begin)))
                 ;; ループ回数を減らす
                 (dec! loop-count)
                 (if (<= loop-count 0) (set! loop-count -2))
                 (set-car! (~ loop ch 'counter) loop-count))))))
          ;; 最終回ループ脱出
          ((#\:)
           ;; ループ有無のチェック
           (if (not (null? (~ loop ch 'begin)))
             (let1 loop-count (car (~ loop ch 'counter))
               (cond
                ;; ループ最終回のとき
                ((= loop-count -2)
                 ;; ループ終了位置へジャンプ
                 (set! i (car (~ loop ch 'end)))
                 ;; ループ情報を破棄
                 (pop! (~ loop ch 'begin))
                 (pop! (~ loop ch 'end))
                 (pop! (~ loop ch 'counter)))))))
          ))))

  ;; MML内の空白、タブ、改行をスキップ(内部処理用)
  ;; (最終の検索位置を返す)
  (define (skip-space mmlvec i)
    (let1 mmllen (vector-length mmlvec)
      (while
        (and (< i mmllen)
             (case (~ mmlvec i)
              ((#\space #\tab #\newline #\return)
               (inc! i))
              (else #f))))
      i))

  ;; MML内の数値を取得(内部処理用)
  ;; (数値と最終の検索位置を、多値で返す)
  (define (get-value mmlvec i errval)
    (let ((mmllen (vector-length mmlvec))
          (start  i)
          (done   #f))
      (if (not (digit->integer (~ mmlvec i)))
        (values errval i)
        (begin
          (inc! i)
          (while (not done)
            (if (or (>= i mmllen) (not (digit->integer (~ mmlvec i))))
              (set! done #t)
              (inc! i)))
          (values (string->number (vector->string mmlvec start i)) i)))))

  ;; MML文字列の前処理(内部処理用)
  (define (preprocess mmlstr)
    (let* ((mmlvec (string->vector mmlstr))
           (mmllen (vector-length mmlvec))
           (c      0)
           (start  0)
           (ch     0)
           (mmlch  (make-vector max-ch "")))
      ;; MMLを小文字に変換
      (set! mmlstr (string-downcase mmlstr))
      ;; MMLをチャンネルごとに分解
      (do ((i 0 i))
          ((>= i mmllen) #f)
        ;; 1文字取り出す
        (set! c (~ mmlvec i))
        (inc! i)
        ;; チャンネル切替のとき
        (when (and (eqv? c #\!) (eqv? (~ mmlvec i) #\c))
          (inc! i)
          (receive (val i2) (get-value mmlvec i 0)
            (set! (~ mmlch ch) (string-append (~ mmlch ch) (string-copy mmlstr start (- i 2))))
            (set! ch           (clamp val 0 (- max-ch 1)))
            (set! start        (- i 2))
            (set! i            i2))))
      (set! (~ mmlch ch) (string-append (~ mmlch ch) (string-copy mmlstr start)))
      ;; MMLを再構成(チャンネル順につなげる)
      (set! mmlstr "")
      (do ((i 0 (+ i 1)))
          ((>= i max-ch) #f)
        (set! mmlstr (string-append mmlstr (~ mmlch i))))
      ;; 「^」記号をタイと休符に置換
      (set! mmlstr (regexp-replace-all #/\^/ mmlstr "&r"))
      ;; 末尾に無効な文字列を追加(安全のため)
      (set! mmlstr (string-append mmlstr "||||"))
      ;; MML文字列を返す
      mmlstr))

  ;; 処理開始
  ;; MML文字列の前処理
  (set! mmlstr (preprocess mmlstr))
  ;; ベクタに変換
  (set! mmlvec (string->vector mmlstr))
  ;; コンパイル(パス1)
  ;; (テンポ変更情報を抽出して、必要な音声バッファのサイズを計算可能とする)
  (compile mmlvec 1)
  ;; テンポ変更情報のソート
  (set! tempolist (sort! tempolist < (cut ~ <> 'pos)))
  ;; 実時間テーブル作成
  (make-time-table)
  ;; 音声バッファの確保
  ;; (一番長いチャンネルのサイズを全体のサイズとする)
  (let ((pcmlen    0)
        (chdatalen 0))
    (do ((i 0 (+ i 1)))
        ((>= i max-ch) #f)
      (set! chdatalen (floor->exact (* mml-sample-rate (get-real-time (~ pos i)))))
      (if (< pcmlen chdatalen) (set! pcmlen chdatalen)))
    (inc! pcmlen) ; 誤差対策で+1
    (set! pcmdata (make-s16vector pcmlen 0)))
  ;; コンパイル(パス2)
  ;; (実際に音声データの値を計算して、音声バッファに格納する)
  (compile mmlvec 2)
  ;; 音声バッファを返す
  pcmdata)


;; PCMデータ(s16vector)をwavファイルに変換して出力ポートに書き出す
(define (write-wav pcmdata port)
  ;; ヘッダの書き出し
  (pack "A4VA4A4VvvVVvvA4V"
        (list "RIFF"                                  ; RIFFヘッダ
              (+ 36 (* (s16vector-length pcmdata) 2)) ; ファイルサイズ-8(byte)
              "WAVE"                                  ; WAVEヘッダ
              "fmt "                                  ; fmtチャンク
              16                                      ; fmtチャンクのサイズ(byte)
              1                                       ; 種別(=1:リニアPCM)
              1                                       ; CH数(=1:モノラル, =2:ステレオ)
              mml-sample-rate                         ; サンプリングレート(Hz)
              (* 2 mml-sample-rate)                   ; データ速度(byte/sec)
              2                                       ; ブロックサイズ(1サンプルのバイト数xCH数)
              16                                      ; 1サンプルのビット数(=8:8bit, =16:16bit)
              "data"                                  ; dataチャンク
              (* (s16vector-length pcmdata) 2))       ; dataチャンクのサイズ(byte)
        :output port)
  ;; データの書き出し
  (write-block pcmdata port 0 -1 'little-endian))


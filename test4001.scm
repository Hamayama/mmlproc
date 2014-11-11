;; -*- coding: utf-8 -*-
;;
;; mmlprocのテスト4(音色生成関数の追加)
;; 2014-11-11
;;
(add-load-path "." :relative)
(use mmlproc)
(use gauche.uvector)
(use gauche.time)
(use math.const)
(use math.mt-random)

;; 初期化
(define pcmdata #f)                 ; PCMデータ(s16vector)
(define wavfile "testdata4001.wav") ; wavファイル名

;; 乱数
(define mr-twister (make <mersenne-twister> :seed (sys-time)))


;; 音色生成関数の追加
(hash-table-put!
 mml-progfunc-table    ; 音色生成関数のハッシュテーブル
 900                   ; 音色番号
 (lambda (t phase)     ; 音色生成関数(FM変調 x 指数関数)
   (let ((ac     1)    ;   キャリア振幅
         (am     1)    ;   モジュレータ振幅
         (fratio 3.5)) ;   周波数比(=モジュレータ周波数/キャリア周波数)
     (* ac (%sin (+ phase (* am (%sin (* phase fratio))))) (%exp (* -5 t))))))


;; MML文字列をPCMデータに変換
(print "mml->pcm")
(time (set! pcmdata (mml->pcm "!c0 @900 cdefgab>c" #f)))
(newline)

;; PCMデータをwavファイルに変換して出力
(print "create " wavfile)
(time (call-with-output-file wavfile (cut write-wav pcmdata <>)))

;; wavファイルの再生(Windowsの場合)
;(sys-system wavfile)


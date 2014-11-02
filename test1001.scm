;; -*- coding: utf-8 -*-
;;
;; mmlprocのテスト1
;; 2014-11-2
;;
(add-load-path "." :relative)
(use mmlproc)
(use gauche.uvector)
(use gauche.time)

;; 定数
(define pcmdata #f)             ; PCMデータ(s16vector)
(define wavfile "testdata.wav") ; wavファイル名

;; MML文字列をPCMデータに変換
(print "mml->pcm")
(time (set! pcmdata (mml->pcm
  "!c0 @500 >cc  | gg | aa | g&r | ff  | ee   | de32d32c32d16.e16 | c2  | \
   !c1 @500 <c>c | ec | fc | ec  | d<b | >c<a | fg                | c2> | "
  )))
(newline)

;; PCMデータをwavファイルに変換して出力
(print "create " wavfile)
(time (call-with-output-file wavfile (cut write-wav pcmdata <>)))

;; wavファイルの再生(Windowsの場合)
;(sys-system "test.wav")


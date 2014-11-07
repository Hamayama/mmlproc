;; -*- coding: utf-8 -*-
;;
;; mmlprocのテスト3(時間測定用)
;; 2014-11-5
;;
(add-load-path "." :relative)
(use mmlproc)
(use gauche.uvector)
(use gauche.time)

;; 初期化
(define pcmdata #f)                 ; PCMデータ(s16vector)
(define wavfile "testdata3001.wav") ; wavファイル名

;; MML文字列をPCMデータに変換
(print "mml->pcm")
(time (set! pcmdata (mml->pcm
  "!c0 @500 [v120o3cdefgab>c< v50 o3cccccccc   ]10 \
   !c1 @501 [v50 o4cccccccc   v120o4cdefgab>c< ]10 \
   !c2 @502 [v50 o3cccccccc   v50 o3cccccccc   ]10 "
  )))
(newline)

;; PCMデータをwavファイルに変換して出力
(print "create " wavfile)
(time (call-with-output-file wavfile (cut write-wav pcmdata <>)))

;; wavファイルの再生(Windowsの場合)
;(sys-system wavfile)


;; 時間測定結果
;; (A)v1.04
;(time (set! pcmdata (mml->pcm "!c0 @500 [v120o3cdefgab>c< v50 o3eeeeeee ...
; real  13.016
; user  12.687
; sys    0.016

;; (B)v1.04 + set!なし (逆に少しだけ遅くなった)
;(time (set! pcmdata (mml->pcm "!c0 @500 [v120o3cdefgab>c< v50 o3eeeeeee ...
; real  13.562
; user  13.359
; sys    0.016

;; (C)v1.04 + %sin使用 (10%くらい速くなった)
;(time (set! pcmdata (mml->pcm "!c0 @500 [v120o3cdefgab>c< v50 o3eeeeeee ...
; real  11.641
; user  11.453
; sys    0.000

;; (D)v1.04 + %sin使用 + doループなし (10%くらい速くなった)
;(time (set! pcmdata (mml->pcm "!c0 @500 [v120o3cdefgab>c< v50 o3eeeeeee ...
; real  10.438
; user  10.281
; sys    0.031

;; (E)v1.05 内容は(D)と同じ (v1.04から20%くらい速くなった)
;(time (set! pcmdata (mml->pcm "!c0 @500 [v120o3cdefgab>c< v50 o3eeeeeee ...
; real  10.438
; user  10.156
; sys    0.094


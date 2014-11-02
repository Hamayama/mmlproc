;; -*- coding: utf-8 -*-
;;
;; mmlprocのテスト2
;; 2014-11-2
;;
(add-load-path "." :relative)
(use gauche.uvector)
(use gauche.time)
(use gauche.test)
(test-start "mmlproc")

(use mmlproc)
(test-module 'mmlproc)

(define (wavmake wavfile mml)
  (define pcmdata (mml->pcm mml))
  (call-with-output-file wavfile (cut write-wav pcmdata <>))
  (undefined))

(test-section "mml->pcm & write-wav")
(test* "tempo1"   (undefined) (wavmake "testdata01.wav" "t120cdt80eft30g"))
(test* "channel"  (undefined) (wavmake "testdata02.wav" "!c0 >c<bagf !c7 cdefr"))
(test* "prog"     (undefined) (wavmake "testdata03.wav" "@0 cde @1 cde @2 cde @3 cde @4 cde @500 cde @501 cde @502 cde"))
(test* "note1"    (undefined) (wavmake "testdata04.wav" "c+8.de"))
(test* "note2"    (undefined) (wavmake "testdata05.wav" "c-%24.de"))
(test* "tie1"     (undefined) (wavmake "testdata06.wav" "c4&c4"))
(test* "tie2"     (undefined) (wavmake "testdata07.wav" "c4^4"))
(test* "slur"     (undefined) (wavmake "testdata08.wav" "c4&d4"))
(test* "octave"   (undefined) (wavmake "testdata09.wav" "!oo5cde>cde<cde"))
(test* "sharp"    (undefined) (wavmake "testdata10.wav" "!+cfg cdefgab>c< !=cfg cdefgab>c< !-eab cdefgab>c< !=eab cdefgab>c"))
(test* "volume"   (undefined) (wavmake "testdata11.wav" "!v16v16cdv8efg"))
(test* "velocity" (undefined) (wavmake "testdata12.wav" "k10cde"))
(test* "length"   (undefined) (wavmake "testdata13.wav" "L%24.cde"))
(test* "gatetime" (undefined) (wavmake "testdata14.wav" "q1cde"))
(test* "loop1"    (undefined) (wavmake "testdata15.wav" "[ abc : de ]2"))
(test* "loop2"    (undefined) (wavmake "testdata16.wav" "[cd:e [fg]3 ]"))
(test* "tempo2"   (undefined) (wavmake "testdata17.wav" "!c0 @500 [     c t71 defg]3  !c1 @0   [t120 a     a2.b]3"))

(test-end)


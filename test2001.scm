;; -*- coding: utf-8 -*-
;;
;; mmlprocのテスト2
;; 2014-11-8
;;
(add-load-path "." :relative)
(use gauche.uvector)
(use gauche.time)
(use gauche.test)
(test-start "mmlproc")

(use mmlproc)
(test-module 'mmlproc)

(define (wavmake wavfile mml :optional (usedll -1))
  (define pcmdata #f)
  (if (boolean? usedll)
    (set! pcmdata (mml->pcm mml usedll))
    (set! pcmdata (mml->pcm mml)))
  (call-with-output-file wavfile (cut write-wav pcmdata <>))
  (undefined))

(test-section "mml->pcm & write-wav")
(test* "tempo1"   (undefined) (wavmake "testdata2001.wav" "t120cdt80eft30g" #t))
(test* "channel"  (undefined) (wavmake "testdata2002.wav" "!c0 >c<bagf !c7 cdefr" #f))
(test* "prog"     (undefined) (wavmake "testdata2003.wav" "@0 cde @1 cde @2 cde @3 cde @4 cde @500 cde @501 cde @502 cde"))
(test* "note1"    (undefined) (wavmake "testdata2004.wav" "c+8.de"))
(test* "note2"    (undefined) (wavmake "testdata2005.wav" "c-%24.de"))
(test* "tie1"     (undefined) (wavmake "testdata2006.wav" "c4&c4"))
(test* "tie2"     (undefined) (wavmake "testdata2007.wav" "c4^4"))
(test* "slur"     (undefined) (wavmake "testdata2008.wav" "c4&d4"))
(test* "octave"   (undefined) (wavmake "testdata2009.wav" "!oo5cde>cde<cde"))
(test* "sharp"    (undefined) (wavmake "testdata2010.wav" "!+cfg cdefgab>c< !=cfg cdefgab>c< !-eab cdefgab>c< !=eab cdefgab>c"))
(test* "volume"   (undefined) (wavmake "testdata2011.wav" "!v16v16cdv8efg"))
(test* "velocity" (undefined) (wavmake "testdata2012.wav" "k10cde"))
(test* "length"   (undefined) (wavmake "testdata2013.wav" "L%24.cde"))
(test* "gatetime" (undefined) (wavmake "testdata2014.wav" "q1cde"))
(test* "loop1"    (undefined) (wavmake "testdata2015.wav" "[ abc : de ]2"))
(test* "loop2"    (undefined) (wavmake "testdata2016.wav" "[cd:e [fg]3 ]"))
(test* "tempo2"   (undefined) (wavmake "testdata2017.wav" "!c0 @500 [     c  t71 defg]3 \
                                                           !c1 @0   [t120 a1        b]3 "))
(test* "chord"    (undefined) (wavmake "testdata2018.wav" "!c0v127c !c1v127d !c2v127e !c3v127f !c4v127g !c5v127a !c6v127b !c7v127>c<"))

(test-end)


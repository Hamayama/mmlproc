;;;
;;; Test mmlproc
;;;

(add-load-path "." :relative)
(use gauche.test)

(test-start "mmlproc")
(use mmlproc)
(test-module 'mmlproc)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-mmlproc" "mmlproc is working"
       (test-mmlproc))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)





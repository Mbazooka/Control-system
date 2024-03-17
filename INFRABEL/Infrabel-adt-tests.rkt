;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              INFRABEL ADT Test            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "Infrabel-adt.rkt")

(define infrabel-make-test
  (test-suite
   "INFRABEL-ADT: MAKE-INFRABEL-ADT TESTS"

  (test-case
   "Test if 'make-infrabel-adt' exists"
   (check-not-exn (lambda () make-infrabel-adt)))

  (test-case
   "Test if constructor does not give an error"
   (check-not-exn (lambda () (make-infrabel-adt))))))

(define all-tests (test-suite "INFRABEL-Module"
                              infrabel-make-test))

(test/gui all-tests)
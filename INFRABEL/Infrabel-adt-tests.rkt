;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Infrabel ADT Test             ;;
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
    (check-not-exn (labmda () make-infrabel-adt)))

   (test-case
    "Test if 
    




;; Bringing all the test-suites together
(define all-tests (test-suite "Infrabel-Module"
                              infrabel-make-test
                              ))

(test/gui all-tests)
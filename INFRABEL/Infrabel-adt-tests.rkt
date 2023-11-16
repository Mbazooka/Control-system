;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Infrabel ADT Test             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "Infrabel-adt.rkt")

(define infrabel-make-test




;; Bringing all the test-suites together
(define all-tests (test-suite "Infrabel-Module"

                              ))

(test/gui all-tests)
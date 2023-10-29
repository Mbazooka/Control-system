;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              switch ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "switch-adt.rkt")

;(require/expose "switch-adt.rkt" (change-position!))

(define new make-switch-adt)

(define switch-new-tests
  (test-suite
   "SWITCH-ADT: MAKE-SWITCH-ADT TESTS"

   (test-case
    "Test if 'make-switch-adt' exists"
    (check-not-exn (lambda () make-switch-adt)))

   (test-case
    "Test if constructor does not error"
    (check-not-exn (lambda () (make-switch-adt 'S-1 'left))))

   ;; regexp needed for test, to make sure error message conforms to what should be outputed
   (test-case
    "Test if constructor gives error with incorrect name"
    (check-exn (regexp "SWITCH-ADT: Invalid name") (lambda () (make-switch-adt 'S-99 'left))))

   (test-case
    "Test if constructor gives error with incorrect orientation"
    (check-exn (regexp "SWITCH-ADT: Invalid orientation given") (lambda () (make-switch-adt 'S-1 'bad-word))))))


;; Bringing all test-suites together
(define all-tests (test-suite "Switch-Module"
                              switch-new-tests))

(test/gui all-tests)



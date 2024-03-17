;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               NMBS ADT Test               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require rackunit
         rackunit/text-ui
         rackunit/gui
         "NMBS-adt.rkt")

(define NMBS-make-test
  (test-suite
   "NMBS-ADT: MAKE-NMBS-ADT TESTS"

  (test-case
   "Test if 'make-nmbs-adt' exists"
   (check-not-exn (lambda () make-nmbs-adt)))

  (test-case
   "Test if constructor does not give an error"
   (check-not-exn (lambda () (make-nmbs-adt))))))

(define test-nmbs (make-nmbs-adt))

(define NMBS-compute-trajectory-tests

  (test-suite
   "NMBS-ADT: COMPUTE-TRAJECTORY TESTS"

   (test-case
    "Test if 'compute-trajectory' works properly: Test 1"
    (check-equal?
     ((test-nmbs 'compute-trajectory) '1-1 '1-4)
     '((1-1 S-28 S-26 1-4))
     "compute-trajectory: incorrect operation"))

   (test-case
    "Test if 'compute-trajectory' works properly: Test 2"
    (check-equal?
     ((test-nmbs 'compute-trajectory) '1-4 '2-2)
     '((1-4 S-26 S-27 1-3 1-3 S-24 S-23 2-4 2-4 S-20 S-6 S-5 S-7 S-25 1-8) (1-8 S-25 S-1 S-2 S-3 2-2))
     "compute-trajectory: incorrect operation"))

   (test-case
    "Test if 'compute-trajectory' works properly: Test 3"
    (check-equal?
     ((test-nmbs 'compute-trajectory) '1-4 '2-7)
     '((1-4 S-26 S-27 1-3 1-3 S-24 S-23 2-4 2-4 S-20 S-6 S-5 S-7 S-2 S-1 2-1) (2-1 S-1 S-2 S-3 S-8 S-4 2-7))
     "compute-trajectory: incorrect operation"))

   (test-case
    "Test if 'compute-trajectory' works properly: Test 4"
    (check-equal?
     ((test-nmbs 'compute-trajectory) '2-7 '2-1)
     '((2-7 S-4 S-8 S-3 S-2 S-1 2-1))
     "compute-trajectory: incorrect operation"))

   (test-case
    "Test if 'compute-trajectory' works properly: Test 5"
    (check-equal?
     ((test-nmbs 'compute-trajectory) '1-4 '2-4)
     '((1-4 S-26 S-27 1-3 1-3 S-24 S-23 2-4))
     "compute-trajectory: incorrect operation"))

   (test-case
    "Test if 'compute-trajectory' works properly: Test 6"
    (check-equal?
     ((test-nmbs 'compute-trajectory) '1-3 '2-3)
     '((1-3 S-27 S-26 1-4) (1-4 S-26 S-28 1-1 1-1 S-10 S-11 S-12 2-3))
     "compute-trajectory: incorrect operation"))

   (test-case
    "Test if 'compute-trajectory' works properly: Test 7"
    (check-equal?
     ((test-nmbs 'compute-trajectory) '1-1 '1-3)
     '((1-1 S-28 S-26 1-4) (1-4 S-26 S-27 1-3))
     "compute-trajectory: incorrect operation"))
   

   ))

(define all-tests (test-suite "NMBS-Module"
                              NMBS-make-test
                              NMBS-compute-trajectory-tests))

(test/gui all-tests)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Train ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "train-adt.rkt")

;; Test the constructor
(define train-make-train-tests
  (test-suite
   "TRAIN-ADT: MAKE-TRAIN-ADT TESTS"

   (test-case
    "Test if 'make-train-adt' exists"
    (check-not-exn (lambda () make-train-adt)))

   (test-case
    "Test if cosntructor does not give error"
    (check-not-exn (lambda () (make-train-adt 'TEST '1-1 '+))))

   (test-case
    "Test if constructor gives error with incorrect orientation"
    (check-exn (regexp "TRAIN-ADT: Invalid orientation given")
               (lambda () (make-train-adt 'TEST '1-1 'illegal-value))))))

;; Dummy train necessary for testing
(define test-train (make-train-adt 'TEST '1-1 '+))

;; Test the get-name operation
(define train-name-tests
  (test-suite
   "TRAIN-ADT: GET-NAME TESTS"

   (test-case
    "Test if 'get-name' gets the correct-name"
    (check-eq? ((test-train 'get-name)) 'TEST "Get-name: incorrect name"))))

;; Test the get-initial-track operation
(define train-initial-track-tests
  (test-suite
   "TRAIN-ADT: GET-INITIAL-TRACK TESTS"

   (test-case
    "Test if 'get-initial-track' gets the correct-name"
    (check-eq? ((test-train 'get-initial-track)) '1-1 "Get-initial-track: incorrect track"))))

;; Tests the current-speed and change speed operation
(define train-get-current-speed/change-speed!-tests
  (test-suite
   "TRAIN-ADT: GET-CURRENT-SPEED/CHANGE-SPEED! TESTS"

   (test-case
    "Test if 'get-current-speed' gets the correct speed"
    (check-eq? ((test-train 'get-current-speed)) 0 "Get-current-speed: incorrect output"))

   (test-case
    "Test if 'change-speed!' changes the speed correctly (positive)"
    (check-eq?
     (begin
       ((test-train 'change-speed!) 200)
       ((test-train 'get-current-speed)))
     200
     "Change-speed!: incorrect output"))

   (test-case
    "Test if 'change-speed!' acts properly with negative speeds (negative)"
    (check-equal?
     ((test-train 'change-speed!) -200)
     "TRAIN-ADT: Illegal speed"
     "Change-speed!: incorrect output"))

   (test-case
    "Test if 'change-speed!' allows one to go over the speed limit of 300"
    (check-equal?
     ((test-train 'change-speed!) 99999)
     "TRAIN-ADT: Illegal speed"
     "Change-speed!: incorrect action"))
       

   ))

;; Tests the change-orientation! operation
(define train-change-orientation!-tests
  (test-suite
   "TRAIN-ADT: GET-ORIENTATION/CHANGE-ORIENTATION! TESTS"

   (test-case
    "Test if 'get-orientation' gives back the correct output"
    (check-eq? ((test-train 'get-orientation)) '+
               "Get-orientation: incorrect output"))

   (test-case
    "Test if 'change-orientation!' changes the orientation correctly"
    (check-eq?
     (begin
       ((test-train 'change-orientation!) '-)
       ((test-train 'get-orientation)))
     '-
     "Change-orientation!: incorrect output"))))

   
   

;; Bringing all test-suites together
(define all-tests (test-suite "TRAIN-Module"
                              train-make-train-tests
                              train-name-tests
                              train-get-current-speed/change-speed!-tests
                              train-change-orientation!-tests))

(test/gui all-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Track ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "track-adt.rkt")

;; Tests the constructor
(define track-make-track-tests
  (test-suite
   "TRACK-ADT: MAKE-TRACK-ADT TESTS"

   (test-case
    "Test if 'make-track-adt' exists"
    (check-not-exn (lambda () make-track-adt)))

   (test-case
    "Test if constructor does not give error"
    (check-not-exn (lambda () (make-track-adt '1-1 'L-1 'S-1 'C-1))))))

;; Dummy track necessary for testing
(define test-track (make-track-adt '1-1 'L-1 'S-1 'C-1))

;; Test the get-name operations of the class
(define track-get-name-ops-tests
  (test-suite
   "TRACK-ADT: GET-NAME-OPS TESTS"

   (test-case
    "Test if 'get-detection-block-name' gets the correct name"
    (check-eq? ((test-track 'get-detection-block-name)) '1-1
               "Get-detection-block-name: incorrect name"))

   (test-case
    "Test if 'get-light-name' gets the correct name"
    (check-eq? ((test-track 'get-light-name)) 'L-1
               "Get-light-name: incorrect name"))

   (test-case
    "Test if 'get-switch-name' gets the correct name"
    (check-eq? ((test-track 'get-switch-name)) 'S-1
               "Get-switch-name: incorrect name"))

   (test-case
    "Test if 'get-barrier-name' gets the correct name"
    (check-eq? ((test-track 'get-barrier-name)) 'C-1
               "Get-barrier-name: incorrect name"))))


;; Test the illegal message sending action
(define track-message-sending-tests
  (test-suite
   "TRACK-ADT: MESSAGE-SENDING TESTS"

   (test-case
    "Test if message-sending is protected against illegal messagaes"
    (check-equal?
     (test-track 'illegal-message)
     "TRACK-ADT: Incorrect message"
     "module allows illegal message to be sent without warning"))))

   

;; Bringing all test-suites together
(define all-tests (test-suite "Track-Module"
                              track-make-track-tests
                              track-get-name-ops-tests
                              track-message-sending-tests))

(test/gui all-tests)


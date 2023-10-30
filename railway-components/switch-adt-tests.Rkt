;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              switch ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "switch-adt.rkt")

;; Tests the constructor
(define switch-make-switch-tests
  (test-suite
   "SWITCH-ADT: MAKE-SWITCH-ADT TESTS"

   (test-case
    "Test if 'make-switch-adt' exists"
    (check-not-exn (lambda () make-switch-adt)))

   (test-case
    "Test if constructor does not give error"
    (check-not-exn (lambda () (make-switch-adt 'S-1 'left))))

   ;; regexp needed for test, to make sure error message conforms to what should be outputed
   (test-case
    "Test if constructor gives error with incorrect name"
    (check-exn (regexp "SWITCH-ADT: Invalid name") (lambda () (make-switch-adt 'S-99 'left))))

   (test-case
    "Test if constructor gives error with incorrect orientation"
    (check-exn (regexp "SWITCH-ADT: Invalid orientation given") (lambda () (make-switch-adt 'S-1 'illegal-value))))))

;; Dummy switch necessary for testing
(define test-switch (make-switch-adt 'S-1 'left))

;; Test the name operation of the class
(define switch-name-tests
  (test-suite
   "SWITCH-ADT: GET-NAME TESTS"
   (test-case
    "Test if 'get-name' get the correct name"
    (check-eq? ((test-switch 'get-name)) 'S-1 "Get-name: incorrect name"))))

;; Test the current/change-position operation of the class
(define switch-current/change-position-tests
  (test-suite
   "SWITCH-ADT: CHANGE/CURRENT-POSITION TESTS"

   (test-case
    "Test if 'current-position' works properly"
    (check-equal?
     ((test-switch 'current-position))
     'left
     "Current-position: incorrect output"))

   (test-case
    "Test if 'change-position' changes the switch state correctly"
    (check-equal?
     (begin
       ((test-switch 'change-position!) 'right)
       ((test-switch 'current-position)))
     'right
     "change-position: incorrect change"))

   (test-case
    "Test if 'change-position' can change switch to illegal state"
    (check-equal?
     ((test-switch 'change-position!) 'illegal-value)
     "SWITCH-ADT: Incorrect state in change-position!"
     "change-position: incorrect change"))))

(define switch-message-sending-tests
  (test-suite
   "SWITCH-ADT: MESSAGE-SENDING TESTS"

   (test-case
    "Test if message-sending is protected against illegal messages"
    (check-equal?
     (test-switch 'illegal-message)
     "SWITCH-ADT: Incorrect message"
     "module allows illegal message to be sent without warning"))))


;; Bringing all test-suites together
(define all-tests (test-suite "Switch-Module"
                              switch-make-switch-tests
                              switch-name-tests
                              switch-current/change-position-tests
                              switch-message-sending-tests))

(test/gui all-tests)



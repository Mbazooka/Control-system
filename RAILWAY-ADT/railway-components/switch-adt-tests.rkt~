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
    (check-not-exn (lambda () (make-switch-adt 'S-1))))))

;; Dummy switches necessary for testing
(define test-switch (make-switch-adt 'S-1))

;; Test the get-name operation of the class
(define switch-name-tests
  (test-suite
   "SWITCH-ADT: GET-NAME TESTS"
   (test-case
    "Test if 'get-name' gets the correct name"
    (check-eq? ((test-switch 'get-name)) 'S-1 "Get-name: incorrect name"))))

;; Test the current/change-position operation of the class
(define switch-current/change-position-tests
  (test-suite
   "SWITCH-ADT: CHANGE/CURRENT-POSITION TESTS"

   (test-case
    "Test if 'current-position' works properly"
    (check-eq?
     ((test-switch 'current-position))
     1
     "Current-position: incorrect output"))

   (test-case
    "Test if 'change-position!' changes the switch state correctly"
    (check-equal?
     (begin
       ((test-switch 'change-position!) 2)
       ((test-switch 'current-position)))
     2
     "change-position!: incorrect change"))

   (test-case
    "Test if 'change-position!' can change switch to illegal state"
    (check-equal?
     ((test-switch 'change-position!) 3)
     "SWITCH-ADT: Incorrect state in change-position!"
     "change-position!: incorrect change"))))

;; Test the illegal message sending action
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



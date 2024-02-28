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
    (check-not-exn (lambda () (make-switch-adt 'S-1 '2-1 'S-25))))))

;; Dummy switches necessary for testing
(define test-switch (make-switch-adt 'S-1 '2-1 'S-25))

;; Test the get-name operation of the class
(define switch-name-tests
  (test-suite
   "SWITCH-ADT: GET-NAME TESTS"
   (test-case
    "Test if 'get-name' gets the correct name"
    (check-eq? ((test-switch 'get-name)) 'S-1 "Get-name: incorrect name"))))

;; Test the current/change-position/current-comp operation of the class
(define switch-current/current-comp/change-position/-tests
  (test-suite
   "SWITCH-ADT: CHANGE/CURRENT-POSITION/CURRENT-COMP TESTS"

   (test-case
    "Test if 'current-position' works properly"
    (check-eq?
     ((test-switch 'current-position))
     1
     "Current-position: incorrect output"))

   (test-case
    "Test if 'current-comp' works properly"
    (check-eq?
     ((test-switch 'current-comp))
     '2-1
     "Current-comp: incorrect output"))

   (test-case
    "Test if 'change-position!' changes the switch state correctly"
    (check-eq?
     (begin
       ((test-switch 'change-position!) 2)
       (and (eq? ((test-switch 'current-position)) 2)
            (eq? ((test-switch 'current-comp)) 'S-25)))
     #t
     "change-position!: incorrect change"))

   (test-case
    "Test if 'change-position!' can change switch to illegal state"
    (check-equal?
     ((test-switch 'change-position!) 3)
     "SWITCH-ADT: Incorrect state in change-position!"
     "change-position!: incorrect change"))))

;; Test the possible-comp-states operation of the class
(define switch-possible-comp-states-tests
  (test-suite
   "SWITCH-ADT: POSSIBLE-COMP-STATES TESTS"

   (test-case
    "Test if 'possible-comp-states' works properly"
    (check-eq?
     (let ((states ((test-switch 'possible-comp-states))))
       (and (eq? (car states) '2-1) (eq? (cadr states) 'S-25)))
     #t
     "possible-comp-states: incorrect output"))))
   

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
                              switch-current/current-comp/change-position/-tests
                              switch-possible-comp-states-tests
                              switch-message-sending-tests))

(test/gui all-tests)



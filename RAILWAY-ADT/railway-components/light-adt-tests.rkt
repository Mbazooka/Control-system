;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Light ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "light-adt.rkt")

;; Tests the constructor
(define light-make-light-tests
  (test-suite
   "LIGHT-ADT: MAKE-LIGHT-ADT TESTS"

   (test-case
    "Test if 'make-light-adt' exists"
    (check-not-exn (lambda () make-light-adt)))

   (test-case
    "Test if constructor does not give error"
    (check-not-exn (lambda () (make-light-adt 'L-1))))))

;; Dummy light necessary for testing
(define test-light (make-light-adt 'L-1))

;; Test the get-name operation of the class
(define light-name-tests
  (test-suite
   "LIGHT-ADT: GET-NAME TESTS"
   (test-case
    "Test if 'get-name' get the correct name"
    (check-eq? ((test-light 'get-name)) 'L-1 "Get-name: incorrect name"))))

;; Tests the operations of the class
(define light-get-state/change-light!-tests
  (test-suite
   "LIGHT-ADT: GET-STATE/CHANGE-LIGHT! TESTS"

   (test-case
    "Test if 'get-state' works properly"
    (check-eq?
     ((test-light 'get-state))
     'Hp0
     "get-state: incorrect output"))

   (test-case
    "Test if 'change-light!' changes the state of the light correctly"
    (check-eq?
     (begin
       ((test-light 'change-light!) 'Hp0+Sh0)
       ((test-light 'get-state)))
     'Hp0+Sh0
     "change-light!: incorrect change"))

   (test-case
    "Test if 'change-light!' rejects illegal light states"
    (check-equal?
     ((test-light 'change-light!) 'illegal-value)
     "LIGHT-ADT: Incorect code given"
     "change-light!: incorrect change"))))

;; Test the illegal message sending action
(define light-message-sending-tests
  (test-suite
   "LIGHT-ADT: MESSAGE-SENDING TESTS"

   (test-case
    "Test if message-sending is protected against illegal messages"
    (check-equal?
     (test-light 'illegal-message)
     "LIGHT-ADT: Incorrect message"
     "module allows illegal message to be sent without warning"))))
   

;; Bringing all test suites together
(define all-tests (test-suite "Light-Module"
                              light-make-light-tests
                              light-name-tests
                              light-get-state/change-light!-tests
                              light-message-sending-tests))

(test/gui all-tests)
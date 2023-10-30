;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             barrier ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "barrier-adt.rkt")

;; Tests the constructor
(define barrier-make-barrier-tests
  (test-suite
   "BARRIER-ADT: MAKE-BARRIER-ADT TESTS"

   (test-case
    "Test if 'make-barrier-adt' exists"
    (check-not-exn (lambda () make-barrier-adt)))

   (test-case
    "Test if constructor does not give error"
    (check-not-exn (lambda () (make-barrier-adt 'C-1))))

   (test-case
    "Test if cosntructor gives error with incorrect name"
    (check-exn (regexp "BARRIER-ADT: Invalid name")
               (lambda () (make-barrier-adt 'illegal-value))))))

;; Dummy barrier necessary for testing
(define test-barrier (make-barrier-adt 'C-1))

;; Test the get-name operation of the class
(define barrier-name-tests
  (test-suite
   "BARRIER-ADT: GET-NAME TESTS"
   (test-case
    "Test if 'get-name' get the correct name"
    (check-eq? ((test-barrier 'get-name)) 'C-1 "Get-name: incorrect name"))))


;; Tests the operations of the class
(define barrier-open!/closed!/open-barrier?-tests
  (test-suite
   "BARRIER-ADT: OPEN!/CLOSED!/OPEN-BARRIER? TESTS"

   (test-case
    "Test if 'open-barrier?' works correctly"
    (check-equal? ((test-barrier 'open-barrier?)) #t "Open-barrier?: incorrect output"))

   (test-case
    "Test if 'closed!' closes the barrier"
    (check-eq?
     (begin
       ((test-barrier 'close!))
       ((test-barrier 'open-barrier?)))
     #f
     "Close!: incorrect change of state"))

   (test-case
    "Test if 'closed!' deals appropiately with already closed barriers"
    (check-eq?
     (begin
       ((test-barrier 'close!))
       ((test-barrier 'open-barrier?)))
     #f
     "Close!: State has been changed"))

   (test-case
    "Test if 'open!' opens the barrier"
    (check-eq?
     (begin
       ((test-barrier 'open!))
       ((test-barrier 'open-barrier?)))
     #t
     "Open!: incorrect change of state"))

   (test-case
    "Test if 'open!' deals appropiately with already open barriers"
    (check-eq?
     (begin
       ((test-barrier 'open!))
       ((test-barrier 'open-barrier?)))
     #t
     "Open!: incorrect change of state"))))

;; Test the illegal message sending action
(define barrier-message-sending-tests
  (test-suite
   "BARRIER-ADT: MESSAGE-SENDING TESTS"
   
   (test-case
    "Test if message-sending is protected against illegal messages"
    (check-equal?
     (test-barrier 'illegal-message)
     "BARRIER-ADT: Incorrect message")
    "module allows illegal message to be sent without warning")))

;; Bringing all test-suites together
(define all-tests (test-suite "Barrier-Module"
                              barrier-make-barrier-tests
                              barrier-name-tests
                              barrier-open!/closed!/open-barrier?-tests
                              barrier-message-sending-tests))

(test/gui all-tests)
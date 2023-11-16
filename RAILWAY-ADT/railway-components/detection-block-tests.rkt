;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          detection-block Test             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "detection-block-adt.rkt")

;; Test the constructor
(define detection-block-make-db-tests
  (test-suite
   "DETECTION-BLOCK-ADT: MAKE-DB-ADT TESTS"

   (test-case
    "Test if 'make-detection-block-adt' exists"
    (check-not-exn (lambda () make-detection-block-adt)))))

;; Dummy detection-block necessary for testing
(define test-detection-block (make-detection-block-adt 'TEST))

;; Test the get-name operation of the class
(define detection-block-name-tests
  (test-suite
   "DETECTION-BLOCK-ADT: GET-NAME TESTS"
   
   (test-case
    "Test if 'get-name' gets the correct name"
    (check-eq? ((test-detection-block 'get-name)) 'TEST "Get-name: incorrect name"))))

;; Test the get-presence/add-train! operation of the class
(define detection-block-get-presence/add-train!-tests
  (test-suite
   "DETECTION-BLOCK-ADT: GET-PRESENCE/add-train! TESTS"

   (test-case
    "Test if 'get-presence' works properly"
    (check-eq? ((test-detection-block 'get-presence) 'TEST) #f
               "Get-presence: Incorrect output"))

   (test-case
    "Test if 'add-train!' works properly with correct input"
    (check-eq?
     (begin
       ((test-detection-block 'add-train!) 'TEST)
       ((test-detection-block 'get-presence) 'TEST))
     #t
     "add-train!: Incorrect change"))

   (test-case
    "Test if 'add-train!' works properly with illegal input"
    (check-equal?
     ((test-detection-block 'add-train!) 123)
     "add-train!: illegal value"
     "add-train!: Incorrect change"))))

;; Test the illegal message sending action
(define detection-block-message-sending-tests
  (test-suite
   "DETECTION-BLOCK-ADT: MESSAGE-SENDING TESTS"

   (test-case
    "Test if message sending is protected against illegal messages"
    (check-equal?
     (test-detection-block 'illegal-value)
     "DETECTION-BLOCK-ADT: Incorrect message"
     "module allows illegal message to be sent without warning"))))
     
;; Bringing al test-suites together
(define all-tests (test-suite "Detection-Block-Module"
                              detection-block-make-db-tests
                              detection-block-name-tests
                              detection-block-get-presence/add-train!-tests
                              detection-block-message-sending-tests))

(test/gui all-tests)

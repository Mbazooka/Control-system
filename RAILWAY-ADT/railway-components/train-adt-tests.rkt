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
    (check-not-exn (lambda () (make-train-adt 'TEST '1-1 'S-10))))))

;; Dummy train necessary for testing
(define test-train (make-train-adt 'TEST '1-1 'S-10))

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

;; Test the get-initial-track-behind operation
(define train-initial-track-behind-tests
  (test-suite
   "TRAIN-ADT: GET-INITIAL-TRACK-BEHIND TESTS"

   (test-case
    "Test if 'get-initial-track-behind' gets the correct name"
    (check-eq? ((test-train 'get-initial-track-behind)) 'S-10
               "Get-initial-track-behind: incorrect track"))))

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
     (begin
       ((test-train 'change-speed!) -200)
       ((test-train 'get-current-speed)))
     -200
     "Change-speed!: incorrect output"))

   (test-case
    "Test if 'change-speed!' allows one to go over the speed limit of 300"
    (check-equal?
     ((test-train 'change-speed!) 99999)
     "TRAIN-ADT: Illegal speed"
     "Change-speed!: incorrect action"))
   ))

;; Tests the get-trajectory-state and change-trajectory-state! operations
(define train-get-trajectory-state/change-trajectory-state!-tests
  (test-suite
   "TRAIN-ADT: GET-TRAJECTORY-STATE/CHANGE-TRAJECTORY-STATE! TESTS"

   (test-case
    "Test if 'get-trajectory-state' gets the correct state"
    (check-eq?
     ((test-train 'get-trajectory-state))
     #f
     "get-trajectory-state: incorrect output"))

   (test-case
    "Test if 'change-trajectory-state' changes the states properly"
    (check-equal?
     (begin
       ((test-train 'change-trajectory-state!) '(1-1 1-2))
       ((test-train 'get-trajectory-state)))
     '(1-1 1-2)
     "change-trajectory-state!: incorect action"))

   (test-case 
    "Test if 'change-trajectory-state' can't accept bogus input"
    (check-equal?
     ((test-train 'change-trajectory-state!) 'bogus)
     "Train-ADT: change-trajectory-state incorrect input"
     "change-trajectory-state!: incorrect input"))

   ))

;; Tests the get-current-track and change-current-track! operations
(define train-get-current-track/get-track-behind/change-current-track!-tests
  (test-suite
   "TRAIN-ADT: GET-CURRENT-TRACK/GET-TRACK-BEHIND/CHANGE-CURRENT-TRACK! TESTS"

   (test-case
    "Test if 'get-current-track' gets the correct track"
    (check-eq?
     ((test-train 'get-current-track))
     '1-1
     "get-current-track: Incorrect output"))

   (test-case
    "Test if 'get-track-behind' gets the correct track"
    (check-eq?
     ((test-train 'get-track-behind))
     'S-10
     "get-track-behind!: Incorrect output"))

   (test-case
    "Test if 'change-current-track!' works properly"
    (check-eq?
     (begin
       ((test-train 'change-current-track!) '2-3)
       (and (eq? ((test-train 'get-current-track)) '2-3)
            (eq? ((test-train 'get-track-behind)) '1-1)))
     #t
     "change-current-track!: incorrect operation"))))
     

;; Bringing all test-suites together
(define all-tests (test-suite "TRAIN-Module"
                              train-make-train-tests
                              train-name-tests
                              train-initial-track-tests
                              train-initial-track-behind-tests
                              train-get-current-speed/change-speed!-tests
                              train-get-trajectory-state/change-trajectory-state!-tests
                              train-get-current-track/get-track-behind/change-current-track!-tests
                              ))

(test/gui all-tests)

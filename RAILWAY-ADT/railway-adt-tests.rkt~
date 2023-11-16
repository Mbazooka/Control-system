;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Railway ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "railway-adt.rkt")

;; Tests the constructor
(define railway-make-tests
  (test-suite
   "RAILWAY-ADT: MAKE-RAILWAY-ADT TESTS"

   (test-case
    "Test if 'make-railway-adt' exists"
    (check-not-exn (lambda () make-railway-adt)))

   (test-case
    "Test if constructor does not give error"
    (check-not-exn (lambda () (make-railway-adt))))))

;; Dummy railway necessary for testing
(define test-railway (make-railway-adt))

;; Test the add-train! operation of the railway
(define railway-add-train-tests
  (test-suite
   "RAILWAY-ADT: ADD-TRAIN TESTS"

   (test-case
    "Test if 'add-train!' exists"
    (check-not-equal?
     (test-railway 'add-train!)
     "RAILWAY-ADT: Incorrect message"
     "Add-train!: operation does not exist"))

   (test-case
    "Test if 'add-train!' works properly"
    (check-not-exn
     (lambda () ((test-railway 'add-train!) 'TEST-TRAIN-1 '1-1 'S-10))))

   (test-case
    "Test if 'add-train!' does not allow name overlap"
    (check-equal?
     ((test-railway 'add-train!) 'TEST-TRAIN-1 '1-2 'S-10)
     "RAILWAY-ADT: Train already exists"
     "Add-train!: Train already exists"))))

;; Test the change-train-speed!/get-train-speed operation of the railway
(define railway-change-train-speed!/get-train-speed-tests
  (test-suite
   "RAILWAY-ADT: CHANGE-TRAIN-SPEED! TESTS"

   (test-case
    "Test if 'change-train-speed!' exists"
    (check-not-equal?
     (test-railway 'change-train-speed!)
     "RAILWAY-ADT: Incorrect message"
     "Change-train-speed!: operation does not exist"))

   (test-case
    "Test if 'get-train-speed' exists"
    (check-not-equal?
     (test-railway 'get-train-speed)
     "RAILWAY-ADT: Incorrect message"
     "Get-current-speed: operation does not exist"))

   (test-case
    "Test if 'change-train-speed!/get-train-speed' works properly"
    (check-eq?
     (begin
       ((test-railway 'add-train!) 'TEST-TRAIN-1 '1-2 'S-10)
       ((test-railway 'change-train-speed!) 'TEST-TRAIN-1 200)
       ((test-railway 'get-train-speed) 'TEST-TRAIN-1))
     200
     "Change-train-speed!/get-train-speed: Incorrect operation"))))

;; Test the change-switch-state!/get-switch-state operation of the railway
(define railway-change-switch-state!/get-switch-state-tests
  (test-suite
   "RAILWAY-ADT: CHANGE-SWITCH-STATE!/GET-SWITCH-STATE TESTS"

   (test-case
    "Test if 'change-switch-state!' exists"
    (check-not-equal?
     (test-railway 'change-switch-state!)
     "RAILWAY-ADT: Incorrect message"
     "Change-switch-state!: operation does not exist"))

   (test-case
    "Test if 'get-switch-state' does not exist"
    (check-not-equal?
     (test-railway 'get-switch-state)
     "RAILWAY-ADT: Incorrect message"
     "Get-switch-state: operation does not exist"))

   (test-case
    "Test if 'change-switch-state!/get-switch-state' works properly"
    (check-eq?
     (begin
       ((test-railway 'change-switch-state!) 'S-1 2)
       ((test-railway 'get-switch-state) 'S-1))
     2
     "change-switch-state!/get-switch-state: Incorrect operation"))))

;; Test the check-barrier-open?/change-barrier-state! operation of the railway
(define railway-check-barrier-open?/change-barrier-state!-tests
  (test-suite
   "RAILWAY-ADT: CHECK-BARRIER-OPEN?/CHANGE-BARRRIER-STATE! TESTS"

   (test-case
    "Test if 'check-barrier-open?' exists"
    (check-not-equal?
     (test-railway 'check-barrier-open?)
     "RAILWAY-ADT: Incorrect message"
     "Check-barrier-open?: operation does not exist"))

   (test-case
    "Test if 'change-barrier-state!' exists"
    (check-not-equal?
     (test-railway 'change-barrier-state!)
     "RAILWAY:ADT: Incorrect message"
     "change-barrier-state!: operation does not exist"))

   (test-case
    "Test if 'check-barrier-open?/change-barrier-state!' works properly"
    (check-eq?
     ((test-railway 'check-barrier-open?) 'C-1)
     #t
     "check-barrier-open?: Incorrect operation"))

   (test-case
    "Test if 'change-barrier-state!' closes properly"
    (check-eq?
     (begin
       ((test-railway 'change-barrier-state!) 'C-1 'close)
       ((test-railway 'check-barrier-open?) 'C-1))
     #f
     "change-barrier-state!: Incorrect operation"))

   (test-case
    "Test if 'change-barrier-state!' opens properly"
    (check-eq?
     (begin
       ((test-railway 'change-barrier-state!) 'C-1 'open)
       ((test-railway 'check-barrier-open?) 'C-1))
     #t
     "change-barrier-state!: Incorrect operation"))
   ))

(define railway-get-light-state/change-light-state!-tests
  (test-suite
   "RAILWAY-ADT: GET-LIGHT-STATE/CHANGE-LIGHT-STATE! TESTS"

   (test-case
    "Test if 'get-light-state' exists"
    (check-not-equal?
     (test-railway 'get-light-state)
     "RAILWAY-ADT: Incorrect message"
     "get-light-state: operation does not exist"))

   (test-case
    "Test if 'change-light-state!' exists"
    (check-not-equal?
     (test-railway 'change-light-state!)
     "RAILWAY-ADT: Incorrect message"
     "change-light-state!: operation does not exist"))

   (test-case
    "Test if 'get-light-state' works properly"
    (check-eq?
     ((test-railway 'get-light-state) 'L-1)
     'Hp0
     "get-light-state: Incorrect operation"))

   (test-case
    "Test if 'change-light-state!' works properly"
    (check-equal?
     (begin
       ((test-railway 'change-light-state!) 'L-1 'Hp1)
       ((test-railway 'get-light-state) 'L-1))
     'Hp1
     "change-light-state!: Incorrect operation"))

   (test-case
    "Test if 'change-light-state!' works properly"
    (check-equal?
     (begin
       ((test-railway 'change-light-state!) 'L-1 'Ks1+Zs3+Zs3v)
       ((test-railway 'get-light-state) 'L-1))
     'Ks1+Zs3+Zs3v
     "change-light-state!: Incorrect operation"))   

   ))

(define railway-change-detection-block-state!/get-detection-block-state-tests

  (test-suite
   "RAILWAY-ADT: CHANGE-DETECTION-BLOCK-STATE!/GET-DTECTION-BLOCK-STATE TESTS"


   (test-case
    "Test if 'get-detection-block-state' exists"
    (test-railway 'get-detection-block-state)
    "RAILWAY-ADT: Incorrect message"
    "get-detection-block-state: operation does not exist")
      
   (test-case
    "Test if 'change-detection-block-state!' exists"
    (check-not-equal?
     (test-railway 'change-detection-block-state!)
     "RAILWAY-ADT: Incorrect message"
     "change-detection-block-state: operation does not exist"))

   (test-case
    "Test if 'get-detection-block-state' works properly"
    (check-eq?
     ((test-railway 'get-detection-block-state) 'T-1-1)
     #f
     "get-detection-block-state: Incorrect operation"))

   (test-case
    "Test if 'change-detection-block-state' exists"
    (check-eq?
     (begin
       ((test-railway 'change-detection-block-state!) 'T-1-1 #t)
       ((test-railway 'get-detection-block-state) 'T-1-1))
     #t
     "change-detection-block-state: Incorrect operation"))

   ))
     
    

;; Bringing all test-suites together
(define all-tests (test-suite "Railway-Module"
                              railway-make-tests
                              railway-add-train-tests
                              railway-change-train-speed!/get-train-speed-tests
                              railway-change-switch-state!/get-switch-state-tests
                              railway-check-barrier-open?/change-barrier-state!-tests
                              railway-get-light-state/change-light-state!-tests
                              railway-change-detection-block-state!/get-detection-block-state-tests))

(test/gui all-tests)
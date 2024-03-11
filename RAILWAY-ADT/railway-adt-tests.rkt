;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Railway ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit
         rackunit/text-ui
         rackunit/gui
         "railway-adt.rkt")

;; Tests the constructor
(define railway-make-test
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
    (check-eq?
     ((test-railway 'add-train!) 'TEST-TRAIN-1 '1-2 'S-10)
     #f
     "Add-train!: Train already exists"))))


;; Test the change-train-speed!/get-train-speed operation of the railway
(define railway-change-train-speed!/get-train-speed/get-train-track/get-train-track-behind/change-train-track!/get-all-trains-tests
  (test-suite
   "RAILWAY-ADT: CHANGE-TRAIN-SPEED!/GET-TRAIN-SPEED/GET-TRAIN-TRACK/CHANGE-TRAIN-TRACK!/GET-ALL-TRAINS TESTS"

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
     "Change-train-speed!/get-train-speed: Incorrect operation"))

   (test-case
    "Test if 'RAILWAY-ADT: OBJECT DOES NOT EXIST' gets triggerd"
    (check-equal?
     ((test-railway 'change-train-speed!) 'UNDEFINED-NAME 200)
     "RAILWAY-ADT: OBJECT DOES NOT EXIST"
     "change-operation-abstraction: Incorrect output"))

   (test-case
    "Test if 'get-all-trains' works properly"
    (check-eq?
     (length ((test-railway 'get-all-trains)))
     1
     "get-all-trains: incorrect output"))

   (test-case
    "Test if 'get-train-track' works properly"
    (check-eq?
     (begin
       ((test-railway 'add-train!) 'TEST-TRAIN-2 '1-8 'S-10)
       ((test-railway 'get-train-track) 'TEST-TRAIN-2))
     '1-8
     "get-train-track: incorrect operation"))

   (test-case
    "Test if 'get-train-track-behind' works properly"
    (check-eq?
     (begin
       ((test-railway 'add-train!) 'TEST-TRAIN-19 '1-1 '1-2)
       ((test-railway 'get-train-track-behind) 'TEST-TRAIN-19))
     '1-2
     "get-train-track-behind: incorrect operation"))

   (test-case
    "Test if 'change-train-track!' works properly"
    (check-eq?
     (begin
       ((test-railway 'add-train!) 'TEST-TRAIN-3 '2-3 'S-10)
       ((test-railway 'change-train-track!) 'TEST-TRAIN-3 '1-2)
       (and (eq? ((test-railway 'get-train-track) 'TEST-TRAIN-3) '1-2)
            (eq? ((test-railway 'get-train-track-behind) 'TEST-TRAIN-3) '2-3)))
     #t
     "change-train-track!: incorrect operation"))

   ))

;; Test the change-switch-state!/get-switch-state operation of the railway
(define railway-change-switch-state!/get-switch-state/get-switch-comp-state-tests
  (test-suite
   "RAILWAY-ADT: CHANGE-SWITCH-STATE!/GET-SWITCH-STATE/GET-SWITCH-COMP-STATE TESTS"

   (test-case
    "Test if 'change-switch-state!' exists"
    (check-not-equal?
     (test-railway 'change-switch-state!)
     "RAILWAY-ADT: Incorrect message"
     "Change-switch-state!: operation does not exist"))

   (test-case
    "Test if 'get-switch-state' exists"
    (check-not-equal?
     (test-railway 'get-switch-state)
     "RAILWAY-ADT: Incorrect message"
     "Get-switch-state: operation does not exist"))

   (test-case
    "Test if 'get-switch-comp-state' exists"
    (check-not-equal?
     (test-railway 'get-switch-comp-state)
     "RAILWAY-ADT: Incorrect message"
     "Get-switch-comp-state; operation does not exist"))

   (test-case
    "Test if 'change-switch-state!/get-switch-state/get-comp-state' works properly"
    (check-eq?
     (begin
       ((test-railway 'change-switch-state!) 'S-1 2)
       (and (eq? ((test-railway 'get-switch-state) 'S-1) 2)
            (eq? ((test-railway 'get-switch-comp-state) 'S-1) 'S-25)))
     #t
     "change-switch-state!/get-switch-state/get-switch-comp-state: Incorrect operation"))))

(define railway-get-all-switches-tests
  (test-suite
   "RAILWAY-ADT: GET-ALL-SWITCHES TESTS"

   (test-case
    "Test if 'get-all-switches' exists"
    (check-not-equal?
     (test-railway 'get-all-switches)
     "RAILWAY-ADT: Incorrect message"
     "get-all-switches: operation does not exist"))

   (test-case
    "Test if 'get-all-switches' works properly"
    (check-eq?
     (length ((test-railway 'get-all-switches)))
     20
     "get-all-switches: Incorrect-operation"))))

(define railway-get-switch-possible-comp-states-tests
  (test-suite
   "RAILWAY-ADT: GET-SWITCH-POSSIBLE-COMP-STATES TESTS"

   (test-case
    "Test if 'get-switch-possible-comp-states' works properly"
    (check-eq?
     (let ((states ((test-railway 'get-switch-possible-comp-states))))
       (and (eq? (car states) '2-1) (eq? (cadr states) 'S-25)))
     #t
     "get-switch-possible-comp-states: incorrect output"))))

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

   (test-case
    "Test if 'change-barrier-state!' gives correct output with invalid names"
    (check-eq?
     ((test-railway 'change-barrier-state!) 'Invalid-name 'open)
     #f
     "change-barrier-state!: Incorrect output"))
   ))

;(define railway-get-all-barriers-tests

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

(define railway-get-all-lights-tests
  (test-suite
   "RAILWAY-ADT: GET-AL-LIGHTS TESTS"

   (test-case
    "Test if 'get-all-lights' exists"
    (check-not-equal?
     (test-railway 'get-all-lights)
     "RAILWAY-ADT: Incorrect message"
     "get-all-lights: operation does not exist"))

   (test-case
    "Test if 'get-all-lights' works properly"
    (check-eq?
     (length ((test-railway 'get-all-lights)))
     2
     "get-all-lights: operation does not work properly"))

   ))

(define railway-update-detection-blocks!/get-detection-block-state/get-all-detection-blocks-tests

  (test-suite
   "RAILWAY-ADT: UPDATE-DETECTION-BLOCKS!/GET-DTECTION-BLOCK-STATE/GET-ALL-DETECTION-BLOCKS TESTS"


   (test-case
    "Test if 'get-detection-block-state!' exists"
    (check-not-equal?
     (test-railway 'get-detection-block-state)
     "RAILWAY-ADT: Incorrect message"
     "get-detection-block-state: operation does not exist"))
      
   (test-case
    "Test if 'update-detection-blocks!' exists"
    (check-not-equal?
     (test-railway 'update-detection-blocks!)
     "RAILWAY-ADT: Incorrect message"
     "update-detection-blocks!: operation does not exist"))

   (test-case
    "Test if 'get-detection-block-state' works properly"
    (check-eq?
     ((test-railway 'get-detection-block-state) '1-1)
     #f
     "get-detection-block-state: Incorrect operation"))

   (test-case
    "Test if 'update-detection-blocks!' works properly"
    (check-equal?
     (begin
       ((test-railway 'update-detection-blocks!) '(1-1) '(1-1 1-2 1-3 1-4 1-5 1-6 1-7 1-8 2-1 2-2 2-3 2-4 2-5 2-6 2-7 2-8))
       ((test-railway 'get-detection-block-state) '1-1))
     #t
     "update-detection-blocks!: Incorrect operation"))

   (test-case
    "Test if 'get-all-detection-blocks' works properly"
    (check-equal?
     (length ((test-railway 'get-all-detection-blocks)))
     16
     "get-all-detection-blocks: incorrect operation"))

   ))

(define railway-compute-path-complex/compute-path-simplified-tests

  (test-suite
   "RAILWAY-ADT: COMPUTE-PATH-COMPLEX/COMPUTE-PATH-SIMPLIFIED TESTS"

   (test-case
    "Test if 'compute-path-complex' exists"
    (check-not-equal?
     (test-railway 'compute-path-complex)
     "RAILWAY-ADT: Incorrect message"
     "Compute-path-complex: operation does not exist"))

   (test-case
    "Test if 'compute-path-simplified' exists"
    (check-not-equal?
     (test-railway 'compute-path-simplified)
     "RAILWAY-ADT: Incorrect message"
     "Compute-path-simplified: operation does not exist"))

   (test-case
    "Test if 'compute-path-complex' works properly"
    (check-equal?
     ((test-railway 'compute-path-complex) '1-3 '1-2)
     '(S-27 1-2)
     "Compute-path-complex: incorrect operation"))

   (test-case
    "Test if 'compute-path-simplified' works properly"
    (check-equal?
     ((test-railway 'compute-path-simplified) '1-3 '1-2)
     '(1-4 1-2)
     "Compute-path-simplified: incorrect operation"))))

(define railway-change-train-trajectory-state!/get-train-trajectory-state-tests
  (test-suite
   "RAILWAY-ADT: CHANGE-TRAIN-TRAJECTORY-STATE!/GET-TRAIN-TRAJECTORY TESTS"

   (test-case
    "Test if 'change-train-trajectory-state!' exists"
    (check-not-equal?
     (test-railway 'change-train-trajectory-state!)
     "RAILWAY-ADT: Incorrect message"
     "change-train-trajectory-state!: operation does not exist"))

   (test-case
    "Test if 'get-train-trajectory-state' exists"
    (check-not-equal?
     (test-railway 'get-train-trajectory-state)
     "RAILWAY-ADT: Incorrect message"
     "get-train-trajectory-state: operation does not exist"))

   (test-case
    "Test if 'get-train-trajectory-state' works properply"
    (check-equal?
     ((test-railway 'get-train-trajectory-state) 'TEST-TRAIN-1)
     #f
     "get-train-trajectory-state: incorrect output"))

   (test-case
    "Test if 'change-train-trajectory-state!' works properly"
    (check-equal?
     (begin
       ((test-railway 'change-train-trajectory-state!) 'TEST-TRAIN-1 '(1-2 2-3))
       ((test-railway 'get-train-trajectory) 'TEST-TRAIN-1))
     '(1-2 2-3)
     "change-trajectory-state!: incorrect action"))

   ))

(define railway-message-sending-tests
  (test-suite
   "RAILWAY-ADT: MESSAGE SENDING TESTS"

   (test-case
    "Test if message-sending is protected against illegal messages"
    (check-equal?
     (test-railway 'illegal-message)
     "RAILWAY-ADT: Incorrect message"
     "module allows illegal message to be sent without warning"))))
    

;; Bringing all test-suites together
(define all-tests (test-suite "Railway-Module"
                              railway-make-test
                              railway-add-train-tests
                              railway-change-train-speed!/get-train-speed/get-train-track/get-train-track-behind/change-train-track!/get-all-trains-tests
                              railway-change-switch-state!/get-switch-state/get-switch-comp-state-tests
                              railway-get-all-switches-tests
                              railway-check-barrier-open?/change-barrier-state!-tests
                              railway-get-light-state/change-light-state!-tests
                              railway-get-all-lights-tests
                              railway-update-detection-blocks!/get-detection-block-state/get-all-detection-blocks-tests
                              railway-message-sending-tests))

(test/gui all-tests)
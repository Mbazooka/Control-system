;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Railway ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require graph
         "./railway-components/barrier-adt.rkt"
         "./railway-components/detection-block-adt.rkt"
         "./railway-components/light-adt.rkt"
         "./railway-components/switch-adt.rkt"
         "./railway-components/track-adt.rkt"
         "./railway-components/train-adt.rkt"
         "hardware-setup.rkt")

(provide make-railway-adt)

(define (make-railway-adt)
  (let* ((HARDWARE-SETUP (make-hardware-setup-adt))
         (HARDWARE-SWITCHES (HARDWARE-SETUP 'get-possible-switches))
         (HARDWARE-LIGHTS (HARDWARE-SETUP 'get-possible-lights))
         (HARDWARE-BARRIERS (HARDWARE-SETUP 'get-possible-barriers))
         (HARDWARE-DETECTION-BLOCKS (HARDWARE-SETUP 'get-possible-detection-blocks))
         (HARDWARE-TRACKS (HARDWARE-SETUP 'get-track-map))
         (railway-graph (unweighted-graph/undirected (HARDWARE-SETUP 'get-railway-connections)))
         (riding-trains (make-hash))) ;; No trains riding initially

    (define (add-train! train-name initial-track initial-track-behind) ;; Trains have a unique name
      (if (hash-ref riding-trains train-name #f) ;; Give back false when not in the hash-map
          #f ;; Giving back a boolean is used for other procedures (c.f. INFRABEL-ADT ADD-TRAIN)
          (let ((new-train (make-train-adt train-name initial-track initial-track-behind)))
            (hash-set! riding-trains train-name new-train)
            #t)))

    ;; Abstraction allowing more general code for change operations (avoiding code duplication)
    (define (change-operation-abstraction HARDWARE operation)
      (lambda (object-name data)
        (let ((object (hash-ref HARDWARE object-name #f)))
          (if object
              ((object operation) data)
              "RAILWAY-ADT: OBJECT DOES NOT EXIST"))))

    ;; Procedure that changes the train speed to a certain value
    (define change-train-speed! (change-operation-abstraction riding-trains 'change-speed!))

    ;; Procedure that changes the switch state to a certain state 
    (define change-switch-state! (change-operation-abstraction HARDWARE-SWITCHES 'change-position!))

    ;; Procedure that changes the light state to a certain state
    (define change-light-state! (change-operation-abstraction HARDWARE-LIGHTS 'change-light!))

    ;; Procedure that changes the detection-block state to a certain state
    (define change-detection-block-state! (change-operation-abstraction HARDWARE-DETECTION-BLOCKS 'change-presence!))

    ;; Procedure that changes the barrier state to a certain state (cannot be generalized)
    (define (change-barrier-state! barrier-name state)
      (let ((barrier-object (hash-ref HARDWARE-BARRIERS barrier-name #f)))
        (if barrier-object
            (cond
              ((eq? state 'open) ((barrier-object 'open!)))
              ((eq? state 'close) ((barrier-object 'close!)))
              (else
               "RAILWAY-ADT: Incorrect barrier state change"))
            #f)))

    ;; Another abstraction allowing more general code for get-operations
    (define (get-operation-abstraction HARDWARE operation)
      (lambda (object-name)
        (let ((object (hash-ref HARDWARE object-name)))
          ((object operation)))))

    ;; Procedure that gets the speed of the train
    (define get-train-speed (get-operation-abstraction riding-trains 'get-current-speed))

    ;; Procedure that gets the switch state
    (define get-switch-state (get-operation-abstraction HARDWARE-SWITCHES 'current-position))

    ;; Procedure that checks if barrier is open
    (define check-barrier-open? (get-operation-abstraction HARDWARE-BARRIERS 'open-barrier?))

    ;; Procedure that gets the light state
    (define get-light-state (get-operation-abstraction HARDWARE-LIGHTS 'get-state))

    ;; Procedure that gets the detection-block-state
    (define (get-detection-block-state detection-block train-name)
      (let ((detection-block-object (hash-ref HARDWARE-DETECTION-BLOCKS detection-block)))
        ((detection-block-object 'get-presence) train-name)))

    (define (dispatch msg)
      (cond
        ((eq? msg 'add-train!) add-train!)
        ((eq? msg 'change-train-speed!) change-train-speed!)
        ((eq? msg 'get-train-speed) get-train-speed) 
        ((eq? msg 'change-switch-state!) change-switch-state!) 
        ((eq? msg 'get-switch-state) get-switch-state) 
        ((eq? msg 'check-barrier-open?) check-barrier-open?)
        ((eq? msg 'change-barrier-state!) change-barrier-state!)
        ((eq? msg 'get-light-state) get-light-state)
        ((eq? msg 'change-light-state!) change-light-state!)
        ((eq? msg 'change-detection-block-state!) change-detection-block-state!)
        ((eq? msg 'get-detection-block-state) get-detection-block-state)
        (else
         "RAILWAY-ADT: Incorrect message")))
    dispatch))

    

    

    




  

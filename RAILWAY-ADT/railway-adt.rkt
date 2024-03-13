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
         "./railway-components/train-adt.rkt")

(provide make-railway-adt)

;; All the possible switches associated with the actual switch and their initial state  
(define possible-switches (hash 'S-1  (make-switch-adt 'S-1 '2-1 'S-25) ;; The switch name linked with the switch object and initial state
                                'S-2  (make-switch-adt 'S-2 'S-7 'S-3)
                                'S-3  (make-switch-adt 'S-3 '2-2 'S-8)
                                'S-4 (make-switch-adt 'S-4 '2-6 '2-7)
                                'S-5 (make-switch-adt 'S-5 '1-6 'S-7)
                                'S-6 (make-switch-adt 'S-6 '2-3 'S-20)
                                'S-7 (make-switch-adt 'S-7 'S-25 'S-2)
                                'S-8 (make-switch-adt 'S-8 '2-5 'S-4)
                                'S-9 (make-switch-adt 'S-9 'S-24 'S-11)
                                'S-10 (make-switch-adt 'S-10 '1-1 'S-16)
                                'S-11 (make-switch-adt 'S-11 'S-9 'S-10)
                                'S-12 (make-switch-adt 'S-12 'S-23 '2-3)
                                'S-16 (make-switch-adt 'S-16 'S-10 '2-8)
                                'S-20 (make-switch-adt 'S-20 '1-5 'S-6)
                                'S-23 (make-switch-adt 'S-23 'S-24 'S-12)
                                'S-24 (make-switch-adt 'S-24 'S-9 '1-3)
                                'S-25 (make-switch-adt 'S-25 'S-7 'S-1)
                                'S-26 (make-switch-adt 'S-26 'S-28 'S-27)
                                'S-27 (make-switch-adt 'S-27 '1-3 '1-2)
                                'S-28 (make-switch-adt 'S-28 '1-7 'S-26)))

;; All the possible lights
(define possible-lights (hash 'L-1 (make-light-adt 'L-1)
                              'L-2 (make-light-adt 'L-2)))

;; All the possible crossings
(define possible-barriers (hash 'C-1 (make-barrier-adt 'C-1)
                                'C-2 (make-barrier-adt 'C-2)))

;; All possible detection-blocks linked with the tracks
(define possible-detection-blocks
  (hash '1-1 (make-detection-block-adt '1-1)
        '1-2 (make-detection-block-adt '1-2)
        '1-3 (make-detection-block-adt '1-3)
        '1-4 (make-detection-block-adt '1-4)
        '1-5 (make-detection-block-adt '1-5)
        '1-6 (make-detection-block-adt '1-6)
        '1-7 (make-detection-block-adt '1-7)
        '1-8 (make-detection-block-adt '1-8)
        '2-1 (make-detection-block-adt '2-1)
        '2-2 (make-detection-block-adt '2-2)
        '2-3 (make-detection-block-adt '2-3)
        '2-4 (make-detection-block-adt '2-4)
        '2-5 (make-detection-block-adt '2-5)
        '2-6 (make-detection-block-adt '2-6)
        '2-7 (make-detection-block-adt '2-7)
        '2-8 (make-detection-block-adt '2-8)))

;; The tracks connected with each other in a list
(define railway-connections '((1-1 S-28) (1-7 S-28) 
                                         (S-26 S-27) (S-26 1-4) (S-28 S-26)
                                         (S-27 1-3) (S-27 1-2) (1-4 1-5)
                                         (1-8 S-25) (1-6 1-7)
                                         (S-25 S-1) (S-1 2-1) 
                                         (S-1 S-2) (S-5 1-6) (S-2 S-3)
                                         (S-2 S-7) (S-7 S-25) (S-3 S-8)
                                         (S-3 2-2) (S-7 S-5) (S-5 S-6)
                                         (S-6 2-3) (S-6 S-20) (S-8 S-4)
                                         (S-8 2-5) (S-4 2-7) (S-4 2-6)
                                         (S-20 2-4) (2-4 S-23)
                                         (S-23 S-12) (S-23 S-24) 
                                         (S-24 S-9) (S-9 S-11)
                                         (S-11 S-10) (S-10 1-1) (S-10 S-16)
                                         (S-16 2-8) (S-24 1-3)
                                         (S-12 2-3) (1-2 S-9) 
                                         (S-11 S-12) (1-5 S-20)
                                         ))

;; The detection blocks connected with each other in a list (simplified version of before)
(define simplified-railway-connections '((1-3 1-4) (1-4 1-2) (1-4 1-1) (1-4 1-5) (1-5 2-4)
                                                     (2-4 1-1) (2-4 1-2) (2-4 1-3) (2-4 1-6) (2-4 2-1) (2-4 1-8)
                                                     (2-3 1-1) (2-3 1-2) (2-3 1-6) (2-3 2-1) (2-3 1-8)
                                                     (2-1 2-2) (2-1 2-5) (2-1 2-6) (2-1 2-7)
                                                     (1-8 2-2) (1-8 2-5) (1-8 2-6)
                                                     (1-1 1-7) 
                                                     (1-6 2-3) (1-6 2-4) (1-6 1-7)
                                                     (1-8 2-4)))

(define (make-railway-adt) 
  (let* ((HARDWARE-SWITCHES possible-switches)
         (HARDWARE-LIGHTS possible-lights)
         (HARDWARE-BARRIERS possible-barriers)
         (HARDWARE-DETECTION-BLOCKS possible-detection-blocks)
         (railway-graph (unweighted-graph/undirected railway-connections))
         (railway-DB-graph (unweighted-graph/undirected simplified-railway-connections))
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

    ;; Procedure that changes the train destination to a certain component
    (define change-train-destination! (change-operation-abstraction riding-trains 'change-destination!))

    ;; Procedure that changes the train track to a certain track
    (define change-train-track! (change-operation-abstraction riding-trains 'change-current-track!))

    ;; Procedure that changes the train traick behind to a certain track (for update purposes)
    (define change-train-track-behind! (change-operation-abstraction riding-trains 'change-current-track-behind!))
                                                                                                                          
    ;; Procedure that changes the trajectory state
    (define change-train-trajectory-state! (change-operation-abstraction riding-trains 'change-trajectory-state!))

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

    ;; Updates the detection block (only that can be put here to make sure the dependency diagram does not become a mess)
    (define (update-detection-blocks! occupied-dbs all-dbs)
      (for-each (lambda (db-id)
                  (change-detection-block-state! db-id #t))
                occupied-dbs)
      (for-each (lambda (db-id)
                  (cond
                    ((not (member db-id occupied-dbs))
                     (change-detection-block-state! db-id #f))))
                all-dbs))
    
    ;; Another abstraction allowing more general code for get-operations
    (define (get-operation-abstraction HARDWARE operation)
      (lambda (object-name)
        (let ((object (hash-ref HARDWARE object-name)))
          ((object operation)))))

    ;; Another abstraction allowing more general get-all-operations
    (define (get-all-abstraction HARDWARE operation)
      (lambda ()
        (map (lambda (hardware-component)
               (cons hardware-component (operation hardware-component)))
             (hash-keys HARDWARE))))

    ;; Procedure that gets the speed of the train
    (define get-train-speed (get-operation-abstraction riding-trains 'get-current-speed))

    ;; Procedure that gets the current-track of/track behind the train
    (define get-train-track (get-operation-abstraction riding-trains 'get-current-track))

    (define get-train-track-behind (get-operation-abstraction riding-trains 'get-track-behind))

    (define (get-all-trains)
      (map (lambda (train-name)
             (let ((actual-train (hash-ref riding-trains train-name)))
               (cons train-name (list ((actual-train 'get-initial-track))
                                      ((actual-train 'get-initial-track-behind))
                                      ((actual-train 'get-current-speed))
                                      ((actual-train 'get-current-track))
                                      ((actual-train 'get-track-behind))
                                      ))))
           (hash-keys riding-trains)))

    ;; Procedure that gets the train trajectory state
    (define get-train-trajectory-state (get-operation-abstraction riding-trains 'get-trajectory-state))

    ;; Procedure that gets the train destination
    (define get-train-destination (get-operation-abstraction riding-trains 'get-destination))

    ;; Procedure that gets the switch state
    (define get-switch-state (get-operation-abstraction HARDWARE-SWITCHES 'current-position))

    ;; Procedure that gets the switch component state
    (define get-switch-comp-state (get-operation-abstraction HARDWARE-SWITCHES 'current-comp))

    ;; Procedure that gets the switch its possible comp states
    (define get-switch-possible-comp-states (get-operation-abstraction HARDWARE-SWITCHES 'possible-comp-states))

    ;; Gets all the hardware switches with their states
    (define get-all-switches (get-all-abstraction HARDWARE-SWITCHES get-switch-state))

    ;; Procedure that checks if barrier is open
    (define check-barrier-open? (get-operation-abstraction HARDWARE-BARRIERS 'open-barrier?))

    ;; Gets all barrier with their states
    (define get-all-barriers (get-all-abstraction HARDWARE-BARRIERS check-barrier-open?))

    ;; Procedure that gets the light state
    (define get-light-state (get-operation-abstraction HARDWARE-LIGHTS 'get-state))

    ;; Gets all lights with their states
    (define get-all-lights (get-all-abstraction HARDWARE-LIGHTS get-light-state))

    ;; Procedure that gets the detection-block-state
    (define get-detection-block-state (get-operation-abstraction HARDWARE-DETECTION-BLOCKS 'get-presence))

    ;; Procedure that gets the detection-block states for all detection-blocks
    (define (get-all-detection-blocks)
      (map
       (lambda (key)
         (cons key (get-detection-block-state key)))
       (hash-keys HARDWARE-DETECTION-BLOCKS)))

    ;; Procedures to compute paths in complex (with switches) and simplified (without switches)
    (define (compute-path-complex start destination)
      (fewest-vertices-path railway-graph start destination))

    (define rest-of-path cdr) ;; Abstraction

    (define (compute-path-simplified start destination)
      (rest-of-path (fewest-vertices-path railway-DB-graph start destination)))
      
    (define (dispatch msg)
      (cond
        ((eq? msg 'add-train!) add-train!)
        ((eq? msg 'change-train-speed!) change-train-speed!)
        ((eq? msg 'get-all-trains) get-all-trains)
        ((eq? msg 'get-train-speed) get-train-speed)
        ((eq? msg 'change-train-trajectory-state!) change-train-trajectory-state!) ;; ADDED
        ((eq? msg 'get-train-trajectory-state) get-train-trajectory-state) ;; ADDED
        ((eq? msg 'get-train-track) get-train-track) ;; ADDED
        ((eq? msg 'get-train-track-behind) get-train-track-behind) ;; ADDED
        ((eq? msg 'change-train-track!) change-train-track!) ;; ADDED
        ((eq? msg 'change-train-track-behind!) change-train-track-behind!) ;; ADDED
        ((eq? msg 'get-train-destination) get-train-destination) ;; ADDED
        ((eq? msg 'change-train-destination!) change-train-destination!) ;; ADDED 
        ((eq? msg 'change-switch-state!) change-switch-state!) 
        ((eq? msg 'get-switch-state) get-switch-state)
        ((eq? msg 'get-switch-comp-state) get-switch-comp-state) ;; ADDED
        ((eq? msg 'get-switch-possible-comp-states) get-switch-possible-comp-states) ;; ADDED
        ((eq? msg 'get-all-switches) get-all-switches)
        ((eq? msg 'check-barrier-open?) check-barrier-open?)
        ((eq? msg 'get-all-barriers) get-all-barriers)
        ((eq? msg 'change-barrier-state!) change-barrier-state!)
        ((eq? msg 'get-light-state) get-light-state)
        ((eq? msg 'get-all-lights) get-all-lights)
        ((eq? msg 'change-light-state!) change-light-state!)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        ((eq? msg 'get-detection-block-state) get-detection-block-state)
        ((eq? msg 'get-all-detection-blocks) get-all-detection-blocks)
        ((eq? msg 'compute-path-complex) compute-path-complex) ;; ADDED
        ((eq? msg 'compute-path-simplified) compute-path-simplified) ;; ADDED
        ((eq? msg 'get-track-neighbour) (lambda (component) (get-neighbors railway-DB-graph component))) ;; ADDED 
        (else
         "RAILWAY-ADT: Incorrect message")))
    dispatch))


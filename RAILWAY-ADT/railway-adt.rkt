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
(define possible-switches (hash 'S-1  (make-switch-adt 'S-1) ;; The switch name linked with the switch object and initial state
                                'S-2  (make-switch-adt 'S-2)
                                'S-3  (make-switch-adt 'S-3)
                                'S-4 (make-switch-adt 'S-4)
                                'S-5 (make-switch-adt 'S-5)
                                'S-6 (make-switch-adt 'S-6)
                                'S-7 (make-switch-adt 'S-7)
                                'S-8 (make-switch-adt 'S-8)
                                'S-9 (make-switch-adt 'S-9)
                                'S-10 (make-switch-adt 'S-10)
                                'S-11 (make-switch-adt 'S-11)
                                'S-12 (make-switch-adt 'S-12)
                                'S-16 (make-switch-adt 'S-16)
                                'S-20 (make-switch-adt 'S-20)
                                'S-23 (make-switch-adt 'S-23)
                                'S-24 (make-switch-adt 'S-24)
                                'S-25 (make-switch-adt 'S-25)
                                'S-26 (make-switch-adt 'S-26)
                                'S-27 (make-switch-adt 'S-27)
                                'S-28 (make-switch-adt 'S-28)))

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
                          
    

;; Tracks with detection-blocks
(define 1-1 (make-track-adt '1-1 #f #f 'C-1))
(define 1-2 (make-track-adt '1-2 #f #f 'C-1))
(define 1-3 (make-track-adt '1-3 'L-1 #f 'C-1))
(define 1-4 (make-track-adt '1-4 #f #f #f))
(define 1-5 (make-track-adt '1-5 'L-2 #f 'C-2))
(define 1-6 (make-track-adt '1-6 #f #f 'C-2))
(define 1-7 (make-track-adt '1-7 #f #f #f))
(define 1-8 (make-track-adt '1-8 #f #f #f))
(define 2-1 (make-track-adt '2-1 #f #f #f))
(define 2-2 (make-track-adt '2-2 #f #f #f))
(define 2-3 (make-track-adt '2-3 #f #f #f))
(define 2-4 (make-track-adt '2-4 #f #f #f))
(define 2-5 (make-track-adt '2-5 #f #f #f))
(define 2-6 (make-track-adt '2-6 #f #f #f))
(define 2-7 (make-track-adt '2-7 #f #f #f))
(define 2-8 (make-track-adt '2-8 #f #f #f)) 

;; Tracks with switches (Made for PHASE 2 purposes, not used yet)
(define T-1 (make-track-adt #f #f 'S-1 #f))
(define T-2 (make-track-adt #f #f 'S-2 #f))
(define T-3 (make-track-adt #f #f 'S-3 #f))
(define T-4 (make-track-adt #f #f 'S-4 #f))
(define T-5 (make-track-adt #f #f 'S-5 #f))
(define T-6 (make-track-adt #f #f 'S-6 #f))
(define T-7 (make-track-adt #f #f 'S-7 #f))
(define T-8 (make-track-adt #f #f 'S-8 #f))
(define T-9 (make-track-adt #f #f 'S-9 #f))
(define T-10 (make-track-adt #f #f 'S-10 #f))
(define T-11 (make-track-adt #f #f 'S-11 #f))
(define T-12 (make-track-adt #f #f 'S-12 #f))
(define T-16 (make-track-adt #f #f 'S-16 #f))
(define T-20 (make-track-adt #f #f 'S-20 #f))
(define T-23 (make-track-adt #f #f 'S-23 #f))
(define T-24 (make-track-adt #f #f 'S-24 #f))
(define T-25 (make-track-adt #f #f 'S-25 #f))
(define T-26 (make-track-adt #f #f 'S-26 #f))
(define T-27 (make-track-adt #f #f 'S-27 #f))
(define T-28 (make-track-adt #f #f 'S-28 #f))

;; The tracks connected with each other in a list
(define railway-connections '((1-1 T-28) (1-7 T-28) 
                                         (T-26 T-27) (T-26 1-4) (T-28 T-26)
                                         (T-27 1-3) (T-27 1-2) (1-4 1-5)
                                         (1-8 T-25) (1-6 1-7)
                                         (T-25 T-1) (T-1 2-1) 
                                         (T-1 T-2) (T-5 1-6) (T-2 T-3)
                                         (T-2 T-7) (T-7 T-25) (T-3 T-8)
                                         (T-3 2-2) (T-7 T-5) (T-5 T-6)
                                         (T-6 2-3) (T-6 T-20) (T-8 T-4)
                                         (T-8 2-5) (T-4 2-7) (T-4 2-6)
                                         (T-20 2-4) (2-4 T-23)
                                         (T-23 T-12) (T-23 T-24) 
                                         (T-24 T-9) (T-9 T-11)
                                         (T-11 T-10) (T-10 1-1) (T-10 T-16)
                                         (T-16 2-8) (T-16 NO-TRACK) (T-24 1-3)
                                         (T-12 2-3) (1-2 T-9) 
                                         (T-11 T-12) (1-8 NO-TRACK) (2-1 NO-TRACK)
                                         (2-2 NO-TRACK) (2-5 NO-TRACK) (2-6 NO-TRACK)
                                         (2-7 NO-TRACK) (1-5 T-20)
                                         ))

;; Hashmap for linking names and tracks
(define track-map (hash '1-1 1-1 '1-2 1-2 '1-3 1-3 '1-4 1-4
                        '1-5 1-5 '1-6 1-6 '1-7 1-7 '1-8 1-8
                        '2-1 2-1 '2-2 2-2 '2-3 2-3 '2-4 2-4
                        '2-5 2-5 '2-6 2-6 '2-7 2-7 '2-8 2-8           
                        'T-1 T-1 'T-2 T-2 'T-3 T-3 'T-4 T-4 'T-5 T-5 'T-6 T-6
                        'T-7 T-7 'T-8 T-8 'T-9 T-9 'T-10 T-10 'T-11 T-11
                        'T-12 T-12 'T-16 T-16 'T-20 T-20 'T-23 T-23
                        'T-24 T-24 'T-25 T-25 'T-26 T-26 'T-27 T-27 'T-28 T-28))

(define (make-railway-adt) 
  (let* ((HARDWARE-SWITCHES possible-switches)
         (HARDWARE-LIGHTS possible-lights)
         (HARDWARE-BARRIERS possible-barriers)
         (HARDWARE-DETECTION-BLOCKS possible-detection-blocks)
         (HARDWARE-TRACKS track-map)
         (railway-graph (unweighted-graph/undirected railway-connections))
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

    ;; Procedure that gets the speed of the train
    (define get-train-speed (get-operation-abstraction riding-trains 'get-current-speed))

    ;; Procedure that gets the switch state
    (define get-switch-state (get-operation-abstraction HARDWARE-SWITCHES 'current-position))

    (define (get-all-switches)
      (map
       (lambda (switch)
         (cons switch (get-switch-state switch)))
       (hash-keys possible-switches)))

    ;; Procedure that checks if barrier is open
    (define check-barrier-open? (get-operation-abstraction HARDWARE-BARRIERS 'open-barrier?))

    ;; Procedure that gets the light state
    (define get-light-state (get-operation-abstraction HARDWARE-LIGHTS 'get-state))

    ;; Procedure that gets the detection-block-state
    (define get-detection-block-state (get-operation-abstraction HARDWARE-DETECTION-BLOCKS 'get-presence))

    ;; Procedure that gets the detection-block states for all detection-blocks
    (define (get-all-db-states)
      (map
       (lambda (key)
         (cons key (get-detection-block-state key)))
       (hash-keys HARDWARE-DETECTION-BLOCKS)))
      

    (define (dispatch msg)
      (cond
        ((eq? msg 'add-train!) add-train!)
        ((eq? msg 'change-train-speed!) change-train-speed!)
        ((eq? msg 'get-train-speed) get-train-speed) 
        ((eq? msg 'change-switch-state!) change-switch-state!) 
        ((eq? msg 'get-switch-state) get-switch-state) ;;;;; TO BE 'REMOVED'/ TESTS ASWELL
        ((eq? msg 'get-all-switches) get-all-switches)
        ((eq? msg 'check-barrier-open?) check-barrier-open?)
        ((eq? msg 'change-barrier-state!) change-barrier-state!)
        ((eq? msg 'get-light-state) get-light-state)
        ((eq? msg 'change-light-state!) change-light-state!)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        ((eq? msg 'get-detection-block-state) get-detection-block-state)
        ((eq? msg 'get-all-db-states) get-all-db-states) 
        (else
         "RAILWAY-ADT: Incorrect message")))
    dispatch))

    

    

    




  

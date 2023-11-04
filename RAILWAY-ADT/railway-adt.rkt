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
         (HARDWARE-LIGHTS (HARDWARE-SETUP 'possible-lights))
         (HARDWARE-BARRIERS (HARDWARE-SETUP 'possible-barriers))
         (HARDWARE-DETECTION-BLOCKS (HARDWARE-SETUP 'possible-detection-blocks))
         (HARDWARE-TRACKS (HARDWARE-SETUP 'get-track-map))
         (railway-graph (unweighted-graph/undirected (HARDWARE-SETUP 'get-railway-connections)))
         (riding-trains (make-hash))) ;; No trains riding initially

    (define (add-train! train-name initial-track initial-track-behind) ;; Trains have a unique name
      (if (hash-ref riding-trains train-name #f) ;; Give back false when not in the hash-map
          "RAILWAY-ADT: Train already exists"
          (let ((new-train (make-train-adt train-name initial-track initial-track-behind)))
            (hash-set! train-name new-train))))

    (define (change-train-speed! train-name speed)
      (let ((train-object (hash-ref riding-trains train-name)))
        ((train-object 'change-speed!) speed)))

    (define (get-train-speed train-name) ;; ?????Debatable
      (let ((train-object (hash-ref riding-trains train-name)))
        (train-object 'get-current-speed)))

    (define (get-switch-state switch-name) ;; ??????Debatable
      (let ((switch-object (hash-ref HARDWARE-SWITCHES switch-name)))
        ((switch-object 'current-position!))))

    (define (change-switch-state! switch-name state) ;; STATE IS 1 (INITIAL) OR 2
      (let ((switch-object (hash-ref HARDWARE-SWITCHES switch-name))) ;; If no switch with name, you get error
        ((switch-object 'change-position!) state)))

    (define (check-barrier-open? barrier-name) ;; ??????Debatable
      (let ((barrier-object (hash-ref HARDWARE-BARRIERS barrier-name)))
        ((barrier-object 'open-barrier?))))

    (define (change-barrier-state! barrier-name state)
      (let ((barrier-object (hash-ref HARDWARE-BARRIERS barrier-name)))
        (cond
          ((eq? state 'open) ((barrier-object 'open!)))
          ((eq? state 'closed) ((barrier-object 'closed!)))
          (else
           "RAILWAY-ADT: Incorrect barrier state change"))))

    (define (get-light-state light-name)
      (let ((light-object (hash-ref HARDWARE-LIGHTS light-name)))
        ((light-object 'get-state))))

    (define (change-light-state! light-name state)
      (let ((light-object (hash-ref HARDWARE-LIGHTS light-name)))
        ((light-object 'change-light!) state)))

    (define (change-detection-block-state! track-name presence?)
      (let ((detection-block-object (hash-ref HARDWARE-LIGHTS track-name)))
        ((detection-block-object 'change-presence!) presence?)))

    (define (get-detection-block-state track-name)
      (let ((detection-block-object (hash-ref HARDWARE-LIGHTS track-name)))
        ((detection-block-object 'get-presence))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'add-train!) add-train!)
        ((eq? msg 'change-train-speed!) change-train-speed!)
        ((eq? msg 'get-train-speed) get-train-speed) ;;;;;;;
        ((eq? msg 'get-switch-state!) get-switch-state) ;;;;;;
        ((eq? msg 'change-switch-state!) change-switch-state!) ;;;;;;
        ((eq? msg 'check-barrier-open?) check-barrier-open?)
        ((eq? msg 'change-barrier-state!) change-barrier-state!)
        ((eq? msg 'get-light-state) get-light-state)
        ((eq? msg 'change-light-state!) change-light-state!)
        ((eq? msg 'change-detection-block-state!) change-detection-block-state!)
        ((eq? msg 'get-detection-block-state) get-detection-block-state)
        (else
         "RAILWAY-ADT: Incorrect message")))
    dispatch))

    

    

    




  

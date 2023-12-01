;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 NMBS ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "./GUI/GUI-adt.rkt"
         "../RAILWAY-ADT/railway-adt.rkt")

(provide make-nmbs-adt)


(define (make-nmbs-adt)
  (let ((railway (make-railway-adt))
        (gui '()))

    (define (update-component-abstraction operation) ;; Abstraction for reoccuring update operation
      (lambda (component state)
        ((railway operation) component state)))

    (define (retrieve-all-abstraction operation) ;; abstraction for reoccuring retrieve-all operation
      (lambda () ((railway operation))))

    (define update-switch! (update-component-abstraction 'change-switch-state!))
    
    (define retrieve-all-switches (retrieve-all-abstraction 'get-all-switches))

    (define (update-barrier! barrier state) 
      (let ((processed-data (if (= state 0) 'open 'close)))
        ((railway 'change-barrier-state!) barrier processed-data)))

    (define retrieve-all-barriers (retrieve-all-abstraction 'get-all-barriers))

    (define update-light! (update-component-abstraction 'change-light-state!))

    (define retrieve-all-lights (retrieve-all-abstraction 'get-all-lights))

    (define retrieve-all-detection-blocks (retrieve-all-abstraction 'get-all-detection-blocks))

    ;;;; TEST
    (define update-train! (update-component-abstraction 'change-train-speed!))

    (define retrieve-all-trains (retrieve-all-abstraction 'get-all-trains))

    (define (apply-train! train-name track track-behind)
      ((railway 'add-train!) train-name track track-behind))     
    ;;;;;;;;;

    (define update-detection-blocks!
      (lambda (data-pair)
        (let ((oc-db (car data-pair))
              (all-db (cdr data-pair)))
          ((railway 'update-detection-blocks!) oc-db all-db))))

    (set! gui (make-gui-adt update-switch! retrieve-all-switches
                            update-barrier! retrieve-all-barriers
                            update-light! retrieve-all-lights
                            retrieve-all-detection-blocks update-train!
                            apply-train! retrieve-all-trains))
      

    (define (dispatch msg)
      (cond
        ((eq? msg 'retrieve-all-switches) retrieve-all-switches)
        ((eq? msg 'apply-train!) apply-train!)
        ((eq? msg 'retrieve-all-barriers) retrieve-all-barriers)
        ((eq? msg 'retrieve-all-lights) retrieve-all-lights)
        ((eq? msg 'retrieve-all-trains) retrieve-all-trains)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "NMBS-ADT: Illegal message")))
    dispatch))
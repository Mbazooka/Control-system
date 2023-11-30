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

    (define (retrieve-all-detection-blocks)
      ((railway 'get-all-detection-blocks)))
      ;(retrieve-all-abstraction 'get-all-detection-blocks))

    (define update-trains! ;; Updates the trains on the track together with their speed
      (lambda () 
        (let ((data ((gui 'provide-trains))))
          (for-each
           (lambda (train)
             (let ((train-name (car train))
                   (train-initial (cadr train))
                   (train-behind (caddr train))
                   (speed (cadddr train)))
               ((railway 'add-train!) train-name train-initial train-behind)
               ((railway 'change-train-speed!) train-name speed)))
           data)
          data)))

    (define update-detection-blocks!
      (lambda (data-pair)
        (let ((oc-db (car data-pair))
              (all-db (cdr data-pair)))
          ((railway 'update-detection-blocks!) oc-db all-db))))

    (set! gui (make-gui-adt update-switch! retrieve-all-switches
                            update-barrier! retrieve-all-barriers
                            update-light! retrieve-all-lights
                            retrieve-all-detection-blocks))
      

    (define (dispatch msg)
      (cond
        ((eq? msg 'retrieve-all-switches) retrieve-all-switches)
        ((eq? msg 'retrieve-all-barriers) retrieve-all-barriers)
        ((eq? msg 'retrieve-all-lights) retrieve-all-lights)
        ((eq? msg 'update-trains!) update-trains!)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "NMBS-ADT: Illegal message")))
    dispatch))
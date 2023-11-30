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

    (define (update-abstraction GUI-op op) ;; Abstraction for certain reoccuring operations
      (lambda ()
        (let ((data ((gui GUI-op))))
          (for-each
           (lambda (hardware-state)
             ((railway op) (car hardware-state) (cdr hardware-state)))
           data)
          data))) ;; Give back the data for now (to be replaced by TCP connection)

    (define (update-switch! switch state) 
      ((railway 'change-switch-state!) switch state))
    
    (define (retrieve-all-switches)
      ((railway 'get-all-switches)))

    ;; TEST FOR BARRIERS
    (define (update-barrier! barrier state)
      (let ((processed-data (if (= state 0) 'open 'close)))
        ((railway 'change-barrier-state!) barrier processed-data)))

    (define (retrieve-all-barriers)
      ((railway 'get-all-barriers)))
    ;;;;;;;;;

    (define update-barriers! (update-abstraction 'provide-barriers 'change-barrier-state!))

    (define update-lights! (update-abstraction 'provide-lights 'change-light-state!))

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
          ((railway 'update-detection-blocks!) oc-db all-db) ;; To be changed
          ((gui 'update-detection-blocks!) ((railway 'get-all-db-states))))))

    (set! gui (make-gui-adt update-switch! retrieve-all-switches update-barrier! retrieve-all-barriers))
      

    (define (dispatch msg)
      (cond
        ((eq? msg 'retrieve-all-switches) retrieve-all-switches)
        ((eq? msg 'retrieve-all-barriers) retrieve-all-barriers)
        ((eq? msg 'update-lights!) update-lights!)
        ((eq? msg 'update-trains!) update-trains!)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "NMBS-ADT: Illegal message")))
    dispatch))
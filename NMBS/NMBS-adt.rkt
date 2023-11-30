;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 NMBS ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "./GUI/GUI-adt.rkt"
         "../RAILWAY-ADT/railway-adt.rkt")

(provide make-nmbs-adt)


(define (make-nmbs-adt)
  (let ((railway (make-railway-adt))
        (gui (make-gui-adt)))

    (define (update-abstraction GUI-data op) ;; Abstraction for certain reoccuring operations
      (lambda ()
        (let ((data (GUI-data)))
          (for-each
           (lambda (hardware-state)
             ((railway op) (car hardware-state) (cdr hardware-state)))
           data)
          data))) ;; Give back the data for now (to be replaced by TCP connection)

    (define update-switches! (update-abstraction (gui 'provide-switches) 'change-switch-state!))

    (define update-barriers! (update-abstraction (gui 'provide-barriers) 'change-barrier-state!))

    (define update-lights! (update-abstraction (gui 'provide-lights) 'change-light-state!))

    (define (update-trains!) ;; Updates the trains on the track together with their speed
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
        data))

    (define (update-detection-blocks! data-pair)
      (let ((oc-db (car data-pair))
            (all-db (cdr data-pair)))
        ((railway 'update-detection-blocks!) oc-db all-db) ;; To be changed
        ((gui 'update-detection-blocks!) ((railway 'get-all-db-states)))))
      

    (define (dispatch msg)
      (cond
        ((eq? msg 'update-switches!) update-switches!)
        ((eq? msg 'update-barriers!) update-barriers!)
        ((eq? msg 'update-lights!) update-lights!)
        ((eq? msg 'update-trains!) update-trains!)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "NMBS-ADT: Illegal message")))
    dispatch))
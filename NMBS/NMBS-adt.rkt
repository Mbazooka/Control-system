;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 NMBS ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../RAILWAY-ADT/railway-adt.rkt")
(require "./GUI/GUI-adt.rkt")

(provide make-nmbs-adt)


(define (make-nmbs-adt)
  (let ((railway (make-railway-adt)))

    (define (update-abstraction GUI-data op) ;; Abstraction for certain reoccuring operations
      (lambda ()
        (let ((data (GUI-data)))
          (for-each
           (lambda (hardware-state)
             ((railway op) (car hardware-state) (cdr hardware-state)))
           data)
          data))) ;; Give back the data for now (to be replaced by TCP connection)

    (define update-switches! (update-abstraction provide-switches 'change-switch-state!))

    (define update-barriers! (update-abstraction provide-barriers 'change-barrier-state!))

    (define update-lights! (update-abstraction provide-lights 'change-light-state!))

    (define (update-trains!) ;; Updates the trains on the track together with their speed
      (let ((data (provide-trains)))
        (for-each
         (lambda (train)
           (let ((train-name (car data))
                 (train-initial (cadr data))
                 (train-behind (caddr data))
                 (speed (cadddr data)))
             ((railway 'add-train!) train-name train-initial train-behind)
             ((railway 'change-train-speed!) train-name speed)))
         data)
        data))    

    (define (dispatch msg)
      (cond
        ((eq? msg 'update-switches!) update-switches!)
        ((eq? msg 'update-barriers!) update-barriers!)
        ((eq? msg 'update-lights) update-lights!)
        ((eq? msg 'update-trains!) update-trains!)
        (else
         "NMBS-ADT: Illegal message")))
    dispatch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Infrabel ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require 
  "../RAILWAY-ADT/railway-adt.rkt"
  "../simulator/interface.rkt")

(define (make-infrabel-adt)
  (let ((railway (make-railway-adt)))

    (start) ;; To start the simulator 

    ;; Adds a train to the hardware if the train-name has not been used
    (define (add-train-HARDWARE! train-name initial-track initial-track-behind)
      (cond
        (((railway 'add-train!) train-name)
         (add-loco! train-name initial-track-behind initial-track))))

    (define (get-speed-train-HARDWARE! train-name)
      (get-loco-speed train-name))

    (define (set-speed-train-HARDWARE! train-name)
      (change-train-speed!
    




       (define (dispatch msg)
         (cond
           ((eq? msg 'add-train-HARDWARE!) add-train-HARDWARE!)
           ((eq? msg 'get-speed-train-HARDWARE!) get-speed-train-HARDWARE!)
           (else
            "INFRABEL-ADT: Incorrect message")))
       dispatch))
    
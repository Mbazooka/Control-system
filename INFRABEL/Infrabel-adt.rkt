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
         (add-loco train-name initial-track-behind initial-track))))

    ;(define (get-speed-train-HARDWARE! train-name)
    ;  (get-loco-speed train-name))

    ;; Changing the train speed to a given speed
    (define (set-speed-train-HARDWARE! train-name speed)
      (cond
        (((railway 'change-train-speed!) speed)
         (set-loco-speed! train-name speed))))

    ;; Changing the switch position to a given state
    (define (set-switch-position-HARDWARE! switch switch-position)
      (cond
        (((railway 'change-switch-state!) switch switch-position)
         (set-switch-position! switch switch-position))))

    ;; Changing the barriers their state
    (define (set-barrier-state-HARDWARE! barrier barrier-state)
      (cond
        (((railway 'change-barrier-state!) barrier barrier-state)
         (cond
           ((eq? barrier-state 'open) (open-crossing! barrier))
           ((eq? barrier-state 'close) (close-crossing! barrier))
           (else
            "INFRABEL-ADT: Incorrect barrier-state ADT")))))

    ;; Changing the light their state
    (define (set-light-state-HARDWARE! light light-state)
      (cond
        (((railway 'change-light-state!) light light-state)
         (set-sign-code! light light-state))))

    ;; Update the 
    


    (define (dispatch msg)
      (cond
        ((eq? msg 'add-train-HARDWARE!) add-train-HARDWARE!)
        ((eq? msg 'set-speed-train-HARDWARE!) set-speed-train-HARDWARE!)
        ((eq? msg 'set-switch-position-HARDWARE!) set-switch-position-HARDWARE!)
        ((eq? msg 'set-barrier-state-HARDWARE!) set-barrier-state-HARDWARE!)
        ((eq? msg 'set-light-state-HARDWARE!) set-light-state-HARDWARE!)
        (else
         "INFRABEL-ADT: Incorrect message")))
    dispatch))

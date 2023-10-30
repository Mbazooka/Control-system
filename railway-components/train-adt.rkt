;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Train ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-train-adt)

(define (make-train-adt name track initial-orientation)
  (let ((speed 0)
        (orientation initial-orientation))

    (define (get-name) name)

    (define (get-current-speed) speed)

    (define (change-speed! input-speed)
      (set! speed input-speed))

    (define (change-orientation! input-orientation)
      (set! orientation input-orientation))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-current-speed) get-current-speed)
        ((eq? msg 'change-speed!) change-speed!)
        ((eq? msg 'change-orientation!) change-orientation!)
        (else
         "TRAIN-ADT: Incorrect message")))
    dispatch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Traffic Light ADT             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (make-traffic-light-adt name)

  (define (change-light! code)
    (set-sign-code! name code))

  (define (dispatch msg)
    (cond
      ((eq? msg 'change-light!) change-light!)
      (else
       "TRAFFIC-LIGHT: Incorrect message")))
  dispatch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Traffic Light ADT             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(#%require (only racket/base time error))
(provide make-traffic-light-adt)

(define possible-lights '(L-1 L-2))
(define possible-states'(Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))

(define (make-traffic-light-adt name) 
  (unless (member name possible-lights) ;; To avoid invalid names to be used and getting weird error from Hardware 
    (error "TRAFFIC-LIGHT: Invalid name"))

  (define (change-light! code)
    (if (member code possible-states)
        (set-sign-code! name code)
        "TRAFFIC-LIGHT: Cannot change light to this state"))

  (define (dispatch msg)
    (cond
      ((eq? msg 'change-light!) change-light!)
      (else
       "TRAFFIC-LIGHT: Incorrect message")))
  dispatch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Railroad switch ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (make-railroad-switch-adt symbol)

  (define (current-position)
    (get-switch-position symbol))

  (define (change-position! state)
    (cond
      ((eq? state 'initial) (set-switch-position! 1)) ;; Initial zet de switch op de initiele positie (1)
      ((eq? state 'deviate) (set-switch-position! 2)) ;; Deviaite verander de positie van de switch (2)
      (else
       "RAILROAD-SWITCH: Incorrect state in change-position!")))

  (define (dispatch msg)
    (cond
      ((eq? msg 'current-position) current-position)
      ((eq? msg 'change-position!) change-position)
      (else
       "RAILROAD-SWITCH: Incorrect message")))
  dispatch)
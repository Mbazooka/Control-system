;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 switch ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(#%require (only racket/base time error)) ;; Library for error messages
(provide make-switch-adt)

(define (make-switch-adt name)

  (let ((state 1))

    (define (get-name) name)
    
    (define (current-position) state)

    (define (change-position! input-state)
      (cond
        ((= input-state 1) (set! state 'left))       
        ((= input-state 2) (set! state 'right))
        (else
         "SWITCH-ADT: Incorrect state in change-position!")))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'current-position) current-position)
        ((eq? msg 'change-position!) change-position!)
        (else
         "SWITCH-ADT: Incorrect message")))
    dispatch))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 switch ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(#%require (only racket/base time error)) ;; Library for error messages
(provide make-switch-adt)

(define possible-switch-names '(S-1 S-2 S-3 S-4 S-5 S-6 S-7 S-8 S-9
                                S-10 S-11 S-12 S-16 S-20 S-23 S-24
                                S-25 S-26 S-27 S-28))

(define (make-switch-adt name)
  (unless (member name possible-switch-names)
    (error "SWITCH-ADT: Invalid name"))

  (let ((state 'straight)) ;; 2 states, straight, rotate

    (define (current-position) state)

    (define (change-position! state)
      (cond
        ((eq? state 'straight) (set! state 'straight))
        ((eq? state 'deviate) (set! state 'deviate))
        (else
         "SWITCH-ADT: Incorrect state in change-position!")))

    (define (dispatch msg)
      (cond
        ((eq? msg 'current-position) current-position)
        ((eq? msg 'change-position!) change-position!)
        (else
         "SWITCH-ADT: Incorrect message")))
    dispatch))
  
                      
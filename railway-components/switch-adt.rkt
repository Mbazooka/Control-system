;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 switch ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(#%require (only racket/base time error)) ;; Library for error messages
(provide make-switch-adt)

(define possible-switch-names '(S-1 S-2 S-3 S-4 S-5 S-6 S-7 S-8 S-9
                                    S-10 S-11 S-12 S-16 S-20 S-23 S-24
                                    S-25 S-26 S-27 S-28))

(define (make-switch-adt name initial-orientation)

  ;; Test to make sure an incorrect switch cannot be made
  (unless (member name possible-switch-names)
    (error "SWITCH-ADT: Invalid name"))

  (unless (or (eq? initial-orientation 'left) (eq? initial-orientation 'right))
    (error "SWITCH-ADT: Invalid orientation given"))

  (let ((state initial-orientation)) ;; 2 states, left, right because they work on railway level

    (define (get-name) name)
    
    (define (current-position) state)

    (define (change-position! input-state)
      (cond
        ((eq? input-state 'left) (set! state 'left))
        ((eq? input-state 'right) (set! state 'right))
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
  

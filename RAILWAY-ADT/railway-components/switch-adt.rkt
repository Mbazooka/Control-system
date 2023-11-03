;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 switch ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(#%require (only racket/base time error)) ;; Library for error messages
(provide make-switch-adt)

(define (make-switch-adt name initial-orientation . special?) ;; Special? check whether it's the special 2-3 switch

  ;; Test to make sure an incorrect switch cannot be made
  (unless (or (eq? initial-orientation 'left) (eq? initial-orientation 'right) (if (not (null? special?)) (eq? initial-orientation 'middle) #f))
    (error "SWITCH-ADT: Invalid orientation given"))

  (let ((state initial-orientation)) ;; In general 2 states, left, right (unless it's the special S-2-3 switch then 3)

    (define (get-name) name)
    
    (define (current-position) state)

    (define (change-position! input-state)
      (cond
        ((eq? input-state 'left) (set! state 'left))
        ((eq? input-state 'middle) (if (not (null? special?)) (set! state 'middle) "SWITCH-ADT: Incorrect state in change-position!"))
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
  

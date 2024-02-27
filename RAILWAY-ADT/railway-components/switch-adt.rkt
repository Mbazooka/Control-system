;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 switch ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-switch-adt)

(define (make-switch-adt name initial-comp-name other-comp-name)

  (let ((state 1)
        (state-comp-name initial-comp-name)) ;; Name of the track it's pointing to

    (define (get-name) name)
    
    (define (current-position) state)

    (define (current-comp) state-comp-name)

    (define (change-position! input-state)
      (cond ;; Only allows valid input states
        ((= input-state 1) (set! state 1) (set! state-comp-name initial-comp-name))       
        ((= input-state 2) (set! state 2) (set! state-comp-name other-comp-name))
        (else
         "SWITCH-ADT: Incorrect state in change-position!")))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'current-position) current-position)
        ((eq? msg 'current-comp) current-comp) ;; ADDED
        ((eq? msg 'change-position!) change-position!)
        (else
         "SWITCH-ADT: Incorrect message")))
    dispatch))
  

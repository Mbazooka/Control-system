;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Train ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(#%require (only racket/base time error)) ;; Library for error messages
(provide make-train-adt)

(define (make-train-adt name initial-track initial-orientation)
  (unless (or (eq? initial-orientation '+)
              (eq? initial-orientation '-))
    (error "TRAIN-ADT: Invalid orientation given"))
  
  (let ((speed 0)
        (orientation initial-orientation))

    (define (get-name) name)

    (define (get-initial-track) initial-track)

    (define (get-current-speed) speed)

    (define (get-orientation) orientation)

    (define (change-speed! input-speed)
      (set! speed input-speed))

    (define (change-orientation! input-orientation)
      (set! orientation input-orientation))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-initial-track) get-initial-track)
        ((eq? msg 'get-current-speed) get-current-speed)
        ((eq? msg 'change-speed!) change-speed!)
        ((eq? msg 'get-orientation) get-orientation)
        ((eq? msg 'change-orientation!) change-orientation!)
        (else
         "TRAIN-ADT: Incorrect message")))
    dispatch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           detection-block ADT             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-detection-block-adt)

(define (make-detection-block-adt name)
  (let ((train-presence? #f)
        (reservation-data #f))

    (define (get-name) name)

    (define (get-presence)
      train-presence?)

    (define (change-presence! state)
      (if (boolean? state) ;; Changes the state only if input is boolean
          (set! train-presence?  state)
          "change-presence!: illegal value"))

    (define (reserve! train-name)
      (set! reservation-data train-name))

    (define (get-reservation) reservation-data) 

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-presence) get-presence)
        ((eq? msg 'change-presence!) change-presence!)
        ((eq? msg 'reserve!) reserve!) 
        ((eq? msg 'get-reservation) get-reservation)
        (else
         "DETECTION-BLOCK-ADT: Incorrect message")))
    dispatch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           detection-block ADT             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-detection-block-adt)

(define (make-detection-block-adt name)
  (let ((train-presence? #f))

    (define (get-name) name)

    (define (get-presence)
      train-presence?)

    (define (change-presence! state)
      (if (boolean? state) ;; Changes the state only if input is boolean
          (set! train-presence?  state)
          "change-presence!: illegal value"))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-presence) get-presence)
        ((eq? msg 'change-presence!) change-presence!)
        (else
         "DETECTION-BLOCK-ADT: Incorrect message")))
    dispatch))

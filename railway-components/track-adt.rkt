;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Track ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-track-adt)

(define (make-track-adt name detection-block-name light-name switch-name barrier-name)

  (define (get-name) name)

  (define (get-detection-block-name) detection-block-name)

  (define (get-light-name) light-name)

  (define (get-switch-name) switch-name)

  (define (get-barrier-name) barrier-name)

  (define (dispatch msg)
    (cond
      ((eq? msg 'get-name) get-name)
      ((eq? msg 'get-detection-block-name) detection-block-name)
      ((eq? msg 'get-light-nae) light-name)
      ((eq? msg 'switch-name) switch-name)
      ((eq? msg 'barrier-name) barrier-name)
      (else
       "TRACK-ADT: Incorrect message")))
  dispatch)
          
  
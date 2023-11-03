;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Track ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-track-adt)

;; Track keeps link between different railway components (using names for efficiency, not actual objects)
(define (make-track-adt name detection-block-name light-name switch-name barrier-name)

  (define (get-name) name)

  (define (get-detection-block-name) detection-block-name)

  (define (get-light-name) light-name)

  (define (get-switch-name) switch-name)

  (define (get-barrier-name) barrier-name)

  (define (dispatch msg)
    (cond
      ((eq? msg 'get-name) get-name)
      ((eq? msg 'get-detection-block-name) get-detection-block-name)
      ((eq? msg 'get-light-name) get-light-name)
      ((eq? msg 'get-switch-name) get-switch-name)
      ((eq? msg 'get-barrier-name) get-barrier-name)
      (else
       "TRACK-ADT: Incorrect message")))
  dispatch)
          
  
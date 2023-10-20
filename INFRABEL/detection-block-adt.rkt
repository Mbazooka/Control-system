;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Detection block ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require unstable/error)
(provide make-detection-block-adt
         get-all-detection-blocks)

(define get-all-detection-blocks get-detection-block-ids)

(define possible-detection-blocks (get-all-detection-blocks))
                                    

(define (make-detection-block-adt name)
  (unless (member name possible-detection-blocks)
    (error 'DETECTION-BLOCK "Invalid name"))

  (define (occupied?)
    (not (false? (member name (get-all-occupied-detection-blocks)))))

  (define (dispatch msg)
    (cond
      ((eq? msg 'occupied?) occupied?)
      (else
       "DETECTION-BLOCK: Incorrect message")))
  dispatch)
    













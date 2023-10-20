;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Detection block ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(#%require (only racket/base time error))
(provide make-detection-block-adt
         get-all-detection-blocks)

(define get-all-detection-blocks get-detection-block-ids)
                                    
(define (make-detection-block-adt name)
  (unless (member name (get-detection-block-ids))
    (error "DETECTION-BLOCK: Invalid name"))

  (define (occupied?)
    (not (false? (member name (get-all-occupied-detection-blocks)))))

  (define (dispatch msg)
    (cond
      ((eq? msg 'occupied?) occupied?)
      (else
       "DETECTION-BLOCK: Incorrect message")))
  dispatch)
    













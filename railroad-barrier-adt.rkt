;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Railroad Barrier ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (railroad-barrier-adt symbol)
  (let ((
  
  (define (up!)
    (open-crossing! symbol))

  (define (down!)
    (close-crossing! symbol))

  (define (state?)
    (
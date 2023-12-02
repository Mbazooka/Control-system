;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                barrier ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-barrier-adt)

(define (make-barrier-adt name)
  (let ((open? #t)) ;; #t means that the barrier is open, #f means not closed

    (define (get-name) name)
    
    (define (open!)
      (set! open? #t))

    (define (close!)
      (set! open? #f))

    (define (open-barrier?) open?)

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'open!) open!)
        ((eq? msg 'close!) close!)
        ((eq? msg 'open-barrier?) open-barrier?)
        (else
         "BARRIER-ADT: Incorrect message")))
    dispatch))
  
  

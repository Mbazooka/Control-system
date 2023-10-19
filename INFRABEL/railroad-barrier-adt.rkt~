;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Railroad Barrier ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (railroad-barrier-adt name)
  (let ((open? #t)) ;; #t is open, #f is closed
    
    (define (up!)
      (cond
        ((not open?) (open-crossing! name) (set! open? #t))))

    (define (down!)
      (cond
        (open? (close-crossing! name) (set! open? #f))))

    (define (open?) open)
    (define (closed?) (not open?))

    (define (dispatch msg)
      (cond
        ((eq? msg 'up!) up!)
        ((eq? msg 'down!) down!)
        ((eq? msg 'open?) open?)
        ((eq? msg 'closed?) closed?)
        (else
         "RAILROAD-BARRIER: Incorrect message")))
    dispatch))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Railroad Barrier ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!Still needs to be made fool-proof (add time)!!
;; !!!!!!!!!AND REMOVE THE SLEEP FUNCTIONS!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#lang racket

(require "../simulator/interface.rkt")
(#%require (only racket/base time error))
(provide make-railroad-barrier-adt)

(define possible-barriers '(C-1 C-2))

(define (make-railroad-barrier-adt name)
  (unless (member name possible-barriers) ;; To avoid invalid names to be used and getting weird error from Hardware 
    (error "RAILROAD-BARRIER: Invalid name"))
  
  (let ((open? #t)) ;; #t is open, #f is closed
    
    (define (up!) ;; Opens crossing if not already open
      (cond
        ((not open?) (open-crossing! name) (set! open? #t))
        (else
         "RAILROAD-BARRIER: Crossing already open")))

    (define (down!) ;; Closes crossing if not already closed
      (cond
        (open? (close-crossing! name) (set! open? #f)) 
        (else
         "RAILROAD-BARRIER: Crossing already closed")))

    (define (open?) open?)
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

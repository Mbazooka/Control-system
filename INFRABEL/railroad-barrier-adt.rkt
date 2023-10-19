;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Railroad Barrier ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!Still needs to be made fool-proof (add time)!!
;; !!!!!!!!!AND REMOVE THE SLEEP FUNCTIONS!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!CHANGE THE TESTS!!!!!!!!!!!!!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#lang racket

(require "../simulator/interface.rkt")
(provide make-railroad-barrier-adt)

(define (make-railroad-barrier-adt name)
  (let ((open? #t)) ;; #t is open, #f is closed
    
    (define (up!) ;; Opens crossing if not already open
      (cond
        ((not open?) (open-crossing! name) (sleep 6) (set! open? #t)) ;; sleep is for testing (MUST BE REMOVED)
        (else
         "RAILROAD-BARRIER: Crossing already open")))

    (define (down!) ;; Closes crossing if not already closed
      (cond
        (open? (close-crossing! name) (sleep 6) (set! open? #f)) ;; sleep is for testing (MUST BE REMOVED)
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

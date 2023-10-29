;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              light ADT Test               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(#%require (only racket/base time error)) ;; Library for error messages
(provide make-light-adt)

(define possible-light-names '(L-1 L-2))
(define possible-states '(Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))

(define (make-light-adt name)
  (unless (member name possible-light-names)
    (error "LIGHT-ADT: Invalid name"))
  
  (let ((state 'Hp0))

    (define (get-state) state)

    (define (change-light! code)
      (if (member code possible-states)
          (set! state code)
          "LIGHT-ADT: Incorect code given"))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-state) get-state)
        ((eq? msg 'change-light!) change-light!)
        (else
         "LIGHT-ADT: Incorrect message")))
    dispatch))


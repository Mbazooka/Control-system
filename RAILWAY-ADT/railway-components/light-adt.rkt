;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              light ADT Test               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-light-adt)

(define possible-states '(Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))

(define (make-light-adt name)
  (let ((state 'Hp0))

    (define (get-name) name)

    (define (get-state) state)

    (define (change-light! code) ;; Changes the state if valid state
      (if (member code possible-states)
          (set! state code)
          "LIGHT-ADT: Incorect code given"))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-state) get-state)
        ((eq? msg 'change-light!) change-light!)
        (else
         "LIGHT-ADT: Incorrect message")))
    dispatch))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Railroad switch ADT Test          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require rackunit)
(require "traffic-light-adt.rkt")

(setup-hardware)

;; Defining traffic-light for testing
(define traffic-light-1 (make-traffic-light-adt 'L-1))

;; Check if changing to each state works properly
(define-check (traffic-light-state state)
  (let ((output-function ((traffic-light-1 'change-light!) state)))
    (unless (or (void? output-function)
                (equal? output-function "TRAFFIC-LIGHT: Cannot change light to this state"))              
      (fail-check "Incorrect behaviour in change-light!"))))

;; Correct states that can be given as input
(traffic-light-state 'Hp0)
(traffic-light-state 'Hp1)
(traffic-light-state 'Hp0+Sh0)
(traffic-light-state 'Ks1+Zs3)
(traffic-light-state 'Ks2)
(traffic-light-state 'Ks2+Zs3)
(traffic-light-state 'Sh1)
(traffic-light-state 'Ks1+Zs3+Zs3v)

;; Wrong input which should not be given as input
(traffic-light-state 'hp0)
(traffic-light-state 'HP1)
(traffic-light-state 'hp0+sh0)
(traffic-light-state 'ks1+ZS3)
(traffic-light-state 'KS2)
(traffic-light-state 'KS2+ZS3)
(traffic-light-state 'sh1)
(traffic-light-state 'KS1+zs3+ZS3V)


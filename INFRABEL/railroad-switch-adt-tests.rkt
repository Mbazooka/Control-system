;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Railroad switch ADT Test          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require rackunit)
(require "railroad-switch-adt.rkt")

(setup-hardware)

(define switch-1 (make-railroad-switch-adt 'S-1))

;; Check if the initial position is indeed the initial position (basically 1 on the hardware)
(check-eq? ((switch-1 'current-position)) 1 "Incorrect initial position")

;; Checks if the change-position with deviate makes the switch change to a deviating position
((switch-1 'change-position!) 'deviate)
(check-eq? ((switch-1 'current-position)) 2 "Incorrect deviate operation")

;; Checks if the change-position with initial makes the switch change to the initial position
((switch-1 'change-position!) 'initial)
(check-eq? ((switch-1 'current-position)) 1 "Incorrect deviate operation")

;; Check if the operation get-all-switches gives back all the switches
(check-equal? (get-all-switches) (get-switch-ids) "Incorrect implementation of get-all-switches")




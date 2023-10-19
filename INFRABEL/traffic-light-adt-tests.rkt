;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Railroad switch ADT Test          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require rackunit)
(require "traffic-light-adt.rkt")

(setup-hardware)

(define traffic-light-1 (make-traffic-light-adt 'L-1))




#lang racket

(require "Infrabel-adt.rkt")

(define infrabel (make-infrabel-adt))

(define (test-route)
  ((infrabel 'add-train-HARDWARE!) 'RHODA '1-1 'S-10)
  ((infrabel 'set-speed-train-HARDWARE!) 'RHODA 200))

(thread test-route)
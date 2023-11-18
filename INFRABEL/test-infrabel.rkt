#lang racket

(require "Infrabel-adt.rkt")

(define infrabel (make-infrabel-adt))

(define (test-route)
  ((infrabel 'add-train-HARDWARE!) 'RHODA '1-1 'S-10)
  (sleep 3)
  ((infrabel 'set-speed-train-HARDWARE!) 'RHODA 200)
  (sleep 3)
  ((infrabel 'set-switch-position-HARDWARE!) 'S-1 2)
  (sleep 3)
  ((infrabel 'set-barrier-state-HARDWARE!) 'C-1 'close)
  (sleep 3)
  ((infrabel 'set-light-state-HARDWARE!) 'L-1 'Hp0+Sh0)
  (sleep 3)
  ((infrabel 'update-detection-blocks!))

  )

(thread test-route)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Railroad Barrier ADT Test         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require rackunit)
(require "railroad-barrier-adt.rkt")

(setup-hardware)

;; Defining barrier for testing
(define barrier (make-railroad-barrier-adt 'C-1))

;; Check if close and open respectively work properly 
(define-check (check-down? barrier)
  ((barrier 'down!))
  (sleep 6) ;; To avoid problems with consecutive tests (Actual code has taken this into account)
  (unless (barrier 'down?)
    (fail-check "Incorrect down! operation")))

(define-check (check-up? barrier)
  ((barrier 'up!))
  (unless (barrier 'up?)
    (fail-check "Incorrect up! operation")))

(check-down? barrier)
(check-up? barrier)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Railroad switch ADT Test          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require rackunit)
(require "railroad-switch-adt.rkt")

(setup-hardware)

;; Defining switches for testing
(define switch-1 (make-railroad-switch-adt 'S-1))
(define switch-2 (make-railroad-switch-adt 'S-2))
(define switch-3 (make-railroad-switch-adt 'S-3))

;; Checks if the current position just after construction
;; is the initial position to begin with (basically state 1 on the hardware)
(check-eq? ((switch-1 'current-position)) 1 "Incorrect initial position")
(check-eq? ((switch-2 'current-position)) 1 "Incorrect initial position")
(check-eq? ((switch-3 'current-position)) 1 "Incorrect initial position")

;; Checks if the change-position with deviate makes the switch change to a deviating position
(define-check (check-deviate? switch)
  ((switch 'change-position!) 'deviate)
  (unless (= ((switch 'current-position)) 2)
    (fail-check "Incorrect deviate operation in change-position!")))

(check-deviate? switch-1)
(check-deviate? switch-2)
(check-deviate? switch-3)

;; Checks if the change-position with initial makes the switch change to the initial position
(define-check (check-initial? switch)
  ((switch 'change-position!) 'initial)
  (unless (= ((switch 'current-position)) 1)
    (fail-check "Incorrect initial operation in change-position!")))

(check-initial? switch-1)
(check-initial? switch-2)
(check-initial? switch-3)

;; Checks if the initial-position? operation works properly
(check-eq? ((switch-1 'initial-position?)) #t)
(check-eq? ((switch-2 'initial-position?)) #t)
(check-eq? ((switch-3 'initial-position?)) #t)

;; Checks if the operation get-all-switches gives back all the switches
(check-equal? (get-all-switches) (get-switch-ids) "Incorrect implementation of get-all-switches")

;; Checks if an incorrect message triggers the right behaviour
(check-equal? (switch-1 'Incorrect-Message) "RAILROAD-SWITCH: Incorrect message" "Incorrect message behaviour")
(check-equal? (switch-2 'Incorrect-Message) "RAILROAD-SWITCH: Incorrect message" "Incorrect message behaviour")
(check-equal? (switch-3 'Incorrect-Message) "RAILROAD-SWITCH: Incorrect message" "Incorrect message behaviour")

;; Checks if an incorrect input symbol given to change-position! triggers the right behaviour
(check-equal? ((switch-1 'change-position!) 'Rotate) "RAILROAD-SWITCH: Incorrect state in change-position!")
(check-equal? ((switch-2 'change-position!) 'Rotate) "RAILROAD-SWITCH: Incorrect state in change-position!")
(check-equal? ((switch-3 'change-position!) 'Rotate) "RAILROAD-SWITCH: Incorrect state in change-position!")







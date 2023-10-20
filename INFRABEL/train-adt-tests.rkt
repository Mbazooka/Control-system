;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Train ADT Test              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require rackunit)
(require "train-adt.rkt")

(setup-hardware)

;; Defining a train for testing
(define train-1 (make-train-adt 'TEST-TRAIN '1-5 '1-4))

;; General check to see if riding? works correctly
(define-check (check-riding? train expected-boolean)
  (unless (eq? ((train 'riding?)) expected-boolean)
    (fail-check "Incorrect riding? operation")))

;; General check to see if change-speed! and get-current-speed! work correctly
(define-check (check-change-speed! train speed expected-output)
  ((train 'change-speed!) speed)
  (unless (equal? ((train 'get-current-speed)) expected-output)
    (fail-check "Incorrect change-speed! operation")))

;; Check to see if get-current-speed agrees with initial conditions
(check-eq? ((train-1 'get-current-speed)) 0 "Incorrect get-current-speed! operation")

;; Check to see if riding? agrees with initial conditions
(check-riding? train-1 #f)

;; Checks if check-change-speed! agrees with described behaviour (can't change speed when not riding)
(check-change-speed! train-1 200 0 "Incorrect change-speed! operation, should not allow to start OR incorrect get-current-speed!")

;; Check if start! operation works correctly
((train-1 'start!) 200)
(check-riding? train-1 #t)
(check-eq? ((train-1 'get-current-speed)) 200 "Incorrect start! operation")
(check-equal? ((train-1 'start!) 300) "TRAIN: Train has already started riding")

;; Checks to see if the change-speed! and get-current-speed operations work properly
(check-riding? train-1 #t)
(check-change-speed! train-1 100 100)
(check-riding? train-1 #t)
(check-change-speed! train-1 300 300)
(check-riding? train-1 #t)
(check-change-speed! train-1 0 300)
(check-riding? train-1 #t)

;; General check to see if go-forward!/go-backward! and get-current-speed! works correctly
(define-check (check-go-forward! train expected-output)
  ((train 'go-forward!))
  (unless (equal? ((train 'get-current-speed)) expected-output)
    (fail-check "Incorrect go-forward! or get-current-position operation")))

(define-check (check-go-backward! train expected-output)
  ((train 'go-backward!))
  (unless (equal? ((train 'get-current-speed)) expected-output)
    (fail-check "Incorrect go-backward! or get-current-position operation")))

;; Checks if the go-forward! and get get-current-speed operations work correctly
(check-go-forward! train-1 300)
(check-equal? ((train-1 'go-forward!)) "TRAIN: Train is already going forward or is idle")
(check-go-backward! train-1 -300)
(check-equal? ((train-1 'go-backward!)) "TRAIN: Train is already going backward or is idle")

;; Checks if stop! operation works correctly
((train-1 'stop!))
(check-eq? ((train-1 'get-current-speed)) 0 "Incorrect stop! operation")
(check-riding? train-1 #f)
(check-equal? ((train-1 'stop!)) "TRAIN: Train has already stopped")


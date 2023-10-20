;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Detection-block ADT Test         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(require rackunit)
(require "detection-block-adt.rkt")


(setup-hardware)

;; Defining detection-block for testing
(define detection-block-1 (make-detection-block-adt '1-2))
(define detection-block-2 (make-detection-block-adt '2-4))

;; Checks, when there are no trains on the tracks, if it produces correct output
(check-eq? ((detection-block-1 'occupied?)) #f "Incorrect output by occupied?")
(check-eq? ((detection-block-2 'occupied?)) #f "Incorrect output by occupied?")

;; Add trains to the track for testing
(add-loco 'TEST-TRAIN-1 'S-27 '1-2)
(add-loco 'TEST-TRAIN-2 'S-23 '2-4)

;; Checks, when there are trains on the tracks, if it produces correct output
(check-eq? ((detection-block-1 'occupied?)) #t "Incorrect output by occupied?")
(check-eq? ((detection-block-2 'occupied?)) #t "Incorrect output by occupied?")

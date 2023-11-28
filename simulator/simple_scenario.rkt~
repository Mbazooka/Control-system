#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       scenario.rkt
;
;       Example scenario.
;

(require "interface.rkt")

(define (set-speed-at train detection-block speed)
  (if (member detection-block (get-occupied-detection-blocks))
      (set-loco-speed! train speed)
      (begin (sleep 2)
             (set-speed-at train detection-block speed))))

(define (back-and-forth)
  (define train 'TEST-ROUTE-TRAIN)
  (add-loco train 'D6 'D7)
  (define train-2 'TEST-2)
  (add-loco train-2 'D4 'D5)
  (define train-3 'TEST-3)
  (add-loco train-3 'D3 'D4)
  (define train-4 'TEST-4)
  (add-loco train-4 'D2 'D3)
  (define train-5 'test-5)
  (add-loco train-5 'D1 'D2)
  (define train-6 'TEST-6)
  (add-loco train-6 'D8 'D1)
  (set-loco-speed! train 0)
  (set-loco-speed! train-2 200)
  (set-loco-speed! train-3 200)
  (set-loco-speed! train-4 200)
  (set-loco-speed! train-5 200)
  (set-loco-speed! train-6 200)

  (define (loop)
    (loop))
  (loop))

; Load the hardware track
(setup-loop)

; Start the simulator
(start)

(thread back-and-forth)
  

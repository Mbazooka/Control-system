;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Hardware and simulator abstraction    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require 
    (prefix-in SIM: "./simulator/interface.rkt")
    (prefix-in HARD: "./hardware-library/interface.rkt"))

(provide setup-hardware start stop get-loco-speed add-loco
         set-loco-speed! get-detection-block-ids
         get-occupied-detection-blocks get-switch-ids
         get-switch-position set-switch-position!
         open-crossing! close-crossing! set-sign-code!)

;; Add hardware setup function

(define (pattern-abstraction selection sim-proc hard-proc)
  (cond
    ((eq? selection 0) (sim-proc))
    ((eq? selection 1) (hard-proc))
    (else "HardSim: Incorrect selection")))

(define (setup-hardware selection)
  (pattern-abstraction selection SIM:setup-hardware (lambda (x) 1))) ;; Gives back one to indicate this procedure is just a dummy procedure that does nothing

(define (start selection)
  (pattern-abstraction selection SIM:start HARD:start))

(define (stop selection)
  (pattern-abstraction selection SIM:stop HARD:stop))

(define (add-loco selection name initial behind)
  (pattern-abstraction selection
                       (lambda () (SIM:add-loco name initial behind))
                       (lambda () (HARD:add-loco name initial behind))))

(define (get-loco-speed selection train)
  (pattern-abstraction selection
                       (lambda () (SIM:get-loco-speed train))
                       (lambda () (HARD:get-loco-speed train))))

(define (set-loco-speed! selection train speed)
  (pattern-abstraction selection
                       (lambda () (SIM:set-loco-speed! train speed))
                       (lambda () (HARD:set-loco-speed! train speed))))

(define (get-detection-block-ids selection)
  (pattern-abstraction selection
                       (lambda () (SIM:get-detection-block-ids))
                       (lambda () (HARD:get-detection-block-ids))))

(define (get-occupied-detection-blocks selection)
  (pattern-abstraction selection
                       (lambda () (SIM:get-occupied-detection-blocks))
                       (lambda () (HARD:get-occupied-detection-blocks))))

(define (get-switch-ids selection)
  (pattern-abstraction selection
                       (lambda () (SIM:get-switch-ids))
                       (lambda () (HARD:get-switch-ids))))

(define (get-switch-position selection switch)
  (pattern-abstraction selection
                       (lambda () (SIM:get-switch-position switch))
                       (lambda () (HARD:get-switch-position switch))))

(define (set-switch-position! selection switch number)
  (pattern-abstraction selection
                       (lambda () (SIM:set-switch-position! switch number))
                       (lambda () (HARD:set-switch-position! switch number))))

(define (open-crossing! selection crossing)
  (pattern-abstraction selection
                       (lambda () (SIM:open-crossing! crossing))
                       (lambda () (HARD:open-crossing! crossing))))

(define (close-crossing! selection crossing)
  (pattern-abstraction selection
                       (lambda () (SIM:close-crossing! crossing))
                       (lambda () (HARD:close-crossing! crossing))))

(define (set-sign-code! selection light code)
  (pattern-abstraction selection
                       (lambda () (SIM:set-sign-code! light code))
                       (lambda () (HARD:set-sign-code! light code))))
                       
       
       
	


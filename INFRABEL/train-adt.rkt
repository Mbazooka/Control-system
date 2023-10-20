;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Train ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(provide make-train-adt)

(define (make-train-adt name track track-behind)
  (add-loco name track track-behind)

  (define (get-current-speed)
    (get-loco-speed name))

  (define (riding?)
    (not (= (get-current-speed) 0)))
  
  (define (change-speed! speed) 
    (cond
      ((= speed 0) "TRAIN: Can't stop train with change-speed! operation")
      ((riding?) (set-loco-speed! name speed)) ;; Only if train is riding you can change it's speed
      (else
       "TRAIN: Can't change speed of non riding train")))

  (define (start! initial-speed)
    (cond
      ((not (riding?)) (set-loco-speed! name initial-speed))
      (else
       "TRAIN: Train has already started riding")))
      
  (define (stop!)
    (cond
      ((riding?) (set-loco-speed! name 0))
      (else
       "TRAIN: Train has already stopped")))

  (define (go-forward!)
    (let ((spd (get-current-speed)))
      (if (< spd 0)
          (set-loco-speed! name (- spd))
          "TRAIN: Train is already going forward or is idle")))

  (define (go-backward!)
    (let ((spd (get-current-speed)))
      (if (> spd 0)
          (set-loco-speed! name (- spd))
          "TRAIN: Train is already going backward or is idle")))

  ;; Go-to-destination! to be added in phase2

  (define (dispatch msg)
    (cond
      ((eq? msg 'get-current-speed) get-current-speed)      
      ((eq? msg 'riding?) riding?)
      ((eq? msg 'change-speed!) change-speed!)
      ((eq? msg 'start!) start!)
      ((eq? msg 'stop!) stop!)
      ((eq? msg 'go-forward!) go-forward!)
      ((eq? msg 'go-backward!) go-backward!)                         
      (else
       "TRAIN: Incorrect message")))
  dispatch)

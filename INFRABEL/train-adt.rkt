;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Train ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (make-train-adt name track track-behind)
  (let ((riding? #f))
    (add-loco! name track track-behind)

    (define (start! initial-speed)
      (cond
        ((not riding?) (set-loco-speed! name initial-speed)
                       (set! riding? #t))
        (else
         "TRAIN: Train has already started riding")))
      
    (define (stop!)
      (cond
        (riding? (set-loco-speed! name initial-speed)
                 (set! riding #f))
        (else
         "TRAIN: Train has already stopped")))
        

    ;; Go-to-destination! to be added in phase2

    (define (dispatch msg)
      (cond
        ((eq? msg 'start!) start!)
        ((eq? msg 'stop!) stop!)
        (else
         "TRAIN: Incorrect message")))
    dispatch))
         
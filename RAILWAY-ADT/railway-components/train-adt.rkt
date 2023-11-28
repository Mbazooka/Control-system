;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Train ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-train-adt)

(define (make-train-adt name initial-track initial-track-behind)  
  (let ((speed 0))

    (define (get-name) name)

    (define (get-initial-track) initial-track)

    (define (get-initial-track-behind) initial-track-behind)

    (define (get-current-speed) speed)

    (define (change-speed! input-speed) ;; Made stupidproof to avoid high speeds
      (if (> (abs input-speed) 200) ;; If not, speed can go too high
          "TRAIN-ADT: Illegal speed"
            (set! speed input-speed)))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-initial-track) get-initial-track)
        ((eq? msg 'get-initial-track-behind) get-initial-track-behind)
        ((eq? msg 'get-current-speed) get-current-speed)
        ((eq? msg 'change-speed!) change-speed!)
        (else
         "TRAIN-ADT: Incorrect message")))
    dispatch))
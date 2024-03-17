;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 Train ADT                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-train-adt)

(define (make-train-adt name initial-track initial-track-behind)  
  (let ((speed 0)
        (trajectory '())
        (current-track initial-track)
        (track-behind initial-track-behind)
        (destination #f)
        )
    
    (define (get-name) name)

    (define (get-initial-track) initial-track)

    (define (get-initial-track-behind) initial-track-behind)

    (define (get-current-speed) speed)

    (define (change-speed! input-speed) ;; Made stupidproof to avoid high speeds
      (if (> (abs input-speed) 200) ;; If not, speed can go too high which allows for bugs and problems
          "TRAIN-ADT: Illegal speed"
          (set! speed input-speed)))

    (define (get-trajectory-state) trajectory)

    (define (change-trajectory-state! state)
      (if (list? state)
          (set! trajectory state)
          "Train-ADT: change-trajectory-state incorrect input"))

    (define (change-current-track! new-track)
      (set! track-behind current-track)
      (set! current-track new-track))
      
    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-initial-track) get-initial-track)
        ((eq? msg 'get-initial-track-behind) get-initial-track-behind)
        ((eq? msg 'get-current-speed) get-current-speed)
        ((eq? msg 'change-speed!) change-speed!)
        ((eq? msg 'get-trajectory-state) get-trajectory-state)
        ((eq? msg 'change-trajectory-state!) change-trajectory-state!) 
        ((eq? msg 'get-current-track) (lambda () current-track)) 
        ((eq? msg 'get-track-behind) (lambda () track-behind)) 
        ((eq? msg 'change-current-track!) change-current-track!) 
        ((eq? msg 'change-current-track-behind!) (lambda (input) (set! track-behind input))) 
        ((eq? msg 'get-destination) (lambda () destination)) 
        ((eq? msg 'change-destination!) (lambda (input-destination) (set! destination input-destination)))
        (else
         "TRAIN-ADT: Incorrect message")))
    dispatch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Railway ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require graph
         "./railway-components/barrier-adt.rkt"
         "./railway-components/detection-block-adt.rkt"
         "./railway-components/light-adt.rkt"
         "./railway-components/switch-adt.rkt"
         "./railway-components/track-adt.rkt"
         "./railway-components/train-adt.rkt"
         "hardware-setup.rkt")

(provide make-railway-adt)

(define (make-railway-adt)
  (let* ((HARDWARE-SETUP (make-hardware-setup-adt))
         (HARDWARE-SWITCHES (HARDWARE-SETUP 'get-possible-switches))
         (HARDWARE-LIGHTS (HARDWARE-SETUP 'possible-lights))
         (HARDWARE-TRACKS (HARDWARE-SETUP 'get-track-map))
         (railway-graph (unweighted-graph/undirected (HARDWARE-SETUP 'get-railway-connections)))
         (riding-trains (make-hash))) ;; No trains riding initially

;    (define (change-switch-state! switch-name state) ;; State?
;      (let ((switch-object (hash-ref HARDWARE-SWITCHES switch-name)))
;        (switch-object 


    (define (dispatch msg)
      (cond
        ((eq? 'hello) 'hello)
        (else
         "RAILWAY-ADT: Incorrect message")))
    dispatch))

    

    

    




  

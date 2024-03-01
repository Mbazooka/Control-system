;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Infrabel ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require 
  "../RAILWAY-ADT/railway-adt.rkt"
  "../simulator/interface.rkt")

(provide make-infrabel-adt)

(define (make-infrabel-adt)
  (let ((railway (make-railway-adt)))

    (setup-hardware) ;; Setup the track

    (start) ;; To start the simulator 

    ;; Adds a train to the hardware if the train-name has not been used
    (define (add-train-HARDWARE! train-name initial-track initial-track-behind)
      (cond
        (((railway 'add-train!) train-name initial-track initial-track-behind)
         (add-loco train-name initial-track-behind initial-track))))

    ;; Changing the train speed to a given speed
    (define (set-speed-train-HARDWARE! train-name speed)
      (cond
        (((railway 'change-train-speed!) train-name speed)
         (set-loco-speed! train-name speed))))

    ;; Changing the switch position to a given state
    (define (set-switch-position-HARDWARE! switch switch-position)
      (cond
        (((railway 'change-switch-state!) switch switch-position)
         (set-switch-position! switch switch-position))))

    ;; Changing the barriers their state
    (define (set-barrier-state-HARDWARE! barrier barrier-state)
      (cond
        (((railway 'change-barrier-state!) barrier barrier-state)
         (if barrier-state
             (open-crossing! barrier)
             (close-crossing! barrier)))))
    
    ;; Changing the light their state
    (define (set-light-state-HARDWARE! light light-state)
      (cond
        (((railway 'change-light-state!) light light-state)
         (set-sign-code! light light-state))))
    
    ;; Update the detection-blocks 
    (define (update-detection-blocks!)
      (let ((db-oc-ids (get-occupied-detection-blocks)) ;; Occupied detection-block ids
            (db-ids (get-detection-block-ids)))
        ((railway 'update-detection-blocks!) db-oc-ids db-ids)
        (cons db-oc-ids db-ids)))

    ;; The following is an abstraction of a reoccuring pattern for updating Hardware
    (define (update-abstraction message operation)
      (lambda (data)
        (for-each
         (lambda (pair)
           (let ((name (car pair))
                 (state (cdr pair)))
             ((railway message) name state)
             (operation name state)))
         data)))

    (define update-switches! (update-abstraction 'change-switch-state! set-switch-position-HARDWARE!))
    (define update-lights! (update-abstraction 'change-light-state! set-light-state-HARDWARE!))
    (define update-barriers! (update-abstraction 'change-barrier-state! set-barrier-state-HARDWARE!))

    ;; Abstraction for synchronous speed changing
    (define (change-speed! train-name speed)
      ((railway 'change-train-speed!)  train-name speed)
      (set-speed-train-HARDWARE! train-name speed))

    (define (update-trains! trains) ;; Updates the trains
      (for-each
       (lambda (train)
         (let ((train-name (car train))
               (init-track (cadr train))
               (beh-track (caddr train))
               (speed (cadddr train)))
           (add-train-HARDWARE! train-name init-track beh-track)
           (change-speed! train-name speed)))
       trains))            

    (define (dispatch msg)
      (cond
        ((eq? msg 'update-switches!) update-switches!)
        ((eq? msg 'update-lights!) update-lights!)
        ((eq? msg 'update-barriers!) update-barriers!)
        ((eq? msg 'update-trains!) update-trains!)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "INFRABEL-ADT: Incorrect message")))
    dispatch))
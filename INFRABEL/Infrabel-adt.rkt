;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Infrabel ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require 
  "../RAILWAY-ADT/railway-adt.rkt"
  "../simulator/interface.rkt")

(provide make-infrabel-adt)

(define (make-infrabel-adt)
  (let ((railway (make-railway-adt))
        (trains-trajectory (make-hash)))

    (setup-hardware) ;; Setup the track

    (start) ;; To start the simulator

    (define (retrieve-all-abstraction operation) ;; abstraction for reoccuring retrieve-all operation
      (lambda () ((railway operation))))

    (define retrieve-all-switches (retrieve-all-abstraction 'get-all-switches))

    (define retrieve-all-trains (retrieve-all-abstraction 'get-all-trains))

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
      (set-speed-train-HARDWARE! train-name speed))

    ;; Abstractions for synchronous switch changing
    (define (change-switch! switch state)
      (set-switch-position-HARDWARE! switch state))

    ;; Procedure for updating the trains
    (define (update-trains! trains) 
      (for-each
       (lambda (train)
         (let ((train-name (car train))
               (init-track (cadr train))
               (beh-track (caddr train))
               (speed (cadddr train))
               (current-track (car (cddddr train)))
               (current-track-behind (cadr (cddddr train))))
           (add-train-HARDWARE! train-name init-track beh-track)
           (change-speed! train-name speed)
           ((railway 'change-train-track!) train-name current-track)
           ((railway 'change-train-track-behind!) train-name current-track-behind)
           ))
       trains))

    ;; Abstractions
    (define first-traj car)
    (define rest-traj cdr)
    (define destination car)
    (define actual-traj cdr)

    ;; Helper procedures to process trajectory
    (define (switch? element)
      (eq? (string-ref (symbol->string element) 0) #\S))

    (define (get-switch-surrounding lst)
      (cons (car lst) (caddr lst)))

    (define get-switch cadr)

    (define (adjust-switch-traj switch switch-state comp-state supposed-state)
      (if (eq? comp-state supposed-state)
          '()
          (change-switch! switch (if (eq? switch-state 1) 2 1))))

    (define (get-destination trajectory)
      (if (null? trajectory)
          trajectory
          (if (null? (cdr trajectory))
              (car trajectory)
              (get-destination (cdr trajectory)))))

    ;; Abstractions 
    (define first-comp car)
    (define rest-comp cdr)
    (define one-ahead cdr)
    (define two-ahead cddr)

    ;; Procedure to process the trajectory and adjust elements
    (define (process-trajectory trajectory)
      (define (process-trajectory-iter current-component)
        (cond
          ((null? (one-ahead current-component)) '())
          ((null? (two-ahead current-component)) '())
          ((not (switch? (cadr current-component))) (process-trajectory-iter (rest-comp current-component))) 
          (else
           (let ((components (get-switch-surrounding current-component))
                 (possible-states ((railway 'get-switch-possible-comp-states) (get-switch current-component)))
                 (current-state ((railway 'get-switch-state) (get-switch current-component)))
                 (current-comp-state ((railway 'get-switch-comp-state) (get-switch current-component))))
             (cond
               ((member (first-comp components) possible-states)
                (adjust-switch-traj (get-switch current-component) current-state  current-comp-state (first-comp components))
                (process-trajectory-iter (rest-comp current-component)))
               ((member (rest-comp components) possible-states)
                (adjust-switch-traj (get-switch current-component) current-state  current-comp-state (rest-comp components))
                (process-trajectory-iter (rest-comp current-component))))))))
      (process-trajectory-iter trajectory))

    ;; Helper procedures
    (define (actual-trajectory? trajectory-data)
      (not (null? trajectory-data)))

    (define (flatten-trajectory data)
      (if (null? data)
          '()
          (append (car data) (flatten-trajectory (cdr data)))))

    ;; Procedure that determines the sign of the speed
    (define (determine-speed-sign train-name data)
      (display ((railway 'get-train-track) train-name)) (newline)
      (display ((railway 'get-train-track-behind) train-name)) (newline)
      (display data) (newline)
      (if (member ((railway 'get-train-track-behind) train-name) (flatten-trajectory data))
          -1
          1
      ))

    ;; Procedure for adding trajectories that need to be processed
    (define (add-trajectories! trajectories)
      (hash-for-each (make-hash trajectories)
                     (lambda (train-name data)
                       (if (actual-trajectory? data)
                           (begin
                             ((railway 'change-train-destination!) train-name (get-destination (first-traj data)))
                             (process-trajectory (first-traj data))
                             (display (determine-speed-sign train-name data)) (newline)
                             (change-speed! train-name (* (determine-speed-sign train-name data) 1)) ;; Determines direction implicitly
                             ((railway 'change-train-trajectory-state!) train-name (first-traj data))
                             (hash-set! trains-trajectory train-name (rest-traj data)))
                           (begin
                             ((railway 'change-train-destination!) train-name #f) ;; To ensure false if there is no trajectory
                             (hash-set! trains-trajectory train-name data)
                           )))))

    ;; Procedure to delay and make sure the train is fully on the detection-block
    (define (train-delay train)
      (let ((current-speed ((railway 'get-train-speed) train)))
        (change-speed! train 200)
        (sleep 0.70)
        (change-speed! train current-speed)))

    ;; Procedure that will update the trains their trajectories
    (define (update-trajectories!)
      (hash-for-each trains-trajectory
                     (lambda (train cc)
                       (cond
                         ((and (not (null? cc)) ;; Something left to do
                               ((railway 'get-detection-block-state) ((railway 'get-train-destination) train))) ;; Reached destination
                          (train-delay train)
                          ((railway 'change-train-destination!) (get-destination (first-traj cc)))
                          (process-trajectory (first-traj cc))
                          (change-speed! train (* -1 ((railway 'get-train-speed) train)))
                          (hash-set! trains-trajectory train (rest-traj cc)))
                         ((and ((railway 'get-train-destination) train)
                               ((railway 'get-detection-block-state) ((railway 'get-train-destination) train))
                               ((railway 'get-train-trajectory-state) train)) ;; Split or something
                          ((railway 'change-train-trajectory-state!) train #f)
                          ((railway 'change-train-destination!) train #f)
                          (train-delay train)
                          (change-speed! train 0))
                         ))
                     ))

    ;; Procedure that determines the possible next tracks
    (define (determine-possible-next-tracks current-track)
      ((railway 'get-track-neighbour) current-track))

    ;; Procedure that will update the train positions
    (define (update-train-positions) 
      (hash-for-each trains-trajectory
                     (lambda (train-name data)
                       (let* ((current-traj ((railway 'get-train-trajectory-state) train-name))
                              (current-traj-no-switch  '()))
                         (cond
                           ((and current-traj (not (null? current-traj)))
                            (set! current-traj-no-switch (filter (lambda (x)
                                                                   (not (switch? x))) current-traj))
                            (for-each
                             (lambda (DB)
                               (if ((railway 'get-detection-block-state) DB)
                                   ((railway 'change-train-track!) train-name DB)
                                   '()))
                             current-traj-no-switch))
                           (else
                            (let ((possible-tracks (determine-possible-next-tracks ((railway 'get-train-track) train-name))))
                              (for-each
                               (lambda (track)
                                 (if ((railway 'get-detection-block-state) track)
                                     ((railway 'change-train-track!) train-name track)
                                     '()))
                               possible-tracks))))
                         ))))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'update-switches!) update-switches!)
        ((eq? msg 'retrieve-all-switches) retrieve-all-switches) ;; ADDED
        ((eq? msg 'update-lights!) update-lights!)
        ((eq? msg 'update-barriers!) update-barriers!)
        ((eq? msg 'add-trajectories!) add-trajectories!) ;; ADDED
        ((eq? msg 'update-trajectories!) update-trajectories!) ;; ADDED
        ((eq? msg 'update-train-positions) update-train-positions) ;; ADDED
        ((eq? msg 'update-trains!) update-trains!)
        ((eq? msg 'retrieve-all-trains) retrieve-all-trains) ;; ADDED
        ((eq? msg 'update-train-positions) update-train-positions) ;; ADDED
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "INFRABEL-ADT: Incorrect message")))
    dispatch))
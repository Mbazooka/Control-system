;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 NMBS ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "./GUI/GUI-adt.rkt"
         "../RAILWAY-ADT/railway-adt.rkt")

(provide make-nmbs-adt)


(define (make-nmbs-adt)
  (let ((railway (make-railway-adt))
        (trains-trajectory (make-hash)) 
        (gui '()))

    (define (update-component-abstraction operation) ;; Abstraction for reoccuring update operation
      (lambda (component state)
        ((railway operation) component state)))

    (define (retrieve-all-abstraction operation) ;; abstraction for reoccuring retrieve-all operation
      (lambda () ((railway operation))))

    (define update-switch! (update-component-abstraction 'change-switch-state!))
    
    (define retrieve-all-switches (retrieve-all-abstraction 'get-all-switches))

    (define (update-barrier! barrier state) 
      (let ((processed-data (if (= state 0) 'open 'close)))
        ((railway 'change-barrier-state!) barrier processed-data)))

    (define retrieve-all-barriers (retrieve-all-abstraction 'get-all-barriers))

    (define update-light! (update-component-abstraction 'change-light-state!))

    (define retrieve-all-lights (retrieve-all-abstraction 'get-all-lights))

    (define retrieve-all-detection-blocks (retrieve-all-abstraction 'get-all-detection-blocks))

    (define update-train! (update-component-abstraction 'change-train-speed!))

    (define retrieve-all-trains (retrieve-all-abstraction 'get-all-trains))

    ;; Helper procedure to determine the destination a trajectory
    (define (get-destination trajectory)
      (if (null? trajectory)
          trajectory
          (if (null? (cdr trajectory))
              (car trajectory)
              (get-destination (cdr trajectory)))))

    ;; Helper procedures to adjust the trajectory taken to a correct trajectory
    (define (environment-component? preceded component followed component-list)
      (and (eq? (car component-list) preceded)
           (eq? (cadr component-list) component)
           (eq? (caddr component-list) followed)))

    (define (adjust-single-trajectory component-list)
      (define (adjustment-traj lst . new-elements)
        (cons (car lst) (append new-elements (cddr lst))))
        
      (define (adjust-single-trajec-rec lst)
        (cond
          ((null? lst) '())
          ((null? (cdr lst)) lst)
          ((null? (cddr lst)) lst)
          ((or (environment-component? 'S-2 'S-7 'S-25 lst)
               (environment-component? 'S-25 'S-7 'S-2 lst))
           (adjustment-traj lst 'S-1))
          ((or (environment-component? 'S-1 'S-25 'S-7 lst)
               (environment-component? 'S-7 'S-25 'S-1 lst))
           (adjustment-traj lst 'S-2))
          (else
           (cons (car lst)
                 (adjust-single-trajec-rec (cdr lst))))))
      (adjust-single-trajec-rec component-list))

    (define (adjust-full-trajectory trajectory)
      (map adjust-single-trajectory trajectory))

    ;; Helper procedure for optimization procedure
    (define (common-elements? lst1 lst2)
      (define member-boolean #f)
      (for-each (lambda (element)
                  (if (not (eq? (member element lst2) #f))
                      (set! member-boolean #t)
                      '()))
                lst1)
      member-boolean)

    ;; Procedure to optimize the path so that it doesn't stop and then continue riding in the same direction (might have duplicates, but that's not a problem)
    (define (optimize-trajectory full-trajectory) ;; Determines if the trajectory can be optimized, i.e. completed in one chunk
      (define (optimize-trajectory-iter current-traj rest-traj acc)
        (if (null? rest-traj)
            (append acc (list current-traj))
            (if (common-elements? current-traj (cdar rest-traj))
                (optimize-trajectory-iter (car rest-traj) (cdr rest-traj) (append acc (list current-traj)))
                (optimize-trajectory-iter (append current-traj (car rest-traj)) (cdr rest-traj) acc))))
      (optimize-trajectory-iter (car full-trajectory) (cdr full-trajectory) '()))     

    ;; Compute the trajectory that must be taken to get to the desired destination
    (define (compute-trajectory start destination) ;; ADDED (ADD TO TESTS)
      (let ((path ((railway 'compute-path-simplified) start destination)))
        (define (compute-individual-trajectory start current-path)
          (if (null? current-path)
              '()
              (let ((individual-path ((railway 'compute-path-complex) start (car current-path))))
                (cons individual-path
                      (compute-individual-trajectory (get-destination individual-path) (cdr current-path))))))
        (optimize-trajectory (adjust-full-trajectory (compute-individual-trajectory start path)))))

    ;; Helper procedures to process trajectory
    (define (switch? element)
      (eq? (string-ref (symbol->string element) 0) #\S))

    (define (get-switch-surrounding lst)
      (cons (car lst) (caddr lst)))

    (define get-switch cadr)

    (define (adjust-switch-traj switch switch-state comp-state supposed-state)
      (if (eq? comp-state supposed-state)
          '()
          ((railway 'change-switch-state!) switch (if (eq? switch-state 1) 2 1))))

    ;; Procedure to process the trajectory and adjust elements
    (define (process-trajectory trajectory)
      (define (process-trajectory-iter current-component)
        (display current-component)
        (newline)
        (cond
          ((null? (cdr current-component)) '())
          ((null? (cddr current-component)) '())
          ((not (switch? (cadr current-component))) (newline) (process-trajectory-iter (cdr current-component))) 
          (else
           (let ((components (get-switch-surrounding current-component))
                 (possible-states ((railway 'get-switch-possible-comp-states) (get-switch current-component)))
                 (current-state ((railway 'get-switch-state) (get-switch current-component)))
                 (current-comp-state ((railway 'get-switch-comp-state) (get-switch current-component))))
             (cond
               ((member (car components) possible-states)
                (adjust-switch-traj (get-switch current-component) current-state  current-comp-state (car components))
                (process-trajectory-iter (cdr current-component)))
               ((member (cdr components) possible-states)
                (adjust-switch-traj (get-switch current-component) current-state  current-comp-state (cdr components))
                (process-trajectory-iter (cdr current-component))))))))
      (process-trajectory-iter trajectory))

    (define (apply-train! train-name track track-behind) ;; Adds a train to the track
      (if ((railway 'add-train!) train-name track track-behind)
          (hash-set! trains-trajectory train-name (cons track '()))
          '()))

    ;; Abstractions
    (define first-traj car)
    (define rest-traj cdr)
    (define destination car)
    (define actual-traj cdr)
    
    ;; Procedure that will add a trajectory for a specific train (to be changed later (current destination), ADDED)
    (define (add-trajectory! train-name destination)
      (define trajectory (compute-trajectory (car (hash-ref trains-trajectory train-name)) destination))
      (hash-set! trains-trajectory train-name (cons (get-destination (first-traj trajectory)) (rest-traj trajectory)))
      (process-trajectory (first-traj trajectory))
      ((railway 'change-train-speed!) train-name 200)) ;; TO BE CHANGED

    ;; Procedure that will update the trains their trajectories
    (define (update-trajectories!)
      (hash-for-each trains-trajectory
                     (lambda (train cc)
                       (cond
                         ((and (not (null? (actual-traj cc))) ;; Something left to do
                                ((railway 'get-detection-block-state) (destination cc))) ;; Reached destination
                             (process-trajectory (first-traj (actual-traj cc)))
                             ((railway 'change-train-speed!) train (* -1 ((railway 'get-train-speed) train)))
                             (hash-set! trains-trajectory train
                                        (cons (get-destination (first-traj (actual-traj cc)))
                                        (rest-traj (actual-traj cc)))))
                         (((railway 'get-detection-block-state) (destination cc)) ;; TO BE CHANGED WITH SPECIFIC TRAIN
                          ((railway 'change-train-speed!) train 0))))
                     ))

    (define update-detection-blocks! ;; Updates the detection-blocks their states
      (lambda (data-pair)
        (let ((oc-db (car data-pair))
              (all-db (cdr data-pair)))
          ((railway 'update-detection-blocks!) oc-db all-db))))

    (set! gui (make-gui-adt update-switch! retrieve-all-switches
                            update-barrier! retrieve-all-barriers
                            update-light! retrieve-all-lights
                            retrieve-all-detection-blocks update-train!
                            apply-train! retrieve-all-trains add-trajectory!))
      
    (define (dispatch msg)
      (cond
        ((eq? msg 'retrieve-all-switches) retrieve-all-switches)
        ((eq? msg 'apply-train!) apply-train!)
        ((eq? msg 'retrieve-all-barriers) retrieve-all-barriers)
        ((eq? msg 'retrieve-all-lights) retrieve-all-lights)
        ((eq? msg 'retrieve-all-trains) retrieve-all-trains)
        ((eq? msg 'compute-trajectory) compute-trajectory)
        ((eq? msg 'add-trajectory!) add-trajectory!) ;; ADDED
        ((eq? msg 'update-trajectories!) update-trajectories!) ;; ADDED
        ((eq? msg 'process-trajectory) process-trajectory)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "NMBS-ADT: Illegal message")))
    dispatch))
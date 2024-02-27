;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 NMBS ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "./GUI/GUI-adt.rkt"
         "../RAILWAY-ADT/railway-adt.rkt")

(provide make-nmbs-adt)


(define (make-nmbs-adt)
  (let ((railway (make-railway-adt))
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
          ((or (environment-component? 'T-2 'T-7 'T-25 lst)
               (environment-component? 'T-25 'T-7 'T-2 lst))
           (adjustment-traj lst 'T-1))
          ((or (environment-component? 'T-1 'T-25 'T-7 lst)
               (environment-component? 'T-7 'T-25 'T-1 lst))
           (adjustment-traj lst 'T-2))
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

    (define (apply-train! train-name track track-behind) ;; Adds a train to the track
      ((railway 'add-train!) train-name track track-behind))     

    (define update-detection-blocks! ;; Updates the detection-blocks their states
      (lambda (data-pair)
        (let ((oc-db (car data-pair))
              (all-db (cdr data-pair)))
          ((railway 'update-detection-blocks!) oc-db all-db))))

    (set! gui (make-gui-adt update-switch! retrieve-all-switches
                            update-barrier! retrieve-all-barriers
                            update-light! retrieve-all-lights
                            retrieve-all-detection-blocks update-train!
                            apply-train! retrieve-all-trains))
      
    (define (dispatch msg)
      (cond
        ((eq? msg 'retrieve-all-switches) retrieve-all-switches)
        ((eq? msg 'apply-train!) apply-train!)
        ((eq? msg 'retrieve-all-barriers) retrieve-all-barriers)
        ((eq? msg 'retrieve-all-lights) retrieve-all-lights)
        ((eq? msg 'retrieve-all-trains) retrieve-all-trains)
        ((eq? msg 'compute-trajectory) compute-trajectory)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        (else
         "NMBS-ADT: Illegal message")))
    dispatch))
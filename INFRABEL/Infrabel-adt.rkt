;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Infrabel ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require 
  "../RAILWAY-ADT/railway-adt.rkt"
  "./HardSim/HardSimAbstractie.rkt")

(provide make-infrabel-adt)

;; Abstraction to make the code more readable
(define SIM-selected 0)
(define HARD-selected 1)
(define HARDSIM-selection HARD-selected)

(define HARD-speed 30)
(define SIM-speed 200)

(define (determine-speed sign)
  (cond
    ((eq? HARDSIM-selection HARD-selected) (if sign HARD-speed (- HARD-speed)))
    ((eq? HARDSIM-selection SIM-selected) (if sign SIM-speed (- SIM-speed)))))

(define train-input-treshold 20)

(define (make-infrabel-adt)
  (let ((railway (make-railway-adt))
        (trains-trajectory (make-hash))
        (train-full-traj (make-hash))
        (train-previous-speed (make-hash))
        (train-manual-movement (make-hash)))

    (setup-hardware HARDSIM-selection) ;; Setup the track

    (start HARDSIM-selection) ;; To start the simulator

    ;; Helper procedure
    (define (flatten-trajectory data)
      (if (null? data)
          '()
          (append (car data) (flatten-trajectory (cdr data)))))

    (define (switch? element)
      (eq? (string-ref (symbol->string element) 0) #\S))

    (define (retrieve-all-abstraction operation) ;; abstraction for reoccuring retrieve-all operation
      (lambda () ((railway operation))))

    (define retrieve-all-switches (retrieve-all-abstraction 'get-all-switches))

    (define retrieve-all-trains (retrieve-all-abstraction 'get-all-trains))

    (define retrieve-DB-reservations (retrieve-all-abstraction 'get-all-detection-block-reservation-states))

    ;; Adds a train to the hardware if the train-name has not been used
    (define (add-train-HARDWARE! train-name initial-track initial-track-behind)
      (cond
        (((railway 'add-train!) train-name initial-track initial-track-behind)
         (add-loco HARDSIM-selection train-name initial-track-behind initial-track)
         ((railway 'detection-block-reserve!) initial-track train-name)
         (hash-set! train-previous-speed train-name 0)
         )))

    ;; Changing the train speed to a given speed
    (define (set-speed-train-HARDWARE! train-name speed)
      (cond
        (((railway 'change-train-speed!) train-name speed)
         (set-loco-speed! HARDSIM-selection train-name speed))))

    ;; Changing the switch position to a given state
    (define (set-switch-position-HARDWARE! switch switch-position)
      (cond
        (((railway 'change-switch-state!) switch switch-position)
         (set-switch-position! HARDSIM-selection switch switch-position))))

    ;; Changing the barriers their state
    (define (set-barrier-state-HARDWARE! barrier barrier-state)
      (cond
        (((railway 'change-barrier-state!) barrier barrier-state)
         (if barrier-state
             (open-crossing! HARDSIM-selection barrier)
             (close-crossing! HARDSIM-selection barrier)))))
    
    ;; Changing the light their state
    (define (set-light-state-HARDWARE! light light-state)
      (cond
        (((railway 'change-light-state!) light light-state)
         (set-sign-code! HARDSIM-selection light light-state))))
    
    ;; Update the detection-blocks 
    (define (update-detection-blocks!)
      (let ((db-oc-ids (get-occupied-detection-blocks HARDSIM-selection)) ;; Occupied detection-block ids
            (db-ids (get-detection-block-ids HARDSIM-selection)))
        ((railway 'update-detection-blocks!) db-oc-ids db-ids (retrieve-DB-reservations))
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

    ;; Free unused neighbouring tracks their reservation
    (define (free-reservation-manual-movement! train-name)
      (let* ((track-behind ((railway 'get-train-track-behind) train-name))
             (current-track ((railway 'get-train-track) train-name))
             (prev-neighbour ((railway 'get-track-neighbour) track-behind)))
        (for-each
         (lambda (track)
           ((railway 'detection-block-reserve!) track #f))
         (filter (lambda (db) (not (eq? db current-track))) prev-neighbour))
        ((railway 'detection-block-reserve!) track-behind #f)        
        ))

    ;; Procedure that will free the reservations it has made
    (define (free-reservation-manual-movement-failed! train-name)
      (let* ((current-track ((railway 'get-train-track) train-name))
             (neighbours ((railway 'get-track-neighbour) current-track)))
        (for-each
         (lambda (track)
           (if (eq? ((railway 'get-detection-block-reservation) track) train-name) ;; Leave reservations not of your own alone
               ((railway 'detection-block-reserve!) track #f)
               '()))
         (filter (lambda (db) (not (eq? db current-track))) neighbours))))

    ;; Determine if train can go to the next detection-block (by reserving all surrounding blocks)
    (define (allow-train-manual-movement? train-name)
      (let* ((train-track ((railway 'get-train-track) train-name))
             (neighbouring-track ((railway 'get-track-neighbour) train-track)))
        (for-each
         (lambda (track)
           (if ((railway 'get-detection-block-reservation) track)
               '()
               ((railway 'detection-block-reserve!) track train-name)))
         neighbouring-track)
        (define movement-allowed? #t)
        (for-each
         (lambda (track)
           (if (eq? ((railway 'get-detection-block-reservation) track) train-name)
               '()
               (set! movement-allowed? #f)))
         neighbouring-track)
        (if movement-allowed?
            '()
            (free-reservation-manual-movement-failed! train-name)) ;; In case not succesful, release reserve tracks
        movement-allowed?
        ))

    ;; Procedure for updating the trains
    (define (update-trains! trains) 
      (for-each
       (lambda (train)
         (let ((train-name (car train))
               (init-track (cadr train))
               (beh-track (caddr train))
               (speed (cadddr train))
               (current-track (car (cddddr train)))
               (current-track-behind (cadr (cddddr train)))
               (traj-state (caddr (cddddr train))))
           (add-train-HARDWARE! train-name init-track beh-track)           
           ((railway 'change-train-track!) train-name current-track)
           ((railway 'change-train-track-behind!) train-name current-track-behind)
           ((railway 'change-train-trajectory-state!) train-name traj-state)
           ;(change-speed! train-name speed)
           (if (and (hash-has-key? train-full-traj train-name)
                    (not (null? (hash-ref train-full-traj train-name)))) ;; Undergoing trajectory?
               '() ;; Cannot change speed (to ensure safety)
                 (if (and (hash-has-key? train-manual-movement train-name) ;; Maybe undergoing manual movement
                          (hash-ref train-manual-movement train-name))
                     '()
                     (if (and (> (abs speed) 5) (allow-train-manual-movement? train-name)) ;; Only when allowed can the train ride if there is attempt to make it ride
                         (begin
                           (change-speed! train-name speed)
                           (hash-set! train-manual-movement train-name #t))                         
                         '())))
           ))
       trains))

    ;; Abstractions
    (define first-traj car)
    (define rest-traj cdr)
    (define destination car)
    (define actual-traj cdr)

    ;; Helper procedures to process trajectory
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

    (define (inversion-track? track)
      (or (eq? track '1-8) (eq? track '2-1)
          (eq? track '2-7) (eq? track '2-6)
          (eq? track '2-5) (eq? track '2-2)))

    (define (special-track? track track-behind) ;; Due to representation a small change must be made here
      (or (and (eq? track '2-3) (or (eq? track-behind '1-1) (eq? track-behind '1-2) (eq? track-behind '1-3)))
          (and (eq? track '2-4) (or (eq? track-behind '1-1) (eq? track-behind '1-2) (eq? track-behind '1-3)))))

    (define (track-behind? track track-1 track-2)
      (or (eq? track track-1) (eq? track track-2)))

    (define (inversion-track-behind? track)
      (or (eq? track '1-8) (eq? track '2-1) (eq? track '2-7)
          (eq? track '2-6) (eq? track '2-5) (eq? track '2-1)
          (eq? track '2-2)))

    (define (determine-sign track-1 track-2 data) ;; Determine the individual sign
      (if (or (member track-1 data)
              (member track-2 data))
          -1
          1))

    (define (determine-sign-after-direction train-name track-behind traj)
      (if (member track-behind traj)
          (hash-ref train-previous-speed train-name)
          '()))

    (define (opposite-speed-sign train-name)
      (if (negative? (hash-ref train-previous-speed train-name)) 1 -1))

    (define (identical-speed-sign train-name)
      (if (negative? (hash-ref train-previous-speed train-name)) -1 1))

    (define (speed-untouched? train-name)
      (= (hash-ref train-previous-speed train-name) 0))

    (define (determine-sign-after-manipulation train-name traj track-1 track-2)
      (if (or (member track-1 traj) (member track-2 traj))
          (opposite-speed-sign train-name)
          (identical-speed-sign train-name)))
          
    ;; WELCOME TO THE UGLIEST PART OF THE CODE. FEEL FREE TO CLOSE YOUR EYES.
    ;; THIS WILL BE FIXED FIRST THING IN PHASE 3. PLEASE NOTE THAT
    ;; THIS WAS EXPERIMENTAL CODE TO CONSIDER ALL CASES AND TO MAKE SURE
    ;; THE SPEED DETERMINATION IS ALWAYS CORRECT. HOWEVER THIS STILL NEEDS TO BE CLEANED UP
    ;; I HAVE NO INTENTION OF LEAVING IT LIKE THIS BUT HAD NO CHOICE
    ;; BUT I KEPT IT LIKE THAT BECAUSE I FIGURED IT OUT LAST MINUTE :D

    ;; Procedure that determines the sign of the speed
    (define (determine-speed-sign train-name data)
      (let ((track ((railway 'get-train-track) train-name))
            (track-behind ((railway 'get-train-track-behind) train-name))
            (traj (flatten-trajectory data)))
        (cond
          ((inversion-track? ((railway 'get-train-track) train-name))
           (if (not (inversion-track? track-behind))
               (if (eq? (hash-ref train-previous-speed train-name) 0) 1 (opposite-speed-sign train-name))
               (opposite-speed-sign train-name)))
          ((special-track? ((railway 'get-train-track) train-name) track-behind)
           (if (eq? (hash-ref train-previous-speed train-name) 0) ;; Ever started?
               (begin
                 (if (or (member '1-1 traj) (member '1-2 traj) (member '1-3 traj)) ;; Never started
                     1
                     -1))
               (if (or (member '1-1 traj) (member '1-2 traj) (member '1-3 traj))
                   (opposite-speed-sign train-name)
                   (identical-speed-sign train-name)                      
                   )))
          ((inversion-track-behind? track-behind)
           (cond
             ((or (member '1-1 traj) (member '1-2 traj) (member '1-3 traj))
              (identical-speed-sign train-name))
             (else
              (opposite-speed-sign train-name))))         
          ((track-behind? track-behind '2-3 '2-4)
           (if (speed-untouched? train-name)
               (determine-sign '2-3 '2-4 traj)
               (determine-sign-after-manipulation train-name traj '2-3 '2-4)))
          ((track-behind? track-behind '1-6 '1-5)
           (let ((train-track ((railway 'get-train-track) train-name)))
             (if (or (eq? train-track '2-3) (eq? train-track '2-4))
                 (if (inversion-track? (get-destination traj))
                     (if (speed-untouched? train-name) -1 (opposite-speed-sign train-name))
                     (if (or (member '1-5 traj) (member '1-6 traj))
                         (opposite-speed-sign train-name)
                         (identical-speed-sign train-name)))
                 (if (speed-untouched? train-name)
                     (determine-sign '1-5 '1-6 traj)
                     (determine-sign-after-manipulation train-name traj '1-5 '1-6)))))
          ((track-behind? track-behind '1-4 '1-7)
           (if (speed-untouched? train-name)
               (if (or (eq? track '1-1) (eq? track '1-2) (eq? track '1-3))
                   (if (or (member '1-4 traj) (member '1-7 traj)) 1 -1)
                   (determine-sign '1-4 '1-7 traj))
               (determine-sign-after-manipulation train-name traj '1-4 '1-7)))
          ((or (track-behind? track-behind '1-1 '1-2) (track-behind? track-behind '1-2 '1-3))
           (if (speed-untouched? train-name)
               (if (or (member '1-1 traj) (member '1-2 traj) (member '1-3 traj))
                   -1
                   1)
               (if (or (member '1-1 traj) (member '1-2 traj) (member '1-3 traj))
                   (opposite-speed-sign train-name)
                   (identical-speed-sign train-name)))))          

        ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Procedure that determines if it's a detection block or not
    (define (detection-block? comp)
      (not (switch? comp)))

    ;; Procedure that accumalates everything
    (define (accumulate operator null-val list)
      (if (null? list)
          null-val
          (operator (car list) (accumulate operator null-val (cdr list)))))

    ;; Procedure that determines whether a train has reserved all components
    (define (reserved-everything? train-name components-necessary)
      (define truth #t)
      (for-each
       (lambda (comp)
         (if (eq? ((railway 'get-detection-block-reservation) comp) train-name)
             '()
             (set! truth #f))
         )
       components-necessary)
      truth)

    ;; Procedure that checks if switches necessary are already in use by another train's trajectory
    (define (switch-readiness? train-name full-traj)
      (let ((switches (filter switch? full-traj)))
        (define readiness #t)
        (hash-for-each
         train-full-traj
         (lambda (current-train-name traj)
           (for-each
            (lambda (switch)
              (if (eq? current-train-name train-name) '() (if (member switch traj) (set! readiness #f) '())))
            switches)))
        readiness))
       
    ;; Procedure that attempts to reserve
    (define (attempt-reservation train-name trajectories)
      (let* ((all-components-nec (flatten trajectories))
             (detection-blocks (filter detection-block? all-components-nec)))
        (for-each
         (lambda (comp)
           (if ((railway 'get-detection-block-reservation) comp)
               '()
               ((railway 'detection-block-reserve!) comp train-name)))
         detection-blocks)
        (let ((bool (and (reserved-everything? train-name detection-blocks)
                         (switch-readiness? train-name all-components-nec))
                    ))
          (if bool
              (hash-set! train-full-traj train-name all-components-nec)
              (free-reservation-after-failure! train-name detection-blocks))
          bool)
        ))

    ;; Procedure that will free reservations when failed
    (define (free-reservation-after-failure! train-name detection-blocks)
      (let* ((train-bool (hash-ref trains-trajectory train-name ))
             (current-db (if train-bool ((railway 'get-train-track) train-name) '9-9))) ;; Dummy DB
        (for-each
         (lambda (track)
           (if (eq? ((railway 'get-detection-block-reservation) track) train-name) ;; Leave reservations not of your own alone
               ((railway 'detection-block-reserve!) track #f)
               '()))
         (filter (lambda (comp) (not (eq? current-db comp))) detection-blocks))))

    trains-trajectory

    ;; Procedure that will free the previous reservations
    (define (free-reservation! train-name) ;; KEEP TRAJECTORY BEFORE AND THEN RELEASE IT EXCEPT LAST ELEMENT
      (let* ((last-db (get-destination (hash-ref train-full-traj train-name)))
             (detection-blocks (filter (lambda (comp) (not (switch? comp))) (hash-ref train-full-traj train-name))))
        (for-each
         (lambda (db)
           ((railway 'detection-block-reserve!) db #f))
         (filter (lambda (db) (not (eq? last-db db))) detection-blocks))
        (hash-set! train-full-traj train-name '())
        ((railway 'detection-block-reserve!) last-db train-name)
        ))

    ;; Procedure for adding trajectories that need to be processed
    (define (add-trajectories! trajectories)
      (hash-for-each (make-hash trajectories)
                     (lambda (train-name data)
                       (hash-set! trains-trajectory train-name data))))

    ;; Procedure to delay and make sure the train is fully on the detection-block
    (define (train-delay train)
      (let ((current-speed ((railway 'get-train-speed) train)))
        (change-speed! train (if (negative? current-speed) (determine-speed #f)  (determine-speed #t)))
        (sleep 0.9)
        (change-speed! train current-speed)))

    ;; Procedure that will update the trains their trajectories
    (define (update-trajectories!)
      (hash-for-each trains-trajectory
                     (lambda (train cc)
                       (cond
                         ((and (eq? ((railway 'get-train-destination) train) #f) ;; Standing idle with no destination
                               (not (null? cc)) ;; But it has a trajectory computed, then you have to start it
                               (attempt-reservation train cc);; Only if it's allowed
                               )
                          ((railway 'change-train-destination!) train (get-destination (first-traj cc)))
                          (process-trajectory (first-traj cc))
                          (change-speed! train (* (determine-speed-sign train cc) (determine-speed #t))) ;; Determines direction implicitly
                          ((railway 'change-train-trajectory-state!) train (first-traj cc))
                          (hash-set! trains-trajectory train (rest-traj cc)))                              
                         ((and (not (null? cc)) ;; Something left to do
                               ((railway 'get-train-destination) train) ;; Already in process
                               ((railway 'get-detection-block-state) ((railway 'get-train-destination) train))) ;; Reached destination
                          (if (eq? HARDSIM-selection HARD-selected) '() (train-delay train))
                          ((railway 'change-train-destination!) train (get-destination (first-traj cc)))
                          (process-trajectory (first-traj cc))
                          (change-speed! train (* -1 ((railway 'get-train-speed) train)))
                          (hash-set! trains-trajectory train (rest-traj cc)))
                         ((and ((railway 'get-train-destination) train)
                               ((railway 'get-detection-block-state) ((railway 'get-train-destination) train))
                               ((railway 'get-train-trajectory-state) train))
                          ((railway 'change-train-trajectory-state!) train '())
                          ((railway 'change-train-destination!) train #f)
                          (if (eq? HARDSIM-selection HARD-selected) '() (train-delay train))
                          (change-speed! train 0)
                          ;(hash-set! train-previous-speed train ((railway 'get-train-speed) train))
                          (free-reservation! train) ;; Free everything when arrived
                          )
                         ))
                     ))

    ;; Procedure that determines the possible next tracks
    (define (determine-possible-next-tracks current-track)
      ((railway 'get-track-neighbour) current-track))

    ;; Train delay to fully reach detection-block and then end it
    (define (train-special-delay train)
      (let ((current-speed ((railway 'get-train-speed) train)))
        (change-speed! train (if (negative? current-speed) (determine-speed #f) (determine-speed #t)))
        (sleep 1)
        (change-speed! train 0)))

    ;; Procedure that will update the train positions
    (define (update-train-positions!) 
      (hash-for-each trains-trajectory
                     (lambda (train-name data)
                       (cond
                         ((and (hash-has-key? train-full-traj train-name)
                               (hash-ref train-full-traj train-name))
                          (let ((current-traj-no-switch (filter (lambda (comp) (not (switch? comp))) (hash-ref train-full-traj train-name))))
                            (for-each
                             (lambda (DB)
                               (if ((railway 'get-detection-block-state) DB)
                                   ((railway 'change-train-track!) train-name DB)
                                   '()))
                             current-traj-no-switch)))                          
                           
                         ((and (hash-has-key? train-manual-movement train-name)
                               (hash-ref train-manual-movement train-name))                            
                          (let* ((current-track ((railway 'get-train-track) train-name))
                                 (used-tracks (map (lambda (train)
                                                     ((railway 'get-train-track) train))
                                                   (hash-keys trains-trajectory)))
                                 (possible-tracks (filter (lambda (track)
                                                            (not (member track used-tracks)))
                                                          (determine-possible-next-tracks current-track))))
                            (for-each
                             (lambda (track)
                               (if (and
                                    (not ((railway 'get-detection-block-state) current-track))
                                    ((railway 'get-detection-block-state) track))
                                   (begin
                                     (if (> (- (abs ((railway 'get-train-speed) train-name)) train-input-treshold) 0) (hash-set! train-previous-speed train-name ((railway 'get-train-speed) train-name)) '())
                                     ((railway 'change-train-track!) train-name track)
                                     (begin
                                       (hash-set! train-manual-movement train-name #f)
                                       (if (eq? HARDSIM-selection HARD-selected) '() (train-special-delay train-name))
                                       (free-reservation-manual-movement! train-name)
                                       )
                                     )
                                   '()))
                             possible-tracks)))
                         ))))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'update-switches!) update-switches!)
        ((eq? msg 'retrieve-all-switches) retrieve-all-switches)
        ((eq? msg 'update-lights!) update-lights!)
        ((eq? msg 'update-barriers!) update-barriers!)
        ((eq? msg 'add-trajectories!) add-trajectories!) 
        ((eq? msg 'update-trajectories!) update-trajectories!) 
        ((eq? msg 'update-train-positions!) update-train-positions!) 
        ((eq? msg 'update-trains!) update-trains!)
        ((eq? msg 'retrieve-all-trains) retrieve-all-trains)
        ((eq? msg 'update-detection-blocks!) update-detection-blocks!)
        ((eq? msg 'retrieve-DB-reservations) retrieve-DB-reservations) 
        (else
         "INFRABEL-ADT: Incorrect message")))
    dispatch))
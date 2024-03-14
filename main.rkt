;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             TCP CONNECTION                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Currently this is not a TCP connection
;; this will have to be replaced in the NMBS ADT or somewhere else
;; Depending on how it works

#lang racket

(require (prefix-in nmbs: "./NMBS/NMBS-adt.rkt")
         (prefix-in infrabel: "./INFRABEL/Infrabel-adt.rkt"))

(define NMBS (nmbs:make-nmbs-adt))
(define INFRABEL (infrabel:make-infrabel-adt))

(define (main-loop)
  ((INFRABEL 'update-switches!) ((NMBS 'retrieve-all-switches)))
  ((INFRABEL 'update-lights!) ((NMBS 'retrieve-all-lights)))
  ((INFRABEL 'update-barriers!) ((NMBS 'retrieve-all-barriers)))
  ((INFRABEL 'update-trains!) ((NMBS 'retrieve-all-trains)))
  ;(display (NMBS 'bla)) (newline)
  ((NMBS 'update-detection-blocks!) ((INFRABEL 'update-detection-blocks!)))
  ((INFRABEL 'add-trajectories!) ((NMBS 'retrieve-trajectories)))
  (sleep 0.2)
  ((INFRABEL 'update-train-positions))
  ((INFRABEL 'update-trajectories!))
  ((NMBS 'update-trains!) ((INFRABEL 'retrieve-all-trains)))
  ((NMBS 'update-switches!) ((INFRABEL 'retrieve-all-switches)))
  (main-loop))

(thread main-loop)
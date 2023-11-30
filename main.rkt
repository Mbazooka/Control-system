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
  ((INFRABEL 'update-lights!) ((NMBS 'update-lights!)))
  ((INFRABEL 'update-barriers!) ((NMBS 'update-barriers!)))
  ((INFRABEL 'update-trains!) ((NMBS 'update-trains!)))
  ((NMBS 'update-detection-blocks!) ((INFRABEL 'update-detection-blocks!)))
  (sleep 0.5)
  (main-loop))

(thread main-loop)
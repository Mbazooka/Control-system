#lang racket

(require (prefix-in nmbs: "./NMBS/NMBS-adt.rkt"))

;; Message to confirm connection
(define-values (in out) (tcp-connect "192.168.129.101" 29487))
(define end-connection #f)
(write "------Connection to client succesful------" out)
(flush-output out)

(define NMBS (nmbs:make-nmbs-adt))

(define (close-client)
  (close-input-port in)
  (close-output-port out))

(define (NMBS-output-updated-railway-components)
  (write ((NMBS 'retrieve-all-switches)) out)
  (flush-output out)
  (sleep 0.01)
  (write ((NMBS 'retrieve-all-lights)) out)
  (flush-output out)
  (sleep 0.01)
  (write ((NMBS 'retrieve-all-barriers)) out)
  (flush-output out)
  (sleep 0.01))

(define (NMBS-output-updated-trains)
  (write ((NMBS 'retrieve-all-trains)) out)
  (flush-output out)
  (sleep 0.01))

(define (NMBS-output-updated-trajectories)
  (write ((NMBS 'retrieve-trajectories)) out)
  (flush-output out)
  (sleep 0.1))

(define (NMBS-update-detection-blocks!)
  (define updated-detection-blocks (read in))
  (define reservations (read in))
  ((NMBS 'update-detection-blocks!) updated-detection-blocks reservations))

(define (NMBS-update-components!)
  ((NMBS 'update-trains!) (read in))
  ((NMBS 'update-switches!) (read in)))

;; The main loop that updates everything as necessary 
(define (NMBS-main-loop)
  (NMBS-output-updated-railway-components)
  (NMBS-output-updated-trains)
  (NMBS-update-detection-blocks!) ;; Works till here
  (NMBS-output-updated-trajectories)
  (NMBS-output-updated-trains)
  (NMBS-update-components!)
  (NMBS-output-updated-trains)
  (sleep 0.5)
  (if end-connection (close-client) (NMBS-main-loop)))

(thread NMBS-main-loop)
#lang racket

(require (prefix-in infrabel: "./INFRABEL/Infrabel-adt.rkt"))

(define SIM-selected 0)
(define HARD-selected 1)

(define the-listener (tcp-listen 29487 2 #t))
(define-values (in out) (tcp-accept the-listener))
(define end-connection #f)
(displayln (read in))

;; Processes whether it is hardware or simulator
(define (hard-sim arg)
  (let ((CL-arg (vector-ref arg 0)))
    (cond
      ((string=? CL-arg "hardware") HARD-selected)
      ((string=? CL-arg "simulator") SIM-selected)
      (else
       (displayln "Incorrect selection -- end everything")
       (exit)))))

(define INFRABEL (infrabel:make-infrabel-adt (hard-sim (read in))))

(define (close-server)
  (tcp-close the-listener)
  (close-input-port in)
  (close-output-port out))

(define (INFRABEL-update-components!)
  ((INFRABEL 'update-switches!) (read in))
  ((INFRABEL 'update-lights!) (read in))
  ((INFRABEL 'update-barriers!) (read in)))

(define (INFRABEL-update-trains!)
  ((INFRABEL 'update-trains!) (read in)))

(define (INFRABEL-update-trajectories!)
  ((INFRABEL 'add-trajectories!) (read in)))

(define (INFRABEL-update!)
  ((INFRABEL 'update-train-positions!))
  ((INFRABEL 'update-trajectories!)))

(define (INFRABEL-output-detection-blocks)
  (write ((INFRABEL 'update-detection-blocks!)) out)
  (flush-output out)
  (sleep 0.01)
  (write ((INFRABEL 'retrieve-DB-reservations)) out)
  (flush-output out)
  (sleep 0.01))

(define (INFRABEL-output-updated-trains)
  (write ((INFRABEL 'retrieve-all-trains)) out)
  (flush-output out)
  (sleep 0.01))

(define (INFRABEL-output-updated-switches)
  (write ((INFRABEL 'retrieve-all-switches)) out)
  (flush-output out)
  (sleep 0.01))

;; Main loop that updates everything as should be
(define (INFRABEL-main-loop)
  (INFRABEL-update-components!)
  (INFRABEL-update-trains!)
  (INFRABEL-output-detection-blocks) ;; Works till here
  (INFRABEL-update-trajectories!) 
  (INFRABEL-update-trains!)
  (INFRABEL-update!)
  (INFRABEL-output-updated-trains)
  (INFRABEL-output-updated-switches)
  (INFRABEL-update-trains!)
  (sleep 0.5)
  (if end-connection (close-server) (INFRABEL-main-loop)))

(thread INFRABEL-main-loop)


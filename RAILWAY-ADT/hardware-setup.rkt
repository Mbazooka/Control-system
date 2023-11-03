;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Hardware-setup ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "./railway-components/barrier-adt.rkt"
         "./railway-components/detection-block-adt.rkt"
         "./railway-components/light-adt.rkt"
         "./railway-components/switch-adt.rkt"
         "./railway-components/track-adt.rkt"
         "./railway-components/train-adt.rkt")

(provide make-hardware-setup-adt)

;; The different components on the hardware railway and their links
(define (make-hardware-setup-adt)
  
  ;; All the possible switches associated with the actual switch and their initial state
  (define possible-switches (hash 'S-1 (list (make-switch-adt 'S-1 'right) 'right)
                                  'S-2-3 (list (make-switch-adt 'S-2 'right) 'right)
                                  ;'S-3 (make-switch-adt 'S-3 'right)
                                  'S-4 (list (make-switch-adt 'S-4 'right) 'right)
                                  'S-5 (list (make-switch-adt 'S-5 'left) 'left)
                                  'S-6 (list (make-switch-adt 'S-6 'left) 'left)
                                  'S-7 (list (make-switch-adt 'S-7 'left) 'left)
                                  'S-8 (list (make-switch-adt 'S-8 'right) 'right)
                                  'S-9 (list (make-switch-adt 'S-9 'left) 'left)
                                  'S-10 (list (make-switch-adt 'S-10 'right) 'right)
                                  'S-11 (list (make-switch-adt 'S-11 'right) 'right)
                                  'S-12 (list (make-switch-adt 'S-12 'left) 'left)
                                  'S-16 (list (make-switch-adt 'S-16 'left) 'left)
                                  'S-20 (list (make-switch-adt 'S-20 'left) 'left)
                                  'S-23 (list (make-switch-adt 'S-23 'right) 'right)
                                  'S-24 (list (make-switch-adt 'S-24 'left) 'left)
                                  'S-25 (list (make-switch-adt 'S-25 'right) 'right)
                                  'S-26 (list (make-switch-adt 'S-26 'right) 'right)
                                  'S-27 (list (make-switch-adt 'S-27 'left) 'left)
                                  'S-28 (list (make-switch-adt 'S-28 'left) 'left)))

  ;; All the possible lights
  (define possible-lights (hash 'L-1 (make-light-adt 'L-1)
                                'L-2 (make-light-adt 'L-2)))

  ;; All the possible crossings
  (define possible-crossings (hash 'C-1 (make-barrier-adt 'C-1)
                                   'C-2 (make-barrier-adt 'C-2)))

  ;; Tracks with detection-blocks
  (define T-1-1 (make-track-adt 'T-1-1 '1-1 #f #f 'C-1))
  (define T-1-2 (make-track-adt 'T-1-2 '1-2 #f #f 'C-1))
  (define T-1-3 (make-track-adt 'T-1-3 '1-3 'L-1 #f 'C-1))
  (define T-1-4 (make-track-adt 'T-1-4 '1-4 #f #f #f))
  (define T-1-5 (make-track-adt 'T-1-5 '1-5 'L-2 #f 'C-2))
  (define T-1-6 (make-track-adt 'T-1-6 '1-6 #f #f 'C-2))
  (define T-1-7 (make-track-adt 'T-1-7 '1-7 #f #f #f))
  (define T-1-8 (make-track-adt 'T-1-8 '1-8 #f #f #f))
  (define T-2-1 (make-track-adt 'T-2-1 '2-1 #f #f #f))
  (define T-2-2 (make-track-adt 'T-2-2 '2-2 #f #f #f))
  (define T-2-3 (make-track-adt 'T-2-3 '2-3 #f #f #f))
  (define T-2-4 (make-track-adt 'T-2-4 '2-4 #f #f #f))
  (define T-2-5 (make-track-adt 'T-2-5 '2-5 #f #f #f))
  (define T-2-6 (make-track-adt 'T-2-6 '2-6 #f #f #f))
  (define T-2-7 (make-track-adt 'T-2-7 '2-7 #f #f #f))
  (define T-2-8 (make-track-adt 'T-2-8 '2-8 #f #f #f)) 

  ;; Tracks without detection-blocks and switches
  (define T-U-1 (make-track-adt 'T-U-1 #f #f #f #f))
  (define T-U-2 (make-track-adt 'T-U-2 #f #f #f #f))
  (define T-U-3 (make-track-adt 'T-U-3 #f #f #f #f))
  (define T-U-4 (make-track-adt 'T-U-4 #f #f #f #f))
  (define T-U-5 (make-track-adt 'T-U-5 #f #f #f #f))
  (define T-U-6 (make-track-adt 'T-U-6 #f #f #f #f))
  (define T-U-7 (make-track-adt 'T-U-7 #f #f #f 'C-1))

  ;; Tracks with switches
  (define T-1 (make-track-adt 'T-1 #f #f 'S-1 #f))
  (define T-2/3 (make-track-adt 'T-2/3 #f #f 'S-2-3 #f))
  ;(define T-3 (make-track-adt 'T-3 #f #f 'S-3 #f))
  (define T-4 (make-track-adt 'T-4 #f #f 'S-4 #f))
  (define T-5 (make-track-adt 'T-5 #f #f 'S-5 #f))
  (define T-6 (make-track-adt 'T-6 #f #f 'S-6 #f))
  (define T-7 (make-track-adt 'T-7 #f #f 'S-7 #f))
  (define T-8 (make-track-adt 'T-8 #f #f 'S-8 #f))
  (define T-9 (make-track-adt 'T-9 #f #f 'S-9 #f))
  (define T-10 (make-track-adt 'T-10 #f #f 'S-10 #f))
  (define T-11 (make-track-adt 'T-11 #f #f 'S-11 #f))
  (define T-12 (make-track-adt 'T-12 #f #f 'S-12 #f))
  (define T-16 (make-track-adt 'T-16 #f #f 'S-16 #f))
  (define T-20 (make-track-adt 'T-20 #f #f 'S-20 #f))
  (define T-23 (make-track-adt 'T-23 #f #f 'S-23 #f))
  (define T-24 (make-track-adt 'T-24 #f #f 'S-24 #f))
  (define T-25 (make-track-adt 'T-25 #f #f 'S-25 #f))
  (define T-26 (make-track-adt 'T-26 #f #f 'S-26 #f))
  (define T-27 (make-track-adt 'T-27 #f #f 'S-27 #f))
  (define T-28 (make-track-adt 'T-28 #f #f 'S-28 #f))

  ;; The tracks connected with each other in a list
  (define railway-connections (list (list T-1-1 T-28) (list T-28 T-1-7) (list T-28 T-U-1)
                                    (list T-U-1 T-26) (list T-26 T-27) (list T-26 T-1-4)
                                    (list T-27 T-1-3) (list T-27 T-1-2) (list T-1-4 T-1-5)
                                    (list T-1-7 T-1-6) (list T-1-8 T-25) (list T-25 T-U-5)
                                    (list T-25 T-1) (list T-1 T-U-6) (list T-1 T-2-1) 
                                    (list T-U-6 T-2/3) (list T-5 T-1-6)
                                    (list T-2/3 T-7) (list T-7 T-U-5) (list T-2/3 T-8)
                                    (list T-2/3 T-2-2) (list T-7 T-5) (list T-5 T-6)
                                    (list T-6 T-2-3) (list T-6 T-20) (list T-8 T-4)
                                    (list T-8 T-2-5) (list T-4 T-2-7) (list T-4 T-2-6)
                                    (list T-20 T-1-5) (list T-20 T-2-4) (list T-2-4 T-23)
                                    (list T-23 T-U-4) (list T-23 T-24) (list T-24 T-U-2)
                                    (list T-24 T-U-3) (list T-U-3 T-9) (list T-9 T-11)
                                    (list T-11 T-10) (list T-10 T-1-1) (list T-10 T-16)
                                    (list T-16 T-2-8) (list T-16 T-U-7) (list T-U-2 T-1-3)
                                    (list T-12 T-U-4) (list T-12 T-2-3) (list T-1-2 T-9) 
                                    (list T-11 T-12) 
                                    ))

  ;; Hashmap for linking names and tracks
  (define track-map (hash 'T-1-1 T-1-1 'T-1-2 T-1-2 'T-1-3 T-1-3 'T-1-4 T-1-4
                          'T-1-5 T-1-5 'T-1-6 T-1-6 'T-1-7 T-1-7 'T-1-8 T-1-8
                          'T-2-1 T-2-1 'T-2-2 T-2-2 'T-2-3 T-2-3 'T-2-4 T-2-4
                          'T-2-5 T-2-5 'T-2-6 T-2-6 'T-2-7 T-2-7 'T-2-8 T-2-8
                          'T-U-1 T-U-1 'T-U-2 T-U-2 'T-U-3 T-U-3 'T-U-4 T-U-4
                          'T-U-5 T-U-5 'T-U-6 T-U-6 'T-U-7 T-U-7 
                          'T-1 T-1 'T-2/3 T-2/3 'T-4 T-4 'T-5 T-5 'T-6 T-6
                          'T-7 T-7 'T-8 T-8 'T-9 T-9 'T-10 T-10 'T-11 T-11
                          'T-12 T-12 'T-16 T-16 'T-20 T-20 'T-23 T-23
                          'T-24 T-24 'T-25 T-25 'T-26 T-26 'T-27 T-27 'T-28 T-28))

  (define (dispatch msg)
    (cond    
      ((eq? msg 'get-possible-switches) possible-switches)      
      ((eq? msg 'possible-lights) possible-lights)
      ((eq? msg 'possible-crossings) possible-crossings)
      ((eq? msg 'get-railway-connections) railway-connections)
      ((eq? msg 'get-track-map) track-map)
      (else
       "HARDWARE-SETUP-ADT: Incorrect message")))
  dispatch)






      
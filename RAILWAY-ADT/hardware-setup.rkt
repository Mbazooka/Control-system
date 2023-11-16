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
  (define possible-switches (hash 'S-1  (make-switch-adt 'S-1) ;; The switch name linked with the switch object and initial state
                                  'S-2  (make-switch-adt 'S-2)
                                  'S-3  (make-switch-adt 'S-3)
                                  'S-4 (make-switch-adt 'S-4)
                                  'S-5 (make-switch-adt 'S-5)
                                  'S-6 (make-switch-adt 'S-6)
                                  'S-7 (make-switch-adt 'S-7)
                                  'S-8 (make-switch-adt 'S-8)
                                  'S-9 (make-switch-adt 'S-9)
                                  'S-10 (make-switch-adt 'S-10)
                                  'S-11 (make-switch-adt 'S-11)
                                  'S-12 (make-switch-adt 'S-12)
                                  'S-16 (make-switch-adt 'S-16)
                                  'S-20 (make-switch-adt 'S-20)
                                  'S-23 (make-switch-adt 'S-23)
                                  'S-24 (make-switch-adt 'S-24)
                                  'S-25 (make-switch-adt 'S-25)
                                  'S-26 (make-switch-adt 'S-26)
                                  'S-27 (make-switch-adt 'S-27)
                                  'S-28 (make-switch-adt 'S-28)))

  ;; All the possible lights
  (define possible-lights (hash 'L-1 (make-light-adt 'L-1)
                                'L-2 (make-light-adt 'L-2)))

  ;; All the possible crossings
  (define possible-barriers (hash 'C-1 (make-barrier-adt 'C-1)
                                  'C-2 (make-barrier-adt 'C-2)))

  ;; All possible detection-blocks linked with the tracks
  (define possible-detection-blocks
    (hash '1-1 (make-detection-block-adt '1-1)
          '1-2 (make-detection-block-adt '1-2)
          '1-3 (make-detection-block-adt '1-3)
          '1-4 (make-detection-block-adt '1-4)
          '1-5 (make-detection-block-adt '1-5)
          '1-6 (make-detection-block-adt '1-6)
          '1-7 (make-detection-block-adt '1-7)
          '1-8 (make-detection-block-adt '1-8)
          '2-1 (make-detection-block-adt '2-1)
          '2-2 (make-detection-block-adt '2-2)
          '2-3 (make-detection-block-adt '2-3)
          '2-4 (make-detection-block-adt '2-4)
          '2-5 (make-detection-block-adt '2-5)
          '2-6 (make-detection-block-adt '2-6)
          '2-7 (make-detection-block-adt '2-7)
          '2-8 (make-detection-block-adt '2-8)))
                          
    

  ;; Tracks with detection-blocks
  (define 1-1 (make-track-adt '1-1 #f #f 'C-1))
  (define 1-2 (make-track-adt '1-2 #f #f 'C-1))
  (define 1-3 (make-track-adt '1-3 'L-1 #f 'C-1))
  (define 1-4 (make-track-adt '1-4 #f #f #f))
  (define 1-5 (make-track-adt '1-5 'L-2 #f 'C-2))
  (define 1-6 (make-track-adt '1-6 #f #f 'C-2))
  (define 1-7 (make-track-adt '1-7 #f #f #f))
  (define 1-8 (make-track-adt '1-8 #f #f #f))
  (define 2-1 (make-track-adt '2-1 #f #f #f))
  (define 2-2 (make-track-adt '2-2 #f #f #f))
  (define 2-3 (make-track-adt '2-3 #f #f #f))
  (define 2-4 (make-track-adt '2-4 #f #f #f))
  (define 2-5 (make-track-adt '2-5 #f #f #f))
  (define 2-6 (make-track-adt '2-6 #f #f #f))
  (define 2-7 (make-track-adt '2-7 #f #f #f))
  (define 2-8 (make-track-adt '2-8 #f #f #f)) 

  ;; Tracks with switches (Made for PHASE 2 purposes, not used yet)
  (define T-1 (make-track-adt #f #f 'S-1 #f))
  (define T-2 (make-track-adt #f #f 'S-2 #f))
  (define T-3 (make-track-adt #f #f 'S-3 #f))
  (define T-4 (make-track-adt #f #f 'S-4 #f))
  (define T-5 (make-track-adt #f #f 'S-5 #f))
  (define T-6 (make-track-adt #f #f 'S-6 #f))
  (define T-7 (make-track-adt #f #f 'S-7 #f))
  (define T-8 (make-track-adt #f #f 'S-8 #f))
  (define T-9 (make-track-adt #f #f 'S-9 #f))
  (define T-10 (make-track-adt #f #f 'S-10 #f))
  (define T-11 (make-track-adt #f #f 'S-11 #f))
  (define T-12 (make-track-adt #f #f 'S-12 #f))
  (define T-16 (make-track-adt #f #f 'S-16 #f))
  (define T-20 (make-track-adt #f #f 'S-20 #f))
  (define T-23 (make-track-adt #f #f 'S-23 #f))
  (define T-24 (make-track-adt #f #f 'S-24 #f))
  (define T-25 (make-track-adt #f #f 'S-25 #f))
  (define T-26 (make-track-adt #f #f 'S-26 #f))
  (define T-27 (make-track-adt #f #f 'S-27 #f))
  (define T-28 (make-track-adt #f #f 'S-28 #f))

  ;; The tracks connected with each other in a list
  (define railway-connections '((1-1 T-28) (1-7 T-28) 
                                             (T-26 T-27) (T-26 1-4) (T-28 T-26)
                                             (T-27 1-3) (T-27 1-2) (1-4 1-5)
                                             (1-8 T-25) (1-6 1-7)
                                             (T-25 T-1) (T-1 2-1) 
                                             (T-1 T-2) (T-5 1-6) (T-2 T-3)
                                             (T-2 T-7) (T-7 T-25) (T-3 T-8)
                                             (T-3 2-2) (T-7 T-5) (T-5 T-6)
                                             (T-6 2-3) (T-6 T-20) (T-8 T-4)
                                             (T-8 2-5) (T-4 2-7) (T-4 2-6)
                                             (T-20 2-4) (2-4 T-23)
                                             (T-23 T-12) (T-23 T-24) 
                                             (T-24 T-9) (T-9 T-11)
                                             (T-11 T-10) (T-10 1-1) (T-10 T-16)
                                             (T-16 2-8) (T-16 NO-TRACK) (T-24 1-3)
                                             (T-12 2-3) (1-2 T-9) 
                                             (T-11 T-12) (1-8 NO-TRACK) (2-1 NO-TRACK)
                                             (2-2 NO-TRACK) (2-5 NO-TRACK) (2-6 NO-TRACK)
                                             (2-7 NO-TRACK) (1-5 T-20)
                                             ))

  ;; Hashmap for linking names and tracks
  (define track-map (hash '1-1 1-1 '1-2 1-2 '1-3 1-3 '1-4 1-4
                          '1-5 1-5 '1-6 1-6 '1-7 1-7 '1-8 1-8
                          '2-1 2-1 '2-2 2-2 '2-3 2-3 '2-4 2-4
                          '2-5 2-5 '2-6 2-6 '2-7 2-7 '2-8 2-8           
                          'T-1 T-1 'T-2 T-2 'T-3 T-3 'T-4 T-4 'T-5 T-5 'T-6 T-6
                          'T-7 T-7 'T-8 T-8 'T-9 T-9 'T-10 T-10 'T-11 T-11
                          'T-12 T-12 'T-16 T-16 'T-20 T-20 'T-23 T-23
                          'T-24 T-24 'T-25 T-25 'T-26 T-26 'T-27 T-27 'T-28 T-28))

  (define (dispatch msg)
    (cond    
      ((eq? msg 'get-possible-switches) possible-switches)      
      ((eq? msg 'get-possible-lights) possible-lights)
      ((eq? msg 'get-possible-detection-blocks) possible-detection-blocks)
      ((eq? msg 'get-possible-barriers) possible-barriers)
      ((eq? msg 'get-railway-connections) railway-connections)
      ((eq? msg 'get-track-map) track-map)
      (else
       "HARDWARE-SETUP-ADT: Incorrect message")))
  dispatch)







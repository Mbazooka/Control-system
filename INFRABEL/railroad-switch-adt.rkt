;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Railroad switch ADT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../simulator/interface.rkt")
(provide make-railroad-switch-adt
         get-all-switches)

(define (make-railroad-switch-adt name)

  (define (current-position)
    (get-switch-position name))

  (define (change-position! state)
    (cond
      ((eq? state 'initial) (set-switch-position! name 1)) ;; Initial sets the switch on the initial position (1)
      ((eq? state 'deviate) (set-switch-position! name 2)) ;; Deviation changes the position of the switch (2)
      (else
       "RAILROAD-SWITCH: Incorrect state in change-position!")))

  (define (initial-position?) (= (current-position) 1))
  (define (deviate-position?) (= (current-position) 2))

  (define (dispatch msg)
    (cond
      ((eq? msg 'current-position) current-position)
      ((eq? msg 'change-position!) change-position!)
      ((eq? msg 'initial-position?) initial-position?)
      ((eq? msg 'deviate-position?) deviate-position?)
      (else
       "RAILROAD-SWITCH: Incorrect message")))
  dispatch)

;; TO BE CHECKED IF GOOD OR BAD?????????
(define get-all-switches get-switch-ids)
;; ?????????????????????????????????????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               GUI ADT Test                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui/base)

;; Counter class
(define (make-counter)
  (let ((ctr 0)) ;; Actual counter
    
    (define (get-counter) ctr)

    (define (increment!) (set! ctr (+ ctr 1)))
    (define (decrement!) (when (>= ctr 0) (set! ctr (- ctr 1))))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-counter) (get-counter))
        ((eq? msg 'increment!) (increment!))
        ((eq? msg 'decerement!) (decrement!))
        (else
         "MAKE-COUNTER: Incorrect message")))
    dispatch))

(define trains-on-railway '()) ;; The trains currently on the railway
(define train-counter (make-counter)) ;; Counter to keep up amount of trains         

;; Constants that determine frame size and title
(define FRAME-WIDTH 800)
(define FRAME-HEIGHT 600)
(define GUI-TITLE "Control Panel")

;; Used for selecting the right tab
(define TRAIN-TAB 0)
(define DETECTION-BLOCKS-TAB 1)
(define SWITCHES-TAB 2)
(define BARRIERS/LIGHTS-TAB 3)
(define current-tab 0)

;; Used for the right offset of the add-train button
(define HORIZONTAL-OFFSET-ADD-TRAIN-BUTTON 190)

;; This is the main frame on which everything is drawn
(define control-panel (new frame%
                           [label GUI-TITLE]
                           [width FRAME-WIDTH]
                           [height FRAME-HEIGHT]))

(send control-panel show #t)

;; The main tabs for adjusting the Hardware components
(define main-tab-panel
  (new tab-panel%
       [parent control-panel]
       [choices (list "Trains"
                      "Detection blocks"
                      "Switches"
                      "Barriers and lights")]))

;; Draws a panel on top of the mainframe, adding in a vertical manner
(define top-panel
  (new vertical-panel%
       [parent main-tab-panel]
       ))

;; Draws a tab-panel for the different trains (in the train-tab)
(define train-tabs '())

(define (draw-train-tabs)
  (set! train-tabs
        (new tab-panel%
             [parent top-panel]
             [choices trains-on-railway])))

;; Draws a panel on top of the top-panel, adding in a horizontal manner
(define second-panel
  (new horizontal-panel%
       [parent main-tab-panel]
       ))

;; Button to be added to the train-tab
(define add-train-button '())

(define (make-add-train-button)
  (set! add-train-button
        (new button%
             [label "Add train"]
             [parent second-panel]
             [horiz-margin HORIZONTAL-OFFSET-ADD-TRAIN-BUTTON]
             )))

;; Button to be added to the train-tab
(define delete-train-button '())

(define (make-delete-train-button)
  (set! delete-train-button
        (new button%
             [label "Delete train"]
             [parent second-panel])))

;; Message for train-tab

;; Initial drawing for tabs
(draw-train-tabs)
(make-add-train-button)
(make-delete-train-button)
(sleep 5)

;; Remove the elements
(define (remove-train-tab-elements)
  (send train-tabs show #f)
  (send add-train-button show #f)
  (send delete-train-button show #f))

(remove-train-tab-elements)




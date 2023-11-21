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
        ((eq? msg 'get-value) (get-counter))
        ((eq? msg 'increment!) (increment!))
        ((eq? msg 'decerement!) (decrement!))
        (else
         "MAKE-COUNTER: Incorrect message")))
    dispatch))

(define train-counter (make-counter)) ;; Counter to keep up amount of trains
(define max-train-speed 200)
(define min-train-speed -200)

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
(define previous-tab 0)

;; Used for the right offset of the add-train button
(define HORIZONTAL-OFFSET-ADD-TRAIN-BUTTON 190)

;; Used for the message placed on train tabs
(define current-train-tab-message "Set train speed")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;; DONT CHANGE ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-train-tabs (make-hash)) ;; The trains currently and their tabs on the railway
  
;; Draws a panel on top of the mainframe, adding in a vertical manner
(define top-panel
  (new vertical-panel%
       [parent main-tab-panel]
       ))

;; Draws a tab-panel for the different trains (in the train-tab)
(define train-tab
  (new tab-panel%
       [parent top-panel]
       [choices '()]
       [callback (lambda (tab event)
                   (let* ((name-train (send tab get-item-label (send tab get-selection)))
                          (value-to-set (hash-ref all-train-tabs name-train)))
                     (send slider set-value value-to-set)))]))
                     

;; Draws a panel on top of the tab, in a vertical manner
(define top-tab-panel
  (new vertical-panel%
       [parent train-tab]
       ))

;; Draws a panel on top of the top-panel, adding in a horizontal manner
(define second-panel ;; ADJUSTED (main-tab-panel)
  (new horizontal-panel%
       [parent top-panel]
       ))
    

;; Button to be added to the train-tab and it's logic
(define (tab-name-generator) ;; Generates name for a tab
  (train-counter 'increment!)
  (format "Train-~a" (train-counter 'get-value)))
  
(define (all-train-tabs-add! train-name) ;; Add an element to all-train-tabs
  (hash-set! all-train-tabs train-name 0)) ;; Adding a train is always at speed 0

(define (add-train-button-logic! panel event) ;; Logic behind the button
  (let ((name (tab-name-generator)))
    (all-train-tabs-add! name) 
    (send train-tab append name)
    (cond ((= (hash-count all-train-tabs) 1) (show-current-train-elements!))))) ;; Might need to be changed because not accessible

(define add-train-button
  (new button%
       [label "Add train"]
       [parent second-panel]
       [callback add-train-button-logic!]
       [horiz-margin HORIZONTAL-OFFSET-ADD-TRAIN-BUTTON]
       ))

;; Button to be added to the train-tab and it's logic
(define (all-train-tabs-delete! train-name) ;; Deletes an element from all-train-tabs (for garbage collection)
  (hash-remove! all-train-tabs train-name))
  
(define (delete-trian-button-logic! panel event) ;; Deletes a running train tab from the tab list
  (let ((selected-tab (send train-tab get-selection)))
    (all-train-tabs-delete! (send train-tab get-item-label selected-tab)) ;; +1 due to tab starting differently
    (send train-tab delete (send train-tab get-selection))
    (cond ((= (hash-count all-train-tabs) 0) (remove-current-train-elements!)))))

(define delete-train-button
  (new button%
       [label "Delete train"]
       [parent second-panel]
       [callback delete-trian-button-logic!]))

;; All the elements for the running-train-tab
(define display-message (new message%
                             [label current-train-tab-message]
                             [parent top-tab-panel]))

(define (slider-logic! slider event)
  (let ((name (send train-tab get-item-label (send train-tab get-selection))))
    (hash-set! all-train-tabs name (send slider get-value))))
    
(define slider
  (new slider%
       [label ""]
       [parent top-tab-panel]
       [callback slider-logic!]
       [min-value min-train-speed]
       [max-value max-train-speed]
       [init-value 0]
       [vert-margin 100]))
  
(define (remove-current-train-elements!)
  (send display-message show #f)
  (send slider show #f))

(define (show-current-train-elements!)
  (send display-message show #t)
  (send slider show #t))

(remove-current-train-elements!)
  






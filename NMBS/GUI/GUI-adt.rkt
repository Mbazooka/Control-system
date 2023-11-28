;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  GUI ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui/base)
(provide provide-trains
         provide-switches
         provide-barriers
         provide-lights
         update-detection-blocks!) ;; API of GUI

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
(define tab-drawing '()) ;; To be initialized later
(define current-tab 0)

;; The trains on the railway
(define all-train-tabs (make-hash)) 

(define (get-train-names-with-speed)
  (map car (hash->list all-train-tabs)))

;; List of possible switches and their current state
(define switch-name-state (list (mcons "S-1" 1) (mcons "S-2" 1) (mcons "S-3" 1) (mcons "S-4" 1) (mcons "S-5" 1)
                                (mcons "S-6" 1) (mcons "S-7" 1) (mcons "S-8" 1) (mcons "S-9" 1) (mcons "S-10" 1)
                                (mcons "S-11" 1) (mcons "S-12" 1) (mcons "S-16" 1) (mcons "S-20" 1)
                                (mcons "S-23" 1) (mcons "S-24" 1) (mcons "S-25" 1) (mcons "S-26" 1)
                                (mcons "S-27" 1) (mcons "S-28" 1)))

(define middle-switch 9)

;; List of possible barriers and lights and their state
(define barrier-name-state (list (mcons "C-1" 1) (mcons "C-2" 1)))
(define light-name-state (list (mcons "L-1" "Hp0") (mcons "L-2" "Hp0")))

;; List of possible detection-blocks and their state
(define detection-block-name-state (list (mcons "1-1 :" "No presence") (mcons "1-2 :" "No presence") (mcons "1-3 :" "No presence")
                                         (mcons "1-4 :" "No presence") (mcons "1-5 :" "No presence") (mcons "1-6 :" "No presence")
                                         (mcons "1-7 :" "No presence") (mcons "1-8 :" "No presence") (mcons "2-1 :" "No presence")
                                         (mcons "2-2 :" "No presence") (mcons "2-3 :" "No presence") (mcons "2-4 :" "No presence")
                                         (mcons "2-5 :" "No presence") (mcons "2-6 :" "No presence") (mcons "2-7 :" "No presence")
                                         (mcons "2-8 :" "No presence")))

(define middle-detection-block 9)

(define name-hardware mcar) ;; Abstractions
(define state-switch (lambda (pair) (- (mcdr pair) 1))) ;; -1 to convert to radio box data
(define state-hardware mcdr)

;; Adjusts the state of a hardware component in the given list to the given value
(define (adjust-state! hardware val components)
  (define (adjust-state-help hardware val current)
    (if (null? current)
        (error "Hardware component does not exist")
        (let ((current-pair (car current)))
          (if (string=? (name-hardware current-pair) hardware)
              (set-mcdr! current-pair val)
              (adjust-state-help hardware val (cdr current))))))
  (adjust-state-help hardware val components))

;; General procedure implementing the logic for all radioboxes
(define (radiobox-logic! hardware adjustment-proc pair)
  (lambda (this event)
    (let ((item-selected (send this get-selection)))
      (adjust-state! (name-hardware pair) (adjustment-proc item-selected) hardware))))

;; Used for the right offset of the add-train button
(define HORIZONTAL-OFFSET-ADD-TRAIN-BUTTON 190)
(define VERTICAL-OFFSET-SLIDER 50)

;; Used for the message placed on train tabs
(define current-train-tab-message "Set train speed")

;; This is the main frame on which everything is drawn
(define control-panel (new frame%
                           [label GUI-TITLE]
                           [width FRAME-WIDTH]
                           [height FRAME-HEIGHT]))

(send control-panel show #t)

;; Adjust tab drawing
(define (adjust-tab! panel)
  (tab-drawing 'remove-panel!)
  (set! tab-drawing (panel)))

;; Do action according to the tab
(define (tab-action! tab)
  (cond
    ((= tab TRAIN-TAB) (adjust-tab! draw-train-panel!))
    ((= tab DETECTION-BLOCKS-TAB) (adjust-tab! draw-detection-block-panel!))
    ((= tab SWITCHES-TAB) (adjust-tab! draw-switch-panel!))
    ((= tab BARRIERS/LIGHTS-TAB) (adjust-tab! draw-barrier/light-panel!))))


;; The main tabs for adjusting the Hardware components
(define main-tab-panel
  (new tab-panel%
       [parent control-panel]
       [callback (lambda (this event)
                   (let ((selected-item (send this get-selection)))
                     (tab-action! selected-item)))]
       [choices (list "Trains"
                      "Detection blocks"
                      "Switches"
                      "Barriers and lights")]))

;; Draws the train tab of the main tabs
(define (draw-train-panel!)

  ;; Draws a panel on top of the mainframe, adding in a vertical manner
  (define train-panel
    (new vertical-panel%
         [parent main-tab-panel]
         ))

  (define (train-tab-change-logic! tab event)
    (let* ((name-train (send tab get-item-label (send tab get-selection)))
           (train-data  (hash-ref all-train-tabs name-train))
           (value-to-set (caddr train-data)))
      (send slider set-value value-to-set)))

  ;; Draws a tab-panel for the different trains (in the train-tab)
  (define train-tab
    (new tab-panel%
         [parent train-panel]
         [choices (get-train-names-with-speed)]
         [callback train-tab-change-logic!]))
                     

  ;; Draws a panel on top of the tab, in a vertical manner
  (define top-tab-panel
    (new vertical-panel%
         [parent train-tab]
         ))

  ;; Draws a panel on top of the top-tab-panel, adding in a horizontal manner
  (define second-panel 
    (new horizontal-panel%
         [parent train-panel]
         ))

  ;; Data to put in drawdown menu
  (define display-data (append (map (lambda (pair) (substring (mcar pair) 0 3)) detection-block-name-state)
                               (map mcar switch-name-state)))

  ;; dropdown menu to determine right place to put train
  (define initial-track (new choice%
                             [label "Initial-track:"]
                             [parent train-panel]
                             [choices (map (lambda (pair) (substring (mcar pair) 0 3)) detection-block-name-state)]))

  (define track-behind (new choice%
                            [label "Track-behind:"]
                            [parent train-panel]
                            [choices display-data]))
    

  ;; Button to be added to the train-tab and it's logic
  (define (tab-name-generator) ;; Generates name for a tab
    (train-counter 'increment!)
    (format "Train-~a" (train-counter 'get-value)))
  
  (define (all-train-tabs-add! train-name data) ;; Add an element to all-train-tabs
    (hash-set! all-train-tabs train-name data)) ;; Adding a train is always at speed 0

  (define (add-train-button-logic! panel event) ;; Logic behind the button
    (let ((name (tab-name-generator))
          (initial-track-sel (string->symbol (send initial-track get-string-selection)))
          (track-behind-sel (string->symbol (send track-behind get-string-selection))))
      (all-train-tabs-add! name (list initial-track-sel track-behind-sel 0)) 
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
  
  (define (delete-train-button-logic! panel event) ;; Deletes a running train tab from the tab list
    (let ((selected-tab (send train-tab get-selection)))
      (all-train-tabs-delete! (send train-tab get-item-label selected-tab)) ;; +1 due to tab starting differently
      (send train-tab delete (send train-tab get-selection))
      (cond ((= (hash-count all-train-tabs) 0) (remove-current-train-elements!))
            (else (train-tab-change-logic! train-tab #f)))))

  (define delete-train-button
    (new button%
         [label "Delete train"]
         [parent second-panel]
         [callback delete-train-button-logic!]))

  ;; All the elements for the running-train-tab
  (define display-message (new message%
                               [label current-train-tab-message]
                               [parent top-tab-panel]))

  (define (slider-logic! slider event) ;; Logic for slider
    (let* ((name (send train-tab get-item-label (send train-tab get-selection)))
           (train-data (hash-ref all-train-tabs name)))
      (hash-set! all-train-tabs name (list (car train-data)
                                           (cadr train-data)
                                           (send slider get-value)))))
    
  (define slider ;; Slider itself
    (new slider%
         [label ""]
         [parent top-tab-panel]
         [callback slider-logic!]
         [min-value min-train-speed]
         [max-value max-train-speed]
         [init-value 0]
         [vert-margin VERTICAL-OFFSET-SLIDER]))
  
  (define (remove-current-train-elements!) ;; Removes the message and slider from the screen 
    (send display-message show #f)
    (send slider show #f))

  (define (show-current-train-elements!) ;; Shows the message and slider on the screen
    (send display-message show #t)
    (send slider show #t))

  (remove-current-train-elements!)

  (cond ((>= (hash-count all-train-tabs) 1)
         (show-current-train-elements!)
         (train-tab-change-logic! train-tab #f)))

  (define (remove-panel!)
    (send main-tab-panel delete-child train-panel))

  (define (dispatch msg)
    (cond
      ((eq? msg 'remove-panel!) (remove-panel!))
      (else
       "DRAW-TRAIN-PANEL!: Illegal message")))
  dispatch)

(set! tab-drawing (draw-train-panel!))

;; Draws the switch tab of the mains tabs
(define (draw-switch-panel!)

  ;; Draw main switch panel
  (define switch-panel
    (new horizontal-panel%
         [parent main-tab-panel]
         ))

  ;; Draw panel for switch on the leftsize gui
  (define left-switch-panel-vertical
    (new vertical-panel%
         [parent switch-panel]
         ))

  ;; Draw panel for switch on the rightsize gui
  (define right-switch-panel-vertical
    (new vertical-panel%
         [parent switch-panel]
         ))

  ;; Draws all the radio boxes
  (define (draw-all-switches!) ;; Draws the radio boxes
    (define current-parent left-switch-panel-vertical)
    (define ctr 0) ;; Counter to allow more concise code
    (for-each (lambda (switch-pair) 
                (if (<= ctr middle-switch)
                    (set! current-parent left-switch-panel-vertical)
                    (set! current-parent right-switch-panel-vertical))
                (set! ctr (+ ctr 1))
                (new radio-box%
                     [label (name-hardware switch-pair)]
                     [parent current-parent]
                     [callback (radiobox-logic! switch-name-state (lambda (nmbr) (+ nmbr 1)) switch-pair)]
                     [choices (list "1"
                                    "2")]
                     [selection (state-switch switch-pair)]
                     ))
              switch-name-state))
  (draw-all-switches!)

  ;; Removes all the drawn elements corresponding to this tab
  (define (remove-panel!)
    (send main-tab-panel delete-child switch-panel))

  (define (dispatch msg)
    (cond
      ((eq? msg 'remove-panel!) (remove-panel!))
      (else
       "DRAW-SWITCH-PANEL!: Illegal message")))
  dispatch)

  
;; Draws the Barriers and light panel
(define (draw-barrier/light-panel!)

  ;; Draw main barrier/light panel
  (define barrier/light-panel
    (new horizontal-panel%
         [parent main-tab-panel]
         ))

  ;; Draw panel for barriers
  (define left-light-panel
    (new vertical-panel%
         [parent barrier/light-panel]
         ))

  ;; Draw panel for lights
  (define right-barrier-panel
    (new vertical-panel%
         [parent barrier/light-panel]
         ))

  ;; Draw all the radioboxes for barriers
  (define (draw-all-barriers!)
    (for-each (lambda (barrier-pair)
                (new radio-box%
                     [label (name-hardware barrier-pair)]
                     [parent right-barrier-panel]
                     [callback (radiobox-logic! barrier-name-state
                                                (lambda (x) x)
                                                barrier-pair)]
                     [choices (list "closed"
                                    "open")]
                     [selection (state-hardware barrier-pair)]))
              barrier-name-state))

  ;; Draws all the dropdown menus for barriers
  (define (draw-all-lights!)
    (for-each (lambda (light-pair)
                (new choice%
                     [label (name-hardware light-pair)]
                     [parent left-light-panel]
                     [callback (lambda (this event)
                                 (let ((selected-item (send this get-string-selection)))
                                   (adjust-state! (name-hardware light-pair) selected-item light-name-state)))]                              
                     [choices (list "Hp0" "Hp1" "Hp0+Sh0"
                                    "Ks1+Zs3" "Ks2" "Ks2+Zs3"
                                    "Sh1" "Ks1+Zs3+Zs3v")]
                     [selection (let ((light-name (state-hardware light-pair)))
                                  (cond
                                    ((string=? light-name "Hp0") 0)
                                    ((string=? light-name "Hp1") 1)
                                    ((string=? light-name "Hp0+Sh0") 2)
                                    ((string=? light-name "Ks1+Zs3") 3)
                                    ((string=? light-name "Ks2") 4)
                                    ((string=? light-name "Ks2+Zs3") 5)
                                    ((string=? light-name "Sh1") 6)
                                    ((string=? light-name "Ks1+Zs3+Zs3v") 7)))]
                     ))
              light-name-state))


  ;; Calling the functions to draw everything
  (draw-all-barriers!)
  (draw-all-lights!)

  ;; Removes all the drawn elements corresponding to this tab
  (define (remove-panel!)
    (send main-tab-panel delete-child barrier/light-panel))

  (define (dispatch msg)
    (cond
      ((eq? msg 'remove-panel!) (remove-panel!))
      (else
       "DRAW-BARRIER/LIGHT-PANEL!: Illegal message")))
  dispatch)

;; Draws the detection-blocks
(define (draw-detection-block-panel!)
  
  ;; Draw main detection-block panel
  (define detection-block-panel
    (new horizontal-panel%
         [parent main-tab-panel]
         [horiz-margin 0]
         ))

  (define vertical-db-panel
    (new vertical-panel%
         [parent detection-block-panel]
         ))

  (define second-vertical-db-panel ;; Pannel necessary for nicer allignment
    (new vertical-panel%
         [parent detection-block-panel]
         ))

  (define message-panel '())
  
  ;; Draw detection-blocks
  (define (draw-detection-blocks!)
    (set! message-panel
          (new vertical-panel%
               [parent vertical-db-panel]))
    (for-each (lambda (det-bl)
                (new message%
                     [parent message-panel] 
                     [label (string-append (name-hardware det-bl) (state-hardware det-bl))]
                     ))
              detection-block-name-state))
  (draw-detection-blocks!)

  ;; Removes all the drawn elements corresponding to this tab
  (define (remove-panel!)
    (send main-tab-panel delete-child detection-block-panel))

  (define (remove-blocks!) ;; Remove messages from the screen
    (send vertical-db-panel delete-child message-panel))

  (define (draw-update-button!)
    (new button%
         [label "Update"]
         [parent message-panel]
         [callback (lambda (this event)
                     (remove-blocks!)
                     (draw-detection-blocks!)
                     (draw-update-button!))]
         ))
  (draw-update-button!)

  (define (dispatch msg)
    (cond
      ((eq? msg 'remove-panel!) (remove-panel!))
      (else
       "DRAW-DETECTION-BLOCK-PANEL!: Illegal message")))
  dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;; API GUI ;;;;;;;;;;;;;;;;;;;;;;;;; 
(define (provide-trains) ;; Gets the trains in list format
  (map (lambda (hardware-state)
         (cons (string->symbol (car hardware-state)) (cdr hardware-state)))
       (hash->list all-train-tabs)))

(define (provide-abstraction hardware-components cdr-op) ;; Different operation according to hardware
  (lambda ()
    (map (lambda (hardware-state)
           (cons (string->symbol (name-hardware hardware-state)) (cdr-op hardware-state)))
         hardware-components)))

(define provide-switches ;; Gets the switches in list format
  (provide-abstraction switch-name-state mcdr))

(define (convert-barrier-state barrier)
  (let ((barrier-value (mcdr barrier)))
    (= barrier-value 1)))

(define provide-barriers ;; Gets the barriers in list format
  (provide-abstraction barrier-name-state convert-barrier-state))

(define (provide-lights) ;; Gets the lights in list format
  (map (lambda (hardware-state)
         (cons (string->symbol (name-hardware hardware-state)) (string->symbol (state-hardware hardware-state))))
       light-name-state))


;; Helper function to update the detection-blocks
(define (set-correct-format! db-assoc-list) ;; Converts list to right format
  (map (lambda (block)
         (cons (string-append (symbol->string (car block)) " :") (cdr block)))
       db-assoc-list))

(define (convert-presence presence) ;; To be changed depending on the detection-block
  (if presence
      "Presence"
      "No presence"))

(define (update-detection-blocks! db-assoc-list)
  (let ((processed-list (set-correct-format! db-assoc-list)))
    (for-each
     (lambda (block)
       (adjust-state! (car block)
                      (convert-presence (cdr block))
                      detection-block-name-state))
     processed-list)))

  


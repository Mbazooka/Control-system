;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  GUI ADT                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require racket/gui/base)
(provide make-gui-adt) ;; API of GUI

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

(define train-speed (make-hash))

;; The trains on the railway
(define all-train-tabs (make-hash)) 

(define (get-train-names-with-speed)
  (map car (hash->list all-train-tabs)))

(define middle-switch 9) ;; Necessary for nice printing of switches

(define middle-detection-block 9)


(define hardware-name car)
(define hardware-data cdr)

;; Used for the right offset of the add-train button
(define ADD-TRAIN-BUTTON-HOR-MARGIN 330)
(define ADD-TRAIN-UPDATE-BUTTON-HOR-MARGIN 20)
(define SLIDER-VERT-MARGIN 50)
(define DETECTION-BLOCK-DATA-VERT-MARGIN 50)
(define GROUP-BARRIER/LIGHT-PANEL-VERT-MARGIN 100)
(define BARRIER/LIGHT-WIDGET-VERT-MARGIN 30)
(define SWITCH-DATA-VERT-MARGIN 100)
(define SWITCH-CHOICE-WIDGET-VERT-MARGIN 60)
(define SWITCH-MESSAGE-WIDGET-VERT-MARGIN 100)

(define (make-gui-adt switch-adjust-cb switch-retrieve-cb
                      barrier-adjust-cb barrier-retrieve-cb
                      light-adjust-cb light-retrieve-cb
                      db-retrieve-cb train-adjust-cb
                      train-make-cb train-retrieve-cb add-trajectory-cb
                      train-dest-cb train-current-cb train-behind-cb)

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

  ;; Takes the names of the data and converts it to a string
  (define (data-converter data)
    (map (lambda (pair) (symbol->string (hardware-name pair))) data))

  ;; Draws the train tab of the main tabs
  (define (draw-train-panel!)

    ;; Draws a panel on top of the mainframe, adding in a vertical manner
    (define train-panel
      (new vertical-panel%
           [parent main-tab-panel]
           ))

    (define (train-tab-change-logic! tab event)
      (let* ((name-train (string->symbol (send tab get-item-label (send tab get-selection))))
             (train-data  (assoc name-train (train-retrieve-cb)))
             (value-to-set (cadddr train-data)))
        (send slider set-value value-to-set)))

    ;; Draws a tab-panel for the different trains (in the train-tab)
    (define train-tab
      (new tab-panel%
           [parent train-panel]
           [choices (data-converter (train-retrieve-cb))]
           [callback train-tab-change-logic!]
           ))                    

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

    ;; Abstraction
    (define processed-db (data-converter (filter (lambda (element) (not (eq? (car element) '2-8))) (db-retrieve-cb))))

    ;; dropdown menu to determine right place to put train
    (define initial-track (new choice%
                               [label "Current-track:"]
                               [parent train-panel]
                               [choices processed-db]))

    (define track-behind (new choice% 
                              [label "Component-behind:"]
                              [parent train-panel]
                              [choices processed-db]))
    
    ;; Dropdown menu to go to a certain destination
    (define destination (new choice%
                             [label "Destination:"]
                             [parent train-panel]
                             [choices processed-db]))

    ;; Button to go to destination
    (define (destination-button-logic! button event)
      (add-trajectory-cb (string->symbol (send train-tab get-item-label (send train-tab get-selection))) (string->symbol (send destination get-string-selection))))
    
    (define destination-button (new button%
                                    [label "Go to destination"]
                                    [parent train-panel]
                                    [callback destination-button-logic!]))

    ;; Button to be added to the train-tab and it's logic
    (define (tab-name-generator) ;; Generates name for a tab
      (format "Train-~a" (train-counter 'get-value)))

    (define (add-train-button-logic! panel event) ;; Logic behind the button
      (let ((name (tab-name-generator))
            (initial-track-sel (string->symbol (send initial-track get-string-selection)))
            (track-behind-sel (string->symbol (send track-behind get-string-selection))))
        (if (train-make-cb (string->symbol name) initial-track-sel track-behind-sel)
            (begin
            (send train-tab append name)
            (train-counter 'increment!))
            '())
        (cond ((= (length (train-retrieve-cb)) 1) (show-current-train-elements!)))))

    (define add-train-button
      (new button%
           [label "Add train"]
           [parent train-panel]
           [callback add-train-button-logic!]
           [horiz-margin ADD-TRAIN-BUTTON-HOR-MARGIN]
           ))

    (define (add-update-button-logic! panel event) ;; Logic behind the update button
      (let* ((train-name (string->symbol (send train-tab get-item-label (send train-tab get-selection))))            
             (current (train-current-cb train-name))
             (current-behind (train-behind-cb train-name))
             (dest (train-dest-cb train-name)))
        (if (send train-tab get-selection)
            (begin
              (send initial-track set-string-selection (symbol->string current))
              (send track-behind set-string-selection (symbol->string current-behind))
              (if dest (send destination set-string-selection (symbol->string dest)) '())
              (let* ((name-train (string->symbol (send train-tab get-item-label (send train-tab get-selection))))
                     (train-data  (assoc name-train (train-retrieve-cb)))
                     (value-to-set (cadddr train-data)))
                (send slider set-value value-to-set))
              )
            '())
        ))    
      
    (define add-update-button
      (new button%
           [label "Update"]
           [parent train-panel]
           [callback add-update-button-logic!]
           [horiz-margin ADD-TRAIN-UPDATE-BUTTON-HOR-MARGIN]
           ))

    ;; All the elements for the running-train-tab
    (define display-message (new message%
                                 [label current-train-tab-message]
                                 [parent top-tab-panel]))

    (define (slider-logic! slider event) ;; Logic for slider
      (let ((name (string->symbol (send train-tab get-item-label (send train-tab get-selection)))))
        ;(hash-set! train-speed name (send slider get-value))
        (train-adjust-cb name (send slider get-value))))
        
    (define slider ;; Slider itself
      (new slider%
           [label ""]
           [parent top-tab-panel]
           [callback slider-logic!]
           [min-value min-train-speed]
           [max-value max-train-speed]
           [init-value 0]
           [vert-margin SLIDER-VERT-MARGIN]))

    ;; Timer to update certain part of GUI at regular intervals 
;    (define train-slider-timer (new timer%
;                                    [notify-callback
;                                     (lambda ()
;                                       (if (>  (hash-count train-speed) 0)
;                                           (let ((name (string->symbol (send train-tab get-item-label (send train-tab get-selection)))))
;                                             (train-adjust-cb name (hash-ref train-speed name)))
;                                           '()))
;                                     ]
;                                    [interval 5000]))
  
    (define (remove-current-train-elements!) ;; Removes the message and slider from the screen 
      (send display-message show #f)
      (send slider show #f))

    (define (show-current-train-elements!) ;; Shows the message and slider on the screen
      (send display-message show #t)
      (send slider show #t))

    (remove-current-train-elements!)

    (cond ((>= (length (train-retrieve-cb)) 1)
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
      (new group-box-panel%
           [parent main-tab-panel]
           [label "Switch overview"]
           ))

    ;; Horizontal grouping
    (define horizontal-grouping-panel
      (new horizontal-panel%
           [parent switch-panel]
           [vert-margin SWITCH-DATA-VERT-MARGIN]
           ))

    (define draw-adjust-panel
      (new group-box-panel%
           [parent horizontal-grouping-panel]
           [label "Adjust switch"]
           ))

    (define draw-get-panel
      (new group-box-panel%
           [parent horizontal-grouping-panel]
           [label "Get switch state"]
           ))

    
    ;; Draws the switch data related to the switch tab in the GUI
    (define (draw-switch-data!)
      (let ((switch-menu (new choice%
                              [label "Choose switch"]
                              [parent draw-adjust-panel]
                              [choices (data-converter (switch-retrieve-cb))]
                              [vert-margin SWITCH-CHOICE-WIDGET-VERT-MARGIN]
                              ))
            (message         (new message%
                                  [parent draw-get-panel]
                                  [vert-margin SWITCH-MESSAGE-WIDGET-VERT-MARGIN]
                                  [label "<State switch>"]
                                  )))
        (new radio-box%
             [label "Adjust current state"]
             [parent draw-adjust-panel]
             [callback (lambda (this event) (switch-adjust-cb (string->symbol (send switch-menu get-string-selection))
                                                              (+ (send this get-selection) 1)))]
             [choices (list "1" "2")]
             [selection #f]
             )

        (new button%
             [parent draw-get-panel]
             [label "Get state"]
             [callback (lambda (this event)
                         (let* ((switch-name (string->symbol (send switch-menu get-string-selection)))
                                (state-switch (cdr (assoc switch-name (switch-retrieve-cb)))))
                           (send message set-label (string-append "Switch state: " (number->string state-switch)))))]
             )
        ))
    (draw-switch-data!)
    
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
      (new group-box-panel%
           [parent main-tab-panel]
           [label "Barrier and lights overview"]
           ))

    ;; Panel on which the group boxes must be drawn
    (define draw-group-panel
      (new horizontal-panel%
           [parent barrier/light-panel]
           [vert-margin GROUP-BARRIER/LIGHT-PANEL-VERT-MARGIN]
           ))
         

    ;; Draw panel for barriers
    (define left-light-panel
      (new group-box-panel%
           [parent draw-group-panel]
           [label "Adjust lights"]
           ))

    ;; Draw panel for lights
    (define right-barrier-panel
      (new group-box-panel%
           [parent draw-group-panel]
           [label "Adjust barriers"]
           ))


    ;; Draw all the radioboxes for barriers
    (define (draw-all-barriers!)
      (for-each (lambda (barrier-pair)
                  (new radio-box%
                       [label (symbol->string (hardware-name barrier-pair))]
                       [parent right-barrier-panel]
                       [callback (lambda (this event)
                                   (barrier-adjust-cb (hardware-name barrier-pair) (send this get-selection)))
                                 ]
                       [choices (list "open"
                                      "closed")]
                       [vert-margin BARRIER/LIGHT-WIDGET-VERT-MARGIN]
                       [selection (if (hardware-data barrier-pair) 0 1)]))
                (barrier-retrieve-cb)))

    ;; Draws all the dropdown menus for barriers
    (define (draw-all-lights!)
      (for-each (lambda (light-pair)
                  (new choice%
                       [label (symbol->string (hardware-name light-pair))]
                       [parent left-light-panel]
                       [callback (lambda (this event)
                                   (let ((selected-item (send this get-string-selection)))
                                     (light-adjust-cb (hardware-name light-pair) (string->symbol selected-item))))]                            
                       [choices (list "Hp0" "Hp1" "Hp0+Sh0"
                                      "Ks1+Zs3" "Ks2" "Ks2+Zs3"
                                      "Sh1" "Ks1+Zs3+Zs3v")]
                       [vert-margin BARRIER/LIGHT-WIDGET-VERT-MARGIN]
                       [selection (let ((light-name (hardware-data light-pair)))
                                    (cond
                                      ((eq? light-name 'Hp0) 0)
                                      ((eq? light-name 'Hp1) 1)
                                      ((eq? light-name 'Hp0+Sh0) 2)
                                      ((eq? light-name 'Ks1+Zs3) 3)
                                      ((eq? light-name 'Ks2) 4)
                                      ((eq? light-name 'Ks2+Zs3) 5)
                                      ((eq? light-name 'Sh1) 6)
                                      ((eq? light-name 'Ks1+Zs3+Zs3v) 7)))]
                       ))
                (light-retrieve-cb)))


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
      (new group-box-panel%
           [parent main-tab-panel]
           [label "Detection block overview"]
           ))

    (define vertical-db-panel
      (new vertical-panel%
           [parent detection-block-panel]
           [vert-margin DETECTION-BLOCK-DATA-VERT-MARGIN]
           ))


    ;; Panel on which messages will be written
    (define message-panel '())

    ;; Helper procedure for conversion of data
    (define (convert-presence presence) 
      (if presence
          "Presence"
          "No presence"))
  
    ;; Draw detection-blocks
    (define (draw-detection-blocks!)
      (set! message-panel
            (new vertical-panel%
                 [parent vertical-db-panel]))
      (for-each (lambda (det-bl)
                  (new message%
                       [parent message-panel] 
                       [label (string-append (symbol->string (hardware-name det-bl))
                                             ": "
                                             (convert-presence (hardware-data det-bl)))]
                       ))
                (db-retrieve-cb)))
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

  (define (dispatch msg)
    "MAKE-GUI-ADT: Illegal message")
  dispatch)

  


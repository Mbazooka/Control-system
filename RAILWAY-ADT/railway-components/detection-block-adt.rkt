;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           detection-block ADT             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide make-detection-block-adt)

(define (make-detection-block-adt name)
  (let ((trains-present '()))

    (define (get-name) name)

    (define (get-presence train-name)
      (if (member train-name trains-present)
          #t
          #f))

    (define (add-train! train-name)
      (if (symbol? train-name)
          (set! trains-present  (cons train-name trains-present))
          "add-train!: illegal value"))

    (define (dispatch msg)
      (cond
        ((eq? msg 'get-name) get-name)
        ((eq? msg 'get-presence) get-presence)
        ((eq? msg 'add-train!) add-train!)
        (else
         "DETECTION-BLOCK-ADT: Incorrect message")))
    dispatch))

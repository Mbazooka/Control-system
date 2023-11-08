;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Infrabel ADT                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require 
  "../RAILWAY-ADT/railway-adt.rkt"
  "../simulator/interface.rkt")

(define (make-infrabel-adt)
  (let ((railway (make-railway-adt)))






    (define (dispatch msg)
      (cond
        ((eq? msg 'add-train!) add-train!)

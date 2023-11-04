#lang racket

(require graph)

(define railway-connections '((T-1-5 T-20) (T-1-1 T-28) (T-1-7 T-1-6) (T-1-7 T-28) 
                                           (T-26 T-27) (T-26 T-1-4) (T-28 T-26)
                                           (T-27 T-1-3) (T-27 T-1-2) (T-1-4 T-1-5)
                                           (T-1-8 T-25) 
                                           (T-25 T-1) (T-1 T-2-1) 
                                           (T-1 T-2) (T-5 T-1-6) (T-2 T-3)
                                           (T-2 T-7) (T-7 T-25) (T-3 T-8)
                                           (T-3 T-2-2) (T-7 T-5) (T-5 T-6)
                                           (T-6 T-2-3) (T-6 T-20) (T-8 T-4)
                                           (T-8 T-2-5) (T-4 T-2-7) (T-4 T-2-6)
                                           (T-20 T-2-4) (T-2-4 T-23)
                                           (T-23 T-12) (T-23 T-24) 
                                           (T-24 T-9) (T-9 T-11)
                                           (T-11 T-10) (T-10 T-1-1) (T-10 T-16)
                                           (T-16 T-2-8) (T-16 NO-TRACK) (T-24 T-1-3)
                                           (T-12 T-2-3) (T-1-2 T-9) 
                                           (T-11 T-12) (T-1-8 NO-TRACK) (T-2-1 NO-TRACK)
                                           (T-2-2 NO-TRACK) (T-2-5 NO-TRACK) (T-2-6 NO-TRACK)
                                           (T-2-7 NO-TRACK) (T-U-7 NO-TRACK)
                                           ))

(define  g (unweighted-graph/undirected railway-connections))







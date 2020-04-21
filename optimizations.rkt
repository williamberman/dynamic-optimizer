#lang racket

(require data/gvector
         "utils.rkt")

(provide get-optimization
         add-optimizer!)

(define optimizers (make-gvector))

(define (add-optimizer! optimizer)
  (gvector-add! optimizers optimizer))

(define (get-optimization call-graph function)
  (define the-optimized-function #f)
  (for ([an-optimizer optimizers])
    (awhen (and (not the-optimized-function)
                (an-optimizer call-graph function))
           (set! the-optimized-function it)))
  the-optimized-function)

#lang racket

(require data/gvector
         "utils.rkt")

(provide get-optimization
         enable-optimizer-plugin!
         reset-optimizer-plugins!)

(define optimizers (make-gvector))

(define (enable-optimizer-plugin! optimizer)
  (gvector-add! optimizers optimizer))

(define (reset-optimizer-plugins!) (set! optimizers (make-gvector)))

(define (get-optimization call-graph function)
  (define the-optimized-function #f)
  (for ([an-optimizer optimizers])
    (awhen (and (not the-optimized-function)
                (an-optimizer call-graph function))
           (set! the-optimized-function it)))
  the-optimized-function)

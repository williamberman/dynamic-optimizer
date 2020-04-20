#lang racket

(require "utils.rkt"
         "call-graph.rkt"
         "bottom-up-constant-space-procedure.rkt"
         "additional-properties.rkt")

(provide get-optimization)

(define (get-optimization call-graph function)
  (cond
    [(can-make-bottom-up-constant-space-procedure? call-graph)
     
     (begin
       (define body (make-bottom-up-constant-space-procedure
                     (call-graph->all-arguments-bottom-up call-graph)
                     (get-subproblem-combination-function
                      (property-ref function 'body))))

       (define the-optimized-function (eval `(lambda () ,@body)))
       (property-set! the-optimized-function 'body body)
       the-optimized-function)]
    [#t #f]))


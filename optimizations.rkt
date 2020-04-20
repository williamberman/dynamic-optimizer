#lang racket

(require "call-graph.rkt"
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

       (make-optimized-function-helper body))]
    [#t #f]))

(define (make-optimized-function-helper body)
  (define the-optimized-function-as-data
    `(lambda (,(gensym 'kws) ,(gensym 'kw-args) ,(gensym 'rest))
         ,@body))

  (define the-optimized-function (eval `(make-keyword-procedure ,the-optimized-function-as-data)))

  (property-set! the-optimized-function 'body the-optimized-function-as-data)

  the-optimized-function)

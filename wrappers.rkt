#lang racket

(require data/gvector)
(require graph)
(require "call-graph.rkt")
(require "utils.rkt")
(require "optimizer.rkt")
(require "optimizations.rkt")

(provide
 with-call-graph
 with-call-graph-save-and-display
 call-graphs
 with-optimizer
 optimized-procedure-optimizer)

;; TODO these need to be ported to the new advice system

(define (with-call-graph fn visit-completed-call-graph)
  (define call-graph-builder (make-call-graph-builder))
  
  (define (before #:args args)
    (call-graph-builder-pre-call call-graph-builder #:args args))
  
  (define (after #:args args #:return-value return-value)
    (call-graph-builder-post-call call-graph-builder #:args args #:return-value return-value)
    (when (call-graph-builder-is-complete? call-graph-builder)
      (visit-completed-call-graph (call-graph-builder-call-graph call-graph-builder))
      (set! call-graph-builder (make-call-graph-builder))))
  
  (around #:before before #:after after #:fn fn))

(define call-graphs (make-gvector))

(define (with-call-graph-save-and-display fn output-file-path)
  (define (visit-completed-call-graph call-graph)
    (gvector-add! call-graphs call-graph)
    (display-graph call-graph output-file-path))
  (with-call-graph fn visit-completed-call-graph))

(define (display-graph call-graph output-file-path)
  (call-with-output-file output-file-path
    (lambda (out)
      (display (graphviz call-graph) out))
    #:exists 'replace))

(struct optimized-procedure (procedure optimizer)
  #:property prop:procedure (struct-field-index procedure))

;; TODO name should be extracted from fn but fn is an annotated procedure
;; and thus obscures the original name of the object
(define (with-optimizer fn name)
  (define call-graph-builder (make-call-graph-builder))
  (define optimizer (make-optimizer fn name))
  
  (define (before #:args args)
    (call-graph-builder-pre-call call-graph-builder #:args args))
  
  (define (after #:args args #:return-value return-value)
    (call-graph-builder-post-call call-graph-builder #:args args #:return-value return-value)
    (when (call-graph-builder-is-complete? call-graph-builder)
      (define optimization (get-optimization
                            (call-graph-builder-call-graph call-graph-builder)
                            fn))
      (when optimization
        (optimizer-add-possible-optimization! optimizer args optimization))
      (set! call-graph-builder (make-call-graph-builder))))

  (define procedure (around #:before before #:after after #:fn optimizer))
  (optimized-procedure procedure optimizer))

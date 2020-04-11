#lang racket

(require "fib.rkt")
(require "utils.rkt")
(require "wrappers.rkt")
(require data/gvector)
(require "optimizations.rkt")

(define (test)
  (install! 'fib (with-call-graph-save-and-display fib "/tmp/call-graph.dot"))
  (fib 5)
  (define call-graph (gvector-ref call-graphs (- (gvector-count call-graphs) 1)))
  (define fib-5 (get-optimization call-graph fib))
  (uninstall! 'fib)
  fib-5)

#lang racket

(require "utils.rkt")
(require "wrappers.rkt")
(require data/gvector)
(require "optimizations.rkt")
(require "optimizer.rkt")
(require "optimizer-repl.rkt")
(require "additional-properties.rkt")

(provide test-1 test-2)

;; TODO define new install functions

(define (test-1)
  (install-call-graph! fib "/tmp/call-graph.dot")
  (fib 5)
  (define call-graph (gvector-ref call-graphs (- (gvector-count call-graphs) 1)))
  (define fib-5 (get-optimization call-graph fib))
  (reset-advice! fib)
  fib-5)

(define (test-2)
  (install-optimizer! fib)
  (fib 5)
  (fib 9)
  (fib 8)
  (fib 8)
  (define the-optimizer (property-ref fib 'optimizer))
  ((make-optimizer-repl the-optimizer)))

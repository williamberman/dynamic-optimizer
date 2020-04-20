#lang racket

(require "fib.rkt"
         "call-graph.rkt"
         "optimizer.rkt"
         "additional-properties.rkt"
         "optimizer-repl.rkt")

(define (test-1)
  (install-call-graph! fib (lambda (call-graph) (save-and-display-call-graph
                                                 call-graph
                                                 "/tmp/call-graph.dot")))
  (fib 5)
  (uninstall-call-graph! fib))

(define (test-2)
  (install-optimizer! fib)
  (fib 5)
  (fib 9)
  (fib 8)
  (fib 8)
  (uninstall-optimizer! fib))

;; TODO use optimizer repl
(define (test-3)
  (install-optimizer! fib)
  (fib 5)
  (fib 9)
  (fib 8)
  (fib 8)
  ((make-optimizer-repl fib (property-ref fib 'optimizer) "fib"))
  (uninstall-optimizer! fib))

#lang racket

(require "utils.rkt"
         "fib.rkt"
         "call-graph.rkt"
         "optimizer.rkt")

(install-call-graph! fib (lambda (call-graph) (pretty-print (graph->tree call-graph))))

;; (require data/gvector)
;; (require "optimizations.rkt")
;; (require "optimizer.rkt")
;; (require "optimizer-repl.rkt")
;; (require "additional-properties.rkt")

;; TODO define new install functions

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
  (fib 8))

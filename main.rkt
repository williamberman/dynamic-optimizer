#lang racket

(require "utils.rkt")
(require "wrappers.rkt")
(require data/gvector)
(require "optimizations.rkt")

;; TODO I'd like to be able to wrap a procedure located in its own module
(define fib
  (annotated-lambda (n)
                    (cond ((= n 0) 0)
                          ((= n 1) 1)
                          (#t (+ (fib (- n 1)) (fib (- n 2)))))))

(define (test)
  (install! 'fib (with-call-graph-save-and-display fib "/tmp/call-graph.dot"))
  (fib 5)
  (define call-graph (gvector-ref call-graphs (- (gvector-count call-graphs) 1)))
  (define fib-5 (get-optimization call-graph fib))
  (uninstall! 'fib)
  fib-5)

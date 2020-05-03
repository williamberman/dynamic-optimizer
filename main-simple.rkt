#lang racket

(require "optimizer.rkt"
         "advice.rkt"
         "additional-properties.rkt"
         "call-graph.rkt"
         "optimizations.rkt"
         "optimizer-plugins/bottom-up-constant-space-procedure.rkt"
         "optimizer-repl.rkt")

;; To convert the outputted dot files
;; fswatch -0 /tmp/call-graph.dot | \
;;        xargs -0 -I \{\} dot -Tpng -o/tmp/call-graph.png /tmp/call-graph.dot
;; fswatch -0 /tmp/call-graph-transpose.dot | \
;;        xargs -0 -I \{\} dot -Tpng -o/tmp/call-graph-transpose.png /tmp/call-graph-transpose.dot

;; Explain
;; - applications vs atoms
;; - prefix notation
;; - define
;; - Condition operator
;; - for loop
;; - Fibonacci sequence

;; F_0 = 0
;; F_1 = 1
;; F_n = F_(n-1) + F_(n-2)
(define/optimizable (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (#t (+ (fib (- n 1))
               (fib (- n 2))))))

(define (demonstrate-call-graph)
  (install-call-graph! fib display-call-graph-and-transpose)
  (fib 5)
  (fib 6)
  (uninstall-call-graph! fib))

(define (demonstrate-optimized-procedure)
  (enable-optimizer-plugin! make-bottom-up-constant-space-procedure)
  (install-optimizer! fib)
  (fib 5)
  (fib 9)
  (fib 8)
  ((make-optimizer-repl fib))
  (uninstall-optimizer! fib)
  (reset-optimizer-plugins!))

(define (simplified-fib-5) 
  (define previous-2 0)
  (define previous-1 1)
  (for ([computing-for '((2) (3) (4) (5))])
    (define cur (+ previous-1 previous-2))
    (set! previous-2 previous-1)
    (set! previous-1 cur))
  previous-1)

#lang racket

(require "./functions-to-optimize/fib.rkt"
         "./functions-to-optimize/maximal-square.rkt"
         "call-graph.rkt"
         "optimizer.rkt"
         "optimizer-repl.rkt"
         "optimizer-plugins/bottom-up-constant-space-procedure.rkt"
         "optimizer-plugins/bottom-up-non-constant-space-procedure.rkt"
         "optimizations.rkt"
         graph)

;; To convert the outputted dot files
;; fswatch -0 /tmp/call-graph.dot | \
;;        xargs -0 -I \{\} dot -Tpng -o/tmp/call-graph.png /tmp/call-graph.dot
;; fswatch -0 /tmp/call-graph-transpose.dot | \
;;        xargs -0 -I \{\} dot -Tpng -o/tmp/call-graph-transpose.png /tmp/call-graph-transpose.dot
(define (x-display-call-graph call-graph)
  (display-call-graph call-graph
                      "/tmp/call-graph.dot")
  (display-call-graph (transpose call-graph)
                      "/tmp/call-graph-transpose.dot"))

(define (example-1)
  (install-call-graph! fib x-display-call-graph)
  (fib 5)
  (fib 6)
  (uninstall-call-graph! fib))

(define (example-2)
  (enable-optimizer-plugin! make-bottom-up-constant-space-procedure)
  (install-optimizer! fib)
  (fib 5)
  (fib 9)
  (fib 8)
  ((make-optimizer-repl fib))
  (uninstall-optimizer! fib)
  (reset-optimizer-plugins!))

(define (example-3)
  (enable-optimizer-plugin! make-bottom-up-constant-space-procedure)
  (install-optimizer! fib-3-back)
  (fib-3-back 5)
  (fib-3-back 9)
  (fib-3-back 8)
  ((make-optimizer-repl fib-3-back))
  (uninstall-optimizer! fib-3-back)
  (reset-optimizer-plugins!))

(define (example-4)
  (install-call-graph! fib-not-constant-space x-display-call-graph)
  (fib-not-constant-space 5)
  (fib-not-constant-space 6)
  (uninstall-call-graph! fib-not-constant-space))

(define (example-5)
  (enable-optimizer-plugin! make-bottom-up-constant-space-procedure)
  (install-optimizer! fib-not-constant-space)
  (fib-not-constant-space 5)
  (fib-not-constant-space 9)
  (fib-not-constant-space 8)
  ((make-optimizer-repl fib-not-constant-space))
  (enable-optimizer-plugin! make-bottom-up-non-constant-space-procedure)
  (fib-not-constant-space 5)
  (fib-not-constant-space 9)
  (fib-not-constant-space 8)
  ((make-optimizer-repl fib-not-constant-space))
  (uninstall-optimizer! fib-not-constant-space)
  (reset-optimizer-plugins!))

(define (example-6)
  (install-call-graph! maximal-square x-display-call-graph)
  (parameterize ([maximal-square-matrix example-matrix-2])
    (maximal-square 3 4))
  (uninstall-call-graph! maximal-square))

(define (example-7)
  (enable-optimizer-plugin! make-bottom-up-constant-space-procedure)
  (enable-optimizer-plugin! make-bottom-up-non-constant-space-procedure)
  (install-optimizer! maximal-square)
  (parameterize ([maximal-square-matrix example-matrix-2])
    (maximal-square 3 4)
    ((make-optimizer-repl maximal-square)))
  (uninstall-optimizer! maximal-square)
  (reset-optimizer-plugins!))

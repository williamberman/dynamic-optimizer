#lang racket

(require "./functions-to-optimize/fib.rkt"
         "./functions-to-optimize/maximal-square.rkt"
         "call-graph.rkt"
         "optimizer.rkt"
         "additional-properties.rkt"
         "optimizer-repl.rkt"
         "optimizers/bottom-up-constant-space-procedure.rkt"
         "optimizers/bottom-up-non-constant-space-procedure.rkt"
         "optimizations.rkt")



;; Command to update the call graph picture on changed call graph
;; TODO the fswatch part is not working
;; fswatch -0 /tmp/call-graph.dot | xargs -0 -I {} dot -Tpng -o/tmp/call-graph.png /tmp/call-graph.dot
(define (x-save-and-display-call-graph call-graph) (save-and-display-call-graph
                                                    call-graph
                                                    "/tmp/call-graph.dot"))

(define (init)
  (add-optimizer! make-bottom-up-constant-space-procedure)
  (add-optimizer! make-bottom-up-non-constant-space-procedure))

(define (example-1)
  (install-call-graph! fib x-save-and-display-call-graph)
  (fib 5)
  (fib 6)
  (uninstall-call-graph! fib))

(define (example-2)
  (install-optimizer! fib)
  (fib 5)
  (fib 9)
  (fib 8)
  (fib 8)
  ((make-optimizer-repl fib (property-ref fib 'optimizer) "fib"))
  (uninstall-optimizer! fib))

(define (example-3)
  (install-optimizer! fib-3-back)
  (fib-3-back 5)
  (fib-3-back 9)
  (fib-3-back 8)
  (fib-3-back 8)
  ((make-optimizer-repl fib-3-back (property-ref fib-3-back 'optimizer) "fib-3-back"))
  (uninstall-optimizer! fib-3-back))

(define (example-4)
  (install-call-graph! fib-not-constant-space x-save-and-display-call-graph)
  (fib-not-constant-space 5)
  (fib-not-constant-space 6)
  (uninstall-call-graph! fib-not-constant-space))

(define (example-5)
  (install-optimizer! fib-not-constant-space)
  (fib-not-constant-space 5)
  (fib-not-constant-space 9)
  (fib-not-constant-space 8)
  (fib-not-constant-space 8)
  ((make-optimizer-repl fib-not-constant-space
                        (property-ref fib-not-constant-space 'optimizer)
                        "fib-not-constant-space"))
  (uninstall-optimizer! fib-not-constant-space))

(define (example-6)
  (install-call-graph! maximal-square x-save-and-display-call-graph)
  (parameterize ([maximal-square-matrix example-matrix-2])
    (maximal-square 3 4))
  (uninstall-call-graph! maximal-square))

(define (example-7)
  (install-optimizer! maximal-square)
  (parameterize ([maximal-square-matrix example-matrix-2])
    (maximal-square 3 4))
  ((make-optimizer-repl maximal-square
                        (property-ref maximal-square 'optimizer)
                        "maximal-square"))
  (uninstall-optimizer! maximal-square))

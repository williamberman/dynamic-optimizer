#lang racket

(require "fib.rkt"
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

(add-optimizer! make-bottom-up-constant-space-procedure)

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
  (install-optimizer! fib-not-constant-space)
  (fib-not-constant-space 5)
  (fib-not-constant-space 9)
  (fib-not-constant-space 8)
  (fib-not-constant-space 8)
  ((make-optimizer-repl fib-not-constant-space
                        (property-ref fib-not-constant-space 'optimizer)
                        "fib-not-constant-space"))
  (uninstall-optimizer! fib-not-constant-space))

(define (example-5)
  (install-call-graph! fib-not-constant-space x-save-and-display-call-graph)
  (fib-not-constant-space 5)
  (fib-not-constant-space 6)
  (uninstall-call-graph! fib-not-constant-space))

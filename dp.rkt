#lang racket

(require racket/match)
(require typed-stack)
(require graph)
(require data/gvector)
(require racket/function)
(require racket/stream)
(require racket/list)
(require racket/set)
(require racket/contract)




(define (test)
  (install! 'fib (with-call-graph% fib))
  (fib 5)
  (define g (gvector-ref call-graphs (- (gvector-count call-graphs) 1)))
  (uninstall! 'fib)
  (define fib-5
    ;; TODO need to find + symbol from the actual body of the function
    ;; TODO need to know if this optimization is actually appropriate
    (eval `(annotated-lambda
            ()
            ,@(make-bottom-up-constant-space-procedure (call-graph->all-arguments g) '+))))
  fib-5)

(define (test%)
  (install! 'fib (with-optimizer fib))
  (fib 5)
  (uninstall! 'fib))


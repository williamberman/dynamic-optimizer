#lang racket

(require "utils.rkt")
(require "call-graph.rkt")
(require "bottom-up-constant-space-procedure.rkt")

(provide get-optimization)

(define (get-optimization call-graph annotated-procedure)
  (cond
    ;; TODO fix the following
    ;; [(can-make-bottom-up-constant-space-procedure? call-graph)
    ;;  (eval `(annotated-lambda () ,@(make-bottom-up-constant-space-procedure
    ;;                                 (call-graph->all-arguments-bottom-up call-graph)
    ;;                                 (get-subproblem-combination-function annotated-procedure))))]
    [#t #f]))

#lang racket

(require "../additional-properties.rkt"
         "../call-graph.rkt"
         "common.rkt"
         graph)

(provide make-bottom-up-non-constant-space-procedure)

(define (make-bottom-up-non-constant-space-procedure call-graph function)
  (if (can-make-procedure? call-graph)
      (make-optimized-function-helper (make-body call-graph
                                                 (call-graph->all-arguments-bottom-up call-graph)
                                                 (get-subproblem-combination-function
                                                  (property-ref function 'body)
                                                  (property-ref function 'function-identifier)
                                                  (property-ref function 'function-signature)
                                                  #:variable-number-of-arguments #t)))
      #f))

(define (can-make-procedure? call-graph)
  (and (dag? call-graph)
       (looks-like-function? call-graph)))

(define (make-body call-graph all-arguments update-function)
  (match-let ([(list initial-values need-to-calculate)
               (split-arguments-into-initial-values-and-need-to-calculate all-arguments)])
    `((define results (make-hash ',(make-results-hash-initial-values initial-values)))
      (for ([dependent-on ',(make-dependent-on call-graph need-to-calculate)]
            [computing-for ',(map a-call-arguments need-to-calculate)])
        (define computed
          (keyword-apply ,update-function
                         '(#:currently-computing)
                         (list computing-for)
                         (map (lambda (requirement) (hash-ref results requirement))
                              dependent-on)))
        (hash-set! results computing-for computed))
      (hash-ref results ',(make-last-arguments initial-values need-to-calculate)))))

(define (make-results-hash-initial-values initial-values)
  (map (lambda (a-call)
         (cons (a-call-arguments a-call) (a-call-return-value a-call)))
       initial-values))

(define (make-dependent-on call-graph need-to-calculate)
  (map (lambda (a-call) (map (lambda (a-neighbor) (a-call-arguments a-neighbor))
                             (get-neighbors call-graph a-call)))
       need-to-calculate))

(define (make-last-arguments initial-values need-to-calculate)
  (a-call-arguments (if (empty? need-to-calculate)
                        (last initial-values)
                        (last need-to-calculate))))


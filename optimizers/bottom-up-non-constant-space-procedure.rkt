#lang racket

(require "../additional-properties.rkt"
         "../call-graph.rkt"
         "common.rkt")

(provide make-bottom-up-non-constant-space-procedure)

(define (make-bottom-up-non-constant-space-procedure call-graph function)
  (define body (make-body
                (call-graph->all-arguments-bottom-up call-graph)
                (get-subproblem-combination-function
                 (property-ref function 'body)
                 (property-ref function 'function-identifier))))
  (make-optimized-function-helper body))

;; TODO the all-arguments argument is likely structured differently
(define (make-body all-arguments update-function)
  (match-let ([(list initial-values need-to-calculate)
               (split-arguments-into-initial-values-and-need-to-calculate all-arguments)])
    `((define results (make-hash ,initial-values))
      (for ([dependent-on '()]   ;; TODO
            [computing-for '()]) ;; TODO
        (define computed
          (keyword-apply ,update-function
                         '(#:currently-computing)
                         (list computing-for)
                         (map (lambda (requirement) (hash-ref results requirement))
                              dependent-on)))
        (hash-set! results computing-for computed))
      (hash-ref results _)))) ;; TODO

(define (optimized-non-constant-space)
  (define results (make-hash '((1 . 1))))
  (for ([dependent-on '((1) (1 2) (3) (3 4))]
        [computing-for '(2 3 4 5)])

    (define computed
      (keyword-apply (lambda (a (b #f) #:currently-computing currently-computing)
                       (if (even? currently-computing)
                           a
                           (+ a b)))
                     '(#:currently-computing)
                     (list computing-for)
                     (map (lambda (requirement) (hash-ref results requirement))
                          dependent-on)))

    (hash-set! results computing-for computed))
  (hash-ref results 5))

;; (define (optimized-non-constant-space)
;;   (define results (make-hash '((1 . 1))))
;;   (for ([dependent-on '((1) (1 2) (3) (3 4))]
;;         [computing-for '(2 3 4 5)])

;;     (define computed
;;       (keyword-apply (lambda (a (b #f) #:currently-computing currently-computing)
;;                        (if (even? currently-computing)
;;                            a
;;                            (+ a b)))
;;                      '(#:currently-computing)
;;                      (list computing-for)
;;                      (map (lambda (requirement) (hash-ref results requirement))
;;                           dependent-on)))

;;     (hash-set! results computing-for computed))
;;   (hash-ref results 5))

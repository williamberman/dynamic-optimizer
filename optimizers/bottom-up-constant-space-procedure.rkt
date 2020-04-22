#lang racket

(require "../call-graph.rkt"
         "common.rkt"
         "../additional-properties.rkt"
         graph)

(provide make-bottom-up-constant-space-procedure)

(define (make-bottom-up-constant-space-procedure call-graph function)
  (if (can-make-procedure? call-graph)
      (make-optimized-function-helper (make-body (call-graph->all-arguments-bottom-up call-graph)
                                                 (get-subproblem-combination-function
                                                  (property-ref function 'body)
                                                  (property-ref function 'function-identifier)
                                                  (property-ref function 'function-signature))))
      #f))

(define (make-body all-arguments update-function)
  (match-let ([(list initial-values need-to-calculate)
               (split-arguments-into-initial-values-and-need-to-calculate all-arguments)])
    `(,@(make-definitions initial-values)
      (for ([i (quote ,(map a-call-arguments
                            need-to-calculate))])
        (define cur ,(make-current-update update-function (length initial-values)))
        ,@(make-definition-updates (length initial-values)))
      ,(number->definition-symbol 1))))

(define (can-make-procedure? call-graph)
  (and (dag? call-graph)
       (looks-like-function? call-graph)
       (uses-constant-space? call-graph)))

(define (make-definitions initial-values)
  (define counter (length initial-values))
  (map
   (lambda (the-call)
     (define return-value
       `(define ,(number->definition-symbol counter) ,(a-call-return-value the-call)))
     (set! counter (- counter 1))
     return-value)
   initial-values))

(define (make-current-update update-function number-definitions)
  `(,update-function
    ,@(stream->list
       (stream-map
        number->definition-symbol
        (in-range 1 (+ 1 number-definitions))))))

(define (make-definition-updates number-definitions)
  (reverse
   (cons `(set! ,(number->definition-symbol 1) cur)
         (stream->list
          (stream-map
           (lambda (definition-number)
             `(set! ,(number->definition-symbol definition-number)
                    ,(number->definition-symbol (- definition-number 1))))
           (in-range 2 (+ 1 number-definitions)))))))

(define (number->definition-symbol definition-number)
  (string->symbol (string-append "previous-" (number->string definition-number))))

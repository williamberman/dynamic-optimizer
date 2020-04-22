#lang racket

(require (for-syntax racket/syntax syntax/parse))

(provide define/optimizable)

(define-syntax (define/optimizable stx)
  (syntax-parse stx
    [(_ (optimizable-identifier:id arg:id ...) body:expr)
     (datum->syntax stx
                    `(begin
                       (require "../additional-properties.rkt"
                                "../capture-literals.rkt"
                                "../advice.rkt")
                       (define (,#'optimizable-identifier ,@#'(arg ...))
                         ,#'body)
                       (set! ,#'optimizable-identifier
                             (make-receptive-function ,#'optimizable-identifier))
                       (property-set! ,#'optimizable-identifier
                                      'body
                                      ,#''body)
                       (property-set! ,#'optimizable-identifier
                                      'function-identifier
                                      ,#''optimizable-identifier)
                       (property-set! ,#'optimizable-identifier
                                      'function-signature
                                      ,#''(arg ...))))]))

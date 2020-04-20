#lang racket

(require "call-graph.rkt"
         "optimizations.rkt"
         "additional-properties.rkt"
         "advice.rkt")

(provide make-optimizer
         optimizer-add-possible-optimization!
         optimizer-enable-optimization!
         optimizer-disable-optimization!
         optimizer-get-disabled-optimizations
         optimizer-get-optimization
         optimizer-optimization-is-enabled?
         optimizer-optimization-is-disabled?
         install-optimizer!)

(struct available-optimization (optimization [state #:mutable]))

(define disabled 'disabled)
(define enabled 'enabled)

(define (make-optimizer) (make-hash))

;; TODO this assumes there can only be one optimization at any given time
(define (optimizer-add-possible-optimization! optimizer args optimization)
  (hash-set! optimizer args (available-optimization optimization disabled)))

(define (optimizer-enable-optimization! optimizer args)
  (set-available-optimization-state! (hash-ref! optimizer args) enabled))

(define (optimizer-disable-optimization! optimizer args)
  (set-available-optimization-state! (hash-ref! optimizer args) disabled))

(define (optimizer-get-disabled-optimizations optimizer)
  (filter (lambda (args) (eq? disabled
                              (available-optimization-state (hash-ref optimizer args))))
          (hash-keys optimizer)))

(define (optimizer-get-optimization optimizer args)
  (available-optimization-optimization (hash-ref optimizer args)))

(define (optimizer-optimization-is-enabled? optimizer args)
  (eq? enabled
       (available-optimization-state (hash-ref optimizer args))))

(define (optimizer-optimization-is-disabled? optimizer args)
  (eq? disabled
       (available-optimization-state (hash-ref optimizer args))))

(define (install-optimizer! receptive-function)
  (define call-graph-builder (make-call-graph-builder))
  (define optimizer (make-optimizer))

  ;; TODO somewhere in here we need to delegate to the optimization
  (define around
    (make-keyword-procedure
     (lambda (kws kw-args the-function . args)
       (call-graph-builder-pre-call call-graph-builder
                                    #:args (list kws kw-args args))

       (define the-return-value (keyword-apply the-function kws kw-args args))

       (call-graph-builder-post-call call-graph-builder
                                     #:args (list kws kw-args args)
                                     #:return-value the-return-value)

       (when (call-graph-builder-is-complete? call-graph-builder)
         (define optimization (get-optimization
                               (call-graph-builder-call-graph call-graph-builder)
                               receptive-function))
         (when optimization
           (optimizer-add-possible-optimization! optimizer
                                                 (list kws kw-args args)
                                                 optimization))
         (set! call-graph-builder (make-call-graph-builder)))

       the-return-value
       )))

  (property-set! receptive-function 'optimizer optimizer)
  (add-function 'around receptive-function around))

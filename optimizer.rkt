#lang racket

(require "call-graph.rkt"
         "optimizations.rkt"
         "additional-properties.rkt"
         "advice.rkt"
         "utils.rkt")

(provide make-optimizer
         optimizer-add-possible-optimization!
         optimizer-enable-optimization!
         optimizer-disable-optimization!
         optimizer-get-disabled-optimizations
         optimizer-get-optimization
         optimizer-optimization-is-enabled?
         optimizer-optimization-is-disabled?
         install-optimizer!
         uninstall-optimizer!)

(struct available-optimization (optimization [state #:mutable]))

(define disabled 'disabled)
(define enabled 'enabled)

(define (make-optimizer) (make-hash))

;; TODO this assumes there can only be one optimization at any given time
(define (optimizer-add-possible-optimization! optimizer args optimization)
  (hash-set! optimizer args (available-optimization optimization disabled)))

(define (optimizer-enable-optimization! optimizer args)
  (set-available-optimization-state! (hash-ref optimizer args) enabled))

(define (optimizer-disable-optimization! optimizer args)
  (set-available-optimization-state! (hash-ref optimizer args) disabled))

(define (optimizer-get-disabled-optimizations optimizer)
  (filter (lambda (args) (eq? disabled
                              (available-optimization-state (hash-ref optimizer args))))
          (hash-keys optimizer)))

(define (optimizer-get-optimization optimizer args)
  (available-optimization-optimization (hash-ref optimizer args)))

(define (optimizer-has-optimization? optimizer args)
  (hash-has-key? optimizer args))

(define (optimizer-optimization-is-enabled? optimizer args)
  (eq? enabled
       (available-optimization-state (hash-ref optimizer args))))

(define (optimizer-optimization-is-disabled? optimizer args)
  (eq? disabled
       (available-optimization-state (hash-ref optimizer args))))

(define (install-optimizer! receptive-function)
  (define call-graph-builder #f)
  (define optimizer (make-optimizer))

  (define around
    (make-keyword-procedure
     (lambda (kws kw-args the-function . args)
       (define the-argument-list (make-argument-list kws kw-args args))
       
       (if (and (not call-graph-builder )
                (optimizer-has-optimization? optimizer the-argument-list)
                (optimizer-optimization-is-enabled? optimizer the-argument-list))
           (keyword-apply (optimizer-get-optimization optimizer the-argument-list)
                          kws
                          kw-args
                          args)
           (begin
             (when (not call-graph-builder)
               (set! call-graph-builder (make-call-graph-builder)))
             
             (call-graph-builder-pre-call call-graph-builder
                                          #:args the-argument-list)

             (let ([the-return-value (keyword-apply the-function kws kw-args args)])
               
               (call-graph-builder-post-call call-graph-builder
                                             #:args the-argument-list
                                             #:return-value the-return-value)

               (when (call-graph-builder-is-complete? call-graph-builder)
                 (define optimization (get-optimization
                                       (call-graph-builder-call-graph call-graph-builder)
                                       receptive-function))
                 (when optimization
                   (optimizer-add-possible-optimization! optimizer
                                                         the-argument-list
                                                         optimization))
                 (set! call-graph-builder #f))

               the-return-value))))))

  (property-set! receptive-function 'optimizer optimizer)
  (add-function 'around receptive-function around))

(define (uninstall-optimizer! receptive-function)
  (property-remove! receptive-function 'optimizer)
  (remove-function 'around receptive-function))

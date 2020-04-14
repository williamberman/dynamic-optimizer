#lang racket

(require "utils.rkt")

(provide make-optimizer
         optimizer-add-possible-optimization!
         optimizer-enable-optimization!
         optimizer-disable-optimization!
         optimizer-possible-optimizations
         optimizer-get-available-optimizations
         optimizer-get-optimization
         optimizer-name
         optimizer-optimization-is-enabled?
         optimizer-optimization-is-disabled?)

;; TODO this name property is weird
;; TODO The dual hash tables thing should be changed
(struct optimizer (optimizations possible-optimizations fn name)
  #:property prop:procedure (struct-field-index fn))

(define make-optimizer
  (lambda (fn [name (object-name fn)])
   (define optimizations (make-hash))
   (define possible-optimizations (make-hash))
   ;; TODO does this handle keyword arguments?
   ;; make-keyword-procedure, keyword-apply
   (define (wrapper . args)
     ;; TODO check optimizations
     (apply fn args))
   (optimizer optimizations possible-optimizations wrapper (symbol->string name))))

;; TODO this assumes there can only be one optimization at any given time
(define (optimizer-add-possible-optimization! optimizer args optimization)
  (hash-set! (optimizer-possible-optimizations optimizer) args optimization))

(define (optimizer-enable-optimization! optimizer args)
  (define the-optimization (hash-ref (optimizer-possible-optimizations optimizer) args))
  (hash-set! (optimizer-optimizations optimizer) args the-optimization)
  (hash-remove! (optimizer-possible-optimizations optimizer) args))

(define (optimizer-disable-optimization! optimizer args)
  (define the-optimization (hash-ref (optimizer-optimizations optimizer) args))
  (hash-remove! (optimizer-optimizations optimizer) args)
  (hash-set! (optimizer-possible-optimizations optimizer) args the-optimization))

(define (optimizer-get-available-optimizations optimizer)
  (hash-keys (optimizer-possible-optimizations optimizer)))

(define (optimizer-get-optimization optimizer args)
  (annotated-procedure-body (hash-ref (optimizer-possible-optimizations optimizer) args)))

(define (optimizer-optimization-is-enabled? optimizer args)
  (hash-has-key? (optimizer-optimizations optimizer) args))

(define (optimizer-optimization-is-disabled? optimizer args)
  (hash-has-key? (optimizer-possible-optimizations optimizer) args))

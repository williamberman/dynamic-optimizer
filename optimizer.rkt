#lang racket

(provide
 make-optimizer
 optimizer-add-possible-optimization!
 optimizer-enable-optimization!
 optimizer-disable-optimization!)

(struct optimizer (optimizations possible-optimizations fn)
  #:property prop:procedure (struct-field-index fn))

(define (make-optimizer fn)
  (define optimizations (make-hash))
  (define possible-optimizations (make-hash))
  ;; TODO does this handle keyword arguments?
  ;; make-keyword-procedure, keyword-apply
  (define (wrapper . args)
    ;; TODO check optimizations
    (apply fn args))
  (optimizer optimizations possible-optimizations wrapper))

;; TODO this assumes there can only be one optimization at any given time
(define (optimizer-add-possible-optimization! optimizer args optimization)
  (hash-set! (optimizer-possible-optimizations optimizer) args optimization))

(define (optimizer-enable-optimization! optimizer args)
  (define the-optimization (hash-ref (optimizer-possible-optimizations optimizer) args))
  (hash-set! (optimizer-optimizations optimizer) args the-optimization))

(define (optimizer-disable-optimization! optimizer args)
  (hash-remove! (optimizer-optimizations optimizer) args))


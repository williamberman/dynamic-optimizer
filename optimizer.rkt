#lang racket

(require "utils.rkt")

(provide
 make-optimizer
 optimizer-add-possible-optimization!
 optimizer-enable-optimization!
 optimizer-disable-optimization!
 optimizer-possible-optimizations
 query-optimizer)

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
  (hash-set! (optimizer-optimizations optimizer) args the-optimization)
  (hash-remove! (optimizer-possible-optimizations optimizer) args))

(define (optimizer-disable-optimization! optimizer args)
  (define the-optimization (hash-ref (optimizer-optimizations optimizer) args))
  (hash-remove! (optimizer-optimizations optimizer) args)
  (hash-set! (optimizer-possible-optimizations optimizer) args the-optimization))

(define (optimizer-get-available-optimizations optimizer)
  (hash-keys (optimizer-possible-optimizations optimizer)))

(define (optimizer-get-optimization optimizer)
  (annotated-procedure-body (hash-ref (optimizer-possible-optimizations optimizer))))

;; These should be available as a set of syntax rules
;; all other standard racket should be available at the
;; same time

;; enable args
;; disable args
;; call args
;; available-optimizations
;; view-optimization args
;; the-optimizer

;; Define an interactive repl for playing with the optimizer
(define (query-optimizer optimizer)
  ;;TODO
  optimizer)

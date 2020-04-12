#lang racket

(require "utils.rkt")

(provide
 make-optimizer
 optimizer-add-possible-optimization!
 optimizer-enable-optimization!
 optimizer-disable-optimization!
 optimizer-possible-optimizations
 optimizer-querier-repl)

;; TODO this name property is weird
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

(define (optimizer-querier-repl optimizer)
  (define namespace (make-optimizer-querier-namespace optimizer))
  (define (the-eval input)
    (eval input namespace))
  (define (loop)
    (define input (optimizer-querier-read optimizer))
    (if (equal? input '(quit))
        (displayln "Goodbye")
        (begin 
          (displayln (the-eval input))
          (loop))))
  (displayln (the-eval '(help)))
  (loop))

(define (make-optimizer-querier-namespace optimizer)
  (define (enable . args)
    (optimizer-enable-optimization! optimizer args))

  (define (disable . args)
    (optimizer-disable-optimization! optimizer args))

  (define (call . args)
    (apply optimizer args))

  (define (available-optimizations)
    (optimizer-get-available-optimizations optimizer))

  ;; TODO this needs to be formatted
  (define (view-optimization . args)
    (optimizer-get-optimization optimizer args))

  (define (help)
    "TODO: Insert standard help message here")
  
  (define namespace (make-base-namespace))

  (define (add-to-namespace symbol value)
    (namespace-set-variable-value! symbol value #t namespace #t))

  (add-to-namespace 'enable enable)
  (add-to-namespace 'disable disable)
  (add-to-namespace 'call call)
  (add-to-namespace 'available-optimizations available-optimizations)
  (add-to-namespace 'view-optimization view-optimization)
  (add-to-namespace 'help help)
  
  namespace)

(define (optimizer-querier-read optimizer)
  (display (string-append "optimizer:" (optimizer-name optimizer) "> "))
  (define input (read))
  input)

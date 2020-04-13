#lang racket

(require "optimizer.rkt")

;; TODO I'd like to not have to provide all symbols, and instead only have
;; to provide make-optimizer-repl
(provide make-optimizer-repl
         enable disable
         call
         available-optimizations
         view-optimization
         help)

;; TODO, on exit this should log the delta
(define (make-optimizer-repl optimizer)
  (define (loop)
    (define input (optimizer-repl-read optimizer))
    (if (equal? input '(quit))
        (displayln "Goodbye")
        (begin 
          (displayln (eval input))
          (loop))))
  
  (lambda ()
    (parameterize ([the-optimizer optimizer])
      (displayln (help))
      (loop))))

(define (optimizer-repl-read optimizer)
  (display (string-append "optimizer:" (optimizer-name optimizer) "> "))
  (define input (read))
  input)

;; TODO wrap these in some syntax that ensures optimizer is not false

(define the-optimizer (make-parameter #f))

(define (enable . args)
  (optimizer-enable-optimization! (the-optimizer) args))

(define (disable . args)
  (optimizer-disable-optimization! (the-optimizer) args))

(define (call . args)
  (apply (the-optimizer) args))

(define (available-optimizations)
  (optimizer-get-available-optimizations (the-optimizer)))

;; TODO this needs to be formatted
(define (view-optimization . args)
  (optimizer-get-optimization (the-optimizer) args))

(define (help)
  "TODO: Insert standard help message here")

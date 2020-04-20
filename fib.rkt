#lang racket

(require "additional-properties.rkt")
(require "capture-literals.rkt")
(require "advice.rkt")

(provide fib fib-3-back fib-not-constant-space)

;; TODO it would be good to extract this definition pattern into a
;; macro

(with-literal
  fib-body
  (define (fib n)
    (a-literal fib-body (cond ((= n 0) 0)
                              ((= n 1) 1)
                              (#t (+ (fib (- n 1))
                                     (fib (- n 2))))))))

(with-literal
  fib-3-back-body
  (define (fib-3-back n)
    (a-literal fib-3-back-body (cond ((= n 0) 0)
                                     ((= n 1) 1)
                                     ((= n 2) 2)
                                     (#t (+ (fib-3-back (- n 1))
                                            (fib-3-back (- n 2))
                                            (fib-3-back (- n 3))))))))

(with-literal
  fib-not-constant-space-body
  (define (fib-not-constant-space n)
    (a-literal fib-not-constant-space-body (cond ((= n 0) 0)
                                                 ((= n 1) 1)
                                                 ((even? n) (fib-not-constant-space (- n 1)))
                                                 (#t (+ (fib-not-constant-space (- n 1))
                                                        (fib-not-constant-space (- n 2))))))))

(set! fib (make-receptive-function fib))
(set! fib-3-back (make-receptive-function fib-3-back))
(set! fib-not-constant-space (make-receptive-function fib-not-constant-space))

(property-set! fib 'body fib-body)
(property-set! fib-3-back 'body fib-3-back-body)
(property-set! fib-not-constant-space 'body fib-not-constant-space-body)

(property-set! fib 'function-identifier 'fib)
(property-set! fib-3-back 'function-identifier 'fib-3-back)
(property-set! fib-not-constant-space 'function-identifier 'fib-not-constant-space)

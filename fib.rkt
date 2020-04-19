#lang racket

(require racket/contract)
(require "additional-properties.rkt")
(require "capture-literals.rkt")
(require "advice.rkt")

(provide fib fib-3-back fib-not-constant-space)

(with-literal
  fib-body
  (define/contract (fib n)
    ((lambda (in) (in . >= . 0)) . -> . (lambda (out) (out . >= . 0)))
    (a-literal fib-body (cond ((= n 0) 0)
                              ((= n 1) 1)
                              (#t (+ (fib (- n 1))
                                     (fib (- n 2))))))))

(with-literal
  fib-3-back-body
  (define/contract (fib-3-back n)
    ((lambda (in) (in . >= . 0)) . -> . (lambda (out) (out . >= . 0)))
    (a-literal fib-3-back-body (cond ((= n 0) 0)
                                     ((= n 1) 1)
                                     ((= n 2) 2)
                                     (#t (+ (fib-3-back (- n 1))
                                            (fib-3-back (- n 2))
                                            (fib-3-back (- n 3))))))))

(with-literal
  fib-not-constant-space-body
  (define/contract (fib-not-constant-space n)
    ((lambda (in) (in . >= . 0)) . -> . (lambda (out) (out . >= . 0)))
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

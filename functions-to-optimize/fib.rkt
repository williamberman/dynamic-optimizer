#lang racket

(require "common.rkt")

(provide fib
         fib-3-back
         fib-not-constant-space)

(define/optimizable (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (#t (+ (fib (- n 1))
               (fib (- n 2))))))

(define/optimizable (fib-3-back n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (#t (+ (fib-3-back (- n 1))
               (fib-3-back (- n 2))
               (fib-3-back (- n 3))))))

(define/optimizable (fib-not-constant-space n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (#t (if (even? n)
                (fib-not-constant-space (- n 1))
                (+ (fib-not-constant-space (- n 1))
                   (fib-not-constant-space (- n 2)))))))

#lang racket

(require racket/contract)
(require "utils.rkt")

(provide fib fib-3-back fib-not-constant-space)

;; TODO is the a way to do define/contract with annotated-lambda?

(define fib
  (annotated-lambda (n)
                    (cond ((= n 0) 0)
                          ((= n 1) 1)
                          (#t (+ (fib (- n 1)) (fib (- n 2)))))))

(define fib-3-back
  (annotated-lambda (n)
                    (cond ((= n 0) 0)
                          ((= n 1) 1)
                          ((= n 2) 2)
                          (#t (+ (fib-3-back (- n 1)) (fib-3-back (- n 2)) (fib-3-back (- n 3)))))))

(define fib-not-constant-space
  (annotated-lambda (n)
                    (cond ((= n 0) 0)
                          ((= n 1) 1)
                          ((even? n) (fib-not-constant-space (- n 1)))
                          (#t (+ (fib-not-constant-space (- n 1)) (fib-not-constant-space (- n 2)))))))

;; (define/contract (fib n)
;;   ((lambda (in) (in . >= . 0)) . -> . (lambda (out) (out . >= . 0)))
;;   (cond ((= n 0) 0)
;;         ((= n 1) 1)
;;         (#t (+ (fib (- n 1)) (fib (- n 2))))))

;; (define/contract (fib-3-back n)
;;   ((lambda (in) (in . >= . 0)) . -> . (lambda (out) (out . >= . 0)))
;;   (cond ((= n 0) 0)
;;         ((= n 1) 1)
;;         ((= n 2) 2)
;;         (#t (+ (fib-3-back (- n 1)) (fib-3-back (- n 2)) (fib-3-back (- n 3))))))

;; (define/contract (fib-not-constant-space n)
;;   ((lambda (in) (in . >= . 0)) . -> . (lambda (out) (out . >= . 0)))
;;   (cond ((= n 0) 0)
;;         ((= n 1) 1)
;;         ((even? n) (fib-not-constant-space (- n 1)))
;;         (#t (+ (fib-not-constant-space (- n 1)) (fib-not-constant-space (- n 2))))))

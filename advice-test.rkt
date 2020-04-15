#lang racket

(require "advice.rkt"
         rackunit
         rackunit/text-ui)

(define advice-tests
  (test-suite
   "Tests for defining advice"

   (test-case
       "Calls function"
     (define function-called #f)

     (define func (make-receptive-function
                   (lambda ()
                     (set! function-called #t))))

     (func)
     (check-equal? function-called #t))

   (test-case
       "Before"

     (define set-in-before #f)

     (define func (make-receptive-function
                   (lambda ()
                     (check-equal? set-in-before #t))))

     (add-function 'before func (lambda () (set! set-in-before #t)))

     (func))

   (test-case
       "After"
     (define set-in-after #f)

     (define func (make-receptive-function
                   (lambda ()
                     (check-equal? set-in-after #f))))

     (add-function 'after func (lambda () (set! set-in-after #t)))
     (func)
     (check-equal? set-in-after #t))

   (test-case
       "Around"
     (define before-executed #f)
     (define function-executed #f)
     (define after-executed #f)

     (define func (make-receptive-function
                   (lambda ()
                     (set! function-executed #t)
                     'original-return-value)))

     (add-function 'around func
                   (lambda (the-func)
                     (set! before-executed #t)
                     (the-func)
                     (set! after-executed #t)
                     'modified-return-value))

     (check-equal? (func) 'modified-return-value)
     (check-equal? before-executed #t)
     (check-equal? function-executed #t)
     (check-equal? after-executed #t))))

(run-tests advice-tests)

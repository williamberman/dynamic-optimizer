#lang racket

(require "common.rkt"
         rackunit
         rackunit/text-ui
         racket/match)

(define common-tests
  (test-suite
   "get-subproblem"

   (test-case "basic fibonacci"
     (define the-function '(cond ((= n 0) 0)
                                 ((= n 1) 1)
                                 (#t (+ (fib (- n 1))
                                     (fib (- n 2))))))

     (define the-subproblem (get-subproblem-combination-function the-function
                                                                 'fib
                                                                 '(n)))

     (check-equal? (match the-subproblem
                     [(list 'lambda
                            (list a b '#:currently-computing c)
                            (list '+ a b))
                      #t]
                     [else #f])
                   #t))

   (test-case "non-constant depth recursive call"
     (define the-function '(cond ((= n 0) 0)
                                 ((= n 1) 1)
                                 (#t (if (even? n)
                                         (fib-not-constant-space (- n 1))
                                         (+ (fib-not-constant-space (- n 1))
                                            (fib-not-constant-space (- n 2)))))))

     (define the-subproblem (get-subproblem-combination-function the-function
                                                                 'fib-not-constant-space
                                                                 '(n)))

     (check-equal? (match the-subproblem
                     [(list 'lambda
                            (list a b '#:currently-computing c)
                            (list 'if (list 'even? (list 'list-ref c 0))
                                  a
                                  (list '+ a b)))
                      #t]
                     [else #f])
                   #t))))

(run-tests common-tests)

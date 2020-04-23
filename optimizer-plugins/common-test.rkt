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
                   #t))

   (test-case "non-constant depth recursive call"
     (define the-function '(if (or (= row 0) (= column 0))
                               (list ((maximal-square-matrix) row column)
                                     ((maximal-square-matrix) row column))
                               (let ([dependent-on (list (maximal-square (- row 1) column)
                                                         (maximal-square row (- column 1))
                                                         (maximal-square (- row 1) (- column 1)))])
                                 (define current (if (= 0 ((maximal-square-matrix) row column))
                                                     0
                                                     (+ 1 (apply min (map car dependent-on)))))
                                 
                                 (define max-seen (apply max (cons current (map cadr dependent-on))))
                                 
                                 (list current max-seen))))
     (define the-subproblem (get-subproblem-combination-function the-function
                                                                 'maximal-square
                                                                 '(row column)))

     (displayln the-subproblem))))

(run-tests common-tests)

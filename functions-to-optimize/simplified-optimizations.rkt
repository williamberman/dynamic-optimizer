#lang racket

(require "./maximal-square.rkt")

(define (simplified-fib) 
  (define previous-2 0)
  (define previous-1 1)
  (for ([computing-for '((2) (3) (4) (5))])
    (define cur (+ previous-1 previous-2))
    (set! previous-2 previous-1)
    (set! previous-1 cur))
  previous-1)

(define (simplified-fib-3-back)
  (define previous-3 0)
  (define previous-2 1)
  (define previous-1 2)
  (for ([computing-for '((3) (4) (5))])
    (define cur (+ previous-1 previous-2 previous-3))
    (set! previous-3 previous-2)
    (set! previous-2 previous-1)
    (set! previous-1 cur))
  previous-1)

(define (simplified-fib-not-constant-space)
  (define results (make-hash '(((1) . 1))))
  (for ([dependent-on
         '(((1)) ((1) (2)) ((3)) ((4) (3)))]
        [computing-for '((2) (3) (4) (5))])
    (define computed
      (keyword-apply
       (lambda (arg-1 (arg-2 #f) #:currently-computing currently-computing)
         (if (even? currently-computing)
             arg-1
             (+ arg-1 arg-2)))
       '(#:currently-computing)
       computing-for
       (map (lambda (requirement)
              (hash-ref results requirement))
            dependent-on)))
    (hash-set! results computing-for computed))
  (hash-ref results '(5)))

(define (simplified-maximal-square)
  (parameterize ([maximal-square-matrix example-matrix-2])
   (define results (make-hash
                    '(((0 1) 1 1)
                      ((3 0) 0 0)
                      ((1 0) 1 1)
                      ((2 0) 1 1)
                      ((0 4) 0 0)
                      ((0 0) 1 1)
                      ((0 2) 1 1)
                      ((0 3) 0 0))))
   (for ([dependent-on
          '(((0 0) (0 1) (1 0))
            ((1 1) (2 0) (1 0))
            ((3 0) (2 0) (2 1))
            ((1 1) (0 2) (0 1))
            ((1 1) (1 2) (2 1))
            ((2 1) (3 1) (2 2))
            ((1 2) (0 3) (0 2))
            ((0 4) (0 3) (1 3))
            ((1 2) (1 3) (2 2))
            ((2 3) (1 3) (1 4))
            ((2 3) (3 2) (2 2))
            ((3 3) (2 3) (2 4)))]
         [computing-for
          '((1 1)
            (2 1)
            (3 1)
            (1 2)
            (2 2)
            (3 2)
            (1 3)
            (1 4)
            (2 3)
            (2 4)
            (3 3)
            (3 4))])
     (define computed
       (keyword-apply
        (lambda (arg-1 arg-2 arg-3 #:row row #:column column)
          (define dependent-on (list arg-1 arg-2 arg-3))
          (define current (if (= 0 ((maximal-square-matrix) row column))
                              0
                              (+ 1 (apply min (map car dependent-on)))))
          (define max-seen (apply max (cons current (map cadr dependent-on))))
          (list current max-seen))
        '(#:column #:row)
        (list (cadr computing-for) (car computing-for))
        (map (lambda (requirement)
               (hash-ref results requirement))
             dependent-on)))
     (hash-set! results computing-for computed))
   (hash-ref results '(3 4))))

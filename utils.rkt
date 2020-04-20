#lang racket

(require graph
         racket/stxparam)

(provide root-node
         graph->tree
         butlast
         tree-map
         aif
         awhen
         it)

;; Root node is a sentinel to provide graph entrypoints
(define root-node 'root-node)

(define (graph->tree% graph [node root-node])
  (cons node (map
              (lambda (child) (graph->tree% graph child))
              (get-neighbors graph node))))

(define/contract (graph->tree graph . args)
  (dag? . -> . any/c)
  (apply graph->tree% (cons graph args)))

(define (butlast lst (n 1))
  (if (<= (length lst) n)
      (list)
      (cons (car lst) (butlast (cdr lst) n))))

(define (tree-map pred lst)
  (let loop ((lst lst)
             (acc identity))
    (cond
      ((null? lst)
       (acc '()))
      ((not (pair? (car lst)))
       (loop (cdr lst) (lambda (r)
                         (acc (cons (pred (car lst)) r)))))
      (else
       (loop (cdr lst)
             (lambda (r)
               (acc (cons (loop (car lst) identity) r))))))))

(define-syntax-parameter it (lambda (stx)
                              (raise-syntax-error
                               (syntax-e stx)
                               "can only be used inside syntax which provides binding.")))

(define-syntax-rule (aif predicate consequent alternative)
  (let ([tmp predicate])
    (if tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
          consequent)
        alternative)))

(define-syntax-rule (awhen predicate consequent)
  (let ([tmp predicate])
    (when tmp
        (syntax-parameterize ([it (make-rename-transformer #'tmp)])
          consequent))))


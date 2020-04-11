#lang racket

(require typed-stack)
(require graph)

(provide
 around
 install!
 uninstall!
 root-node
 graph->tree
 butlast
 annotated-lambda
 annotated-procedure-body
 get-subproblem-combination-function)

(define (around #:fn fn #:before before #:after after)
  (define (wrapped . args)
    (define return-value (match (apply before args)
                           [(list 'return-value rv) rv]
                           [_ (apply fn args)]))
    (apply after (list) #:args args #:return-value return-value)
    return-value)
  wrapped)

(define old-values (make-weak-hash))

(define (install! identifier new-value)
  (define these-old-values (hash-ref old-values identifier make-stack))
  (push! these-old-values (eval identifier))
  (hash-set! old-values identifier these-old-values)
  (eval `(set! ,identifier ,new-value)))

(define (uninstall! identifier)
  (define these-old-values (hash-ref old-values identifier))
  (define old-value (top these-old-values))
  (pop! these-old-values)
  (eval `(set! ,identifier ,old-value)))

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

(struct annotated-procedure (procedure body)
  #:property prop:procedure (struct-field-index procedure))

(define-syntax-rule (annotated-lambda formals . body)
  (annotated-procedure (lambda formals . body)
                       '(lambda formals . body)))

;; This is going to be a really fun experiment in code walking
(define (get-subproblem-combination-function annotated-procedure)
  ;; TODO
  '+)

#lang racket

(require "call-graph.rkt"
         "utils.rkt"
         graph
         rackunit
         rackunit/text-ui)

(define call-graph-builder-tests
  (test-suite
   "Tests for building call graphs"

   (test-case
       "single element call graph"
     (define the-call-graph-builder (make-call-graph-builder))
     (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #f)
     (call-graph-builder-pre-call the-call-graph-builder #:args '(1))
     (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #f)
     (call-graph-builder-post-call the-call-graph-builder #:args '(1) #:return-value 2)
     (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #t)
     (define the-call-graph (call-graph-builder-call-graph the-call-graph-builder))
     (define the-children (get-neighbors the-call-graph root-node))
     (check-equal? (length the-children) 1)
     (define the-node (car the-children))
     (check-equal? (a-call-arguments the-node) '(1))
     (check-equal? (a-call-return-value the-node) 2))

   (test-case
       "call graph with one level of children"
     (define the-call-graph-builder (make-call-graph-builder))

     (call-graph-builder-pre-call the-call-graph-builder #:args '(1))
     (call-graph-builder-pre-call the-call-graph-builder #:args '(2))
     (call-graph-builder-post-call the-call-graph-builder #:args '(2) #:return-value 3)
     (call-graph-builder-pre-call the-call-graph-builder #:args '(3))
     (call-graph-builder-post-call the-call-graph-builder #:args '(3) #:return-value 4)
     (call-graph-builder-post-call the-call-graph-builder #:args '(1) #:return-value 2)
     
     (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #t)

     (define the-call-graph (call-graph-builder-call-graph the-call-graph-builder))
     (define the-node (car (get-neighbors the-call-graph root-node)))

     (check-equal? (a-call-arguments the-node) '(1))
     (check-equal? (a-call-return-value the-node) 2)

     (define the-neighbors (get-neighbors the-call-graph the-node))
     (check-equal? (length the-neighbors) 2)

     (check-equal? (a-call-arguments (car the-neighbors)) '(3))
     (check-equal? (a-call-return-value (car the-neighbors)) 4)

     (check-equal? (a-call-arguments (cadr the-neighbors)) '(2))
     (check-equal? (a-call-return-value (cadr the-neighbors)) 3))

   (test-case
       "general call graph"
     (define the-call-graph-builder (make-call-graph-builder))

     (define (run-0)
       (call-graph-builder-pre-call the-call-graph-builder #:args '(0))
       (call-graph-builder-post-call the-call-graph-builder #:args '(0) #:return-value 0))

     (define (run-1)
       (call-graph-builder-pre-call the-call-graph-builder #:args '(1))
       (call-graph-builder-post-call the-call-graph-builder #:args '(1) #:return-value 1))

     (define (run-2)
       (call-graph-builder-pre-call the-call-graph-builder #:args '(2))
       (run-1)
       (run-0)
       (call-graph-builder-post-call the-call-graph-builder #:args '(2) #:return-value 1))

     (define (run-3)
       (call-graph-builder-pre-call the-call-graph-builder #:args '(3))
       (run-2)
       (run-1)
       (call-graph-builder-post-call the-call-graph-builder #:args '(3) #:return-value 2))

     (define (run-4)
       (call-graph-builder-pre-call the-call-graph-builder #:args '(4))
       (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #f)
       (run-3)
       (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #f)
       (run-2)
       (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #f)
       (call-graph-builder-post-call the-call-graph-builder #:args '(4) #:return-value 3))

     (run-4)
     (check-equal? (call-graph-builder-is-complete? the-call-graph-builder) #t)

     (define simplified-call-graph
       (tree-map
        (lambda (node) (if (a-call? node)
                           (list (a-call-arguments node) (a-call-return-value node))
                           node))
        (graph->tree (call-graph-builder-call-graph the-call-graph-builder))))

     (check-equal? simplified-call-graph
                   `(,root-node
                     (((4) 3)
                      (((3) 2)
                       (((1) 1))
                       (((2) 1)
                        (((1) 1))
                        (((0) 0))))
                      (((2) 1)
                       (((1) 1))
                       (((0) 0)))))))))

(run-tests call-graph-builder-tests)

#lang racket

(require typed-stack)
(require graph)
(require "utils.rkt")
(require data/gvector)

(provide a-call
         a-call?
         call-graph-builder-call-graph
         a-call-arguments
         a-call-return-value
         make-call-graph-builder
         call-graph-builder-pre-call
         call-graph-builder-post-call
         call-graph-builder-is-complete?
         looks-like-function?
         get-constant-space-used
         uses-constant-space?
         get-single-subproblem
         all-children-are-leaves?
         call-graph->all-arguments-top-down
         call-graph->all-arguments-bottom-up)

(struct a-call
  (arguments return-value children-hash)
  #:transparent)

(struct call-graph-builder
  (call-stack call-graph is-complete?)
  #:mutable)

(define (make-call-graph-builder)
  (call-graph-builder (make-stack) (unweighted-graph/directed (list)) #f))

(define (call-graph-builder-pre-call call-graph-builder #:args args)
  (push! (call-graph-builder-call-stack call-graph-builder) (make-gvector)))

(define (call-graph-builder-post-call call-graph-builder #:args args #:return-value return-value)
  (define children (top (call-graph-builder-call-stack call-graph-builder)))
  (pop! (call-graph-builder-call-stack call-graph-builder))
  (define the-call (a-call args return-value (equal-hash-code children)))
  (for ([child children])
    (add-directed-edge! (call-graph-builder-call-graph call-graph-builder) the-call child))
  (if (stack-empty? (call-graph-builder-call-stack call-graph-builder))
      (begin
        (add-directed-edge! (call-graph-builder-call-graph call-graph-builder) root-node the-call)
        (set-call-graph-builder-is-complete?! call-graph-builder #t))
      (gvector-add! (top (call-graph-builder-call-stack call-graph-builder)) the-call)))

(define (looks-like-function?% call-graph (node root-node) (previously-seen-calls (make-hash)))
  (define (recur)
    (andmap (lambda (child) (looks-like-function?% call-graph child previously-seen-calls))
              (get-neighbors call-graph node)))
  (if (eq? node root-node)
      (recur)
      (if (hash-has-key? previously-seen-calls (a-call-arguments node))
          (if (equal? (hash-ref previously-seen-calls (a-call-arguments node))
                      (a-call-return-value node))
              (recur)
              #f)
          (begin
            (hash-set! previously-seen-calls (a-call-arguments node) (a-call-return-value node))
            (recur)))))

(define/contract (looks-like-function? call-graph . args)
  (->* (dag?) () any/c)
  (apply looks-like-function?% (cons call-graph args)))

(struct exn:fail:not-a-constant-space-call-graph exn:fail (call-graph node))

(define (get-constant-space-used% call-graph (node root-node))
  (define children (get-neighbors call-graph node))

  (define (fail)
    (raise (exn:fail:not-a-constant-space-call-graph
            "Not a constant space call graph"
            (current-continuation-marks)
            call-graph node)))

  (define (collapse lst)
    (if (andmap (lambda (val) (= (car lst) val)) lst)
        (car lst)
        (fail)))
  
  (if (eq? node root-node)
      (collapse (map (lambda (child) (get-constant-space-used% call-graph child)) children))
      (if (all-children-are-leaves? call-graph node)
          (length children)
          (let* ([the-subproblem (get-single-subproblem call-graph node)]
                 [child-space-used (get-constant-space-used% call-graph the-subproblem)])
            (if (= child-space-used (length children))
                child-space-used
                (fail))))))

(define/contract (get-constant-space-used call-graph . args)
  (->* (dag?) () any/c)
  (apply get-constant-space-used% (cons call-graph args)))

(define (uses-constant-space? . args)
  (with-handlers ([exn:fail:not-a-constant-space-call-graph? (lambda (_) #f)])
    (apply get-constant-space-used args)
    #t))

(define (all-children-are-leaves? call-graph node)
  (andmap (lambda (child) (= 0 (length (get-neighbors call-graph child))))
          (get-neighbors call-graph node)))

(struct exn:fail:no-subproblem-found exn:fail (call-graph node))

;; One child such that all other children are children of this child
(define/contract (get-single-subproblem call-graph node)
  (any/c any/c . -> . (negate (curry eq? #f)))
  (define children (get-neighbors call-graph node))
  (define subproblem
    (findf (lambda (child)
             (define grand-children (apply set (get-neighbors call-graph child)))
             (andmap
              (lambda (other-child) (or
                                     (eq? other-child child)
                                     (set-member? grand-children other-child)))
              children))
           children))
  (if subproblem
      subproblem
      (raise (exn:fail:no-subproblem-found
              "No subproblem found."
              (current-continuation-marks)
              call-graph node))))

(define/contract (call-graph->all-arguments-top-down-last-step call-graph node)
  (->i ([call-graph graph?]
        [node (call-graph) (curry all-children-are-leaves? call-graph)])
       [result (call-graph node) any/c])
  (cons (list 'calculate (a-call-arguments node))
        (map
         (lambda (child) (make-base-case child))
         (sort (get-neighbors call-graph node) >
               #:key (lambda (child)
                       (length (get-neighbors (transpose call-graph) child)))))))


(define (call-graph->all-arguments-top-down call-graph (node root-node))
  (define children (get-neighbors call-graph node))
  (cond
    [(eq? node root-node)
     (call-graph->all-arguments-top-down call-graph (car (get-neighbors call-graph node)))]
    [(= 0 (length children))
     (list (make-base-case node))]
    [else
     (begin
       (with-handlers ([exn:fail:no-subproblem-found?
                        (lambda (_)
                          (call-graph->all-arguments-top-down-last-step call-graph node))])
         (cons (list 'calculate (a-call-arguments node))
               (call-graph->all-arguments-top-down
                call-graph
                (get-single-subproblem call-graph node)))))]))

(define (make-base-case node)
  (list 'base-case (a-call-arguments node) (a-call-return-value node)))

(define (call-graph->all-arguments-bottom-up . args)
  (reverse (apply call-graph->all-arguments-top-down args)))

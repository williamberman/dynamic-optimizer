#lang racket

(require typed-stack)
(require graph)
(require "utils.rkt")

(provide
 a-call
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

(define waiting-for-return-value 'waiting-for-return-value)

(struct a-call
  (arguments return-value)
  #:transparent
  #:mutable
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (equal?-recur (a-call-arguments a) (a-call-arguments b)))
   (define (hash-proc a hash-recur)
     (hash-recur (a-call-arguments a)))
   (define (hash2-proc a hash2-recur)
     (hash2-recur (a-call-arguments a)))])

(struct call-graph-builder (nodes-visited call-graph))

(define (make-call-graph-builder)
  (call-graph-builder (make-stack root-node) (unweighted-graph/directed (list))))

(define (call-graph-builder-pre-call call-graph-builder #:args args)
  (define the-call (a-call args waiting-for-return-value))
  (add-directed-edge!
   (call-graph-builder-call-graph call-graph-builder)
   (top (call-graph-builder-nodes-visited call-graph-builder))
   the-call)
  (push! (call-graph-builder-nodes-visited call-graph-builder) the-call))

(define (call-graph-builder-post-call call-graph-builder #:args args #:return-value return-value)
  ;; TODO need to be able to add to the graph only once return values
  ;; are known
  (set-a-call-return-value!
   (top (call-graph-builder-nodes-visited call-graph-builder))
   return-value)
  (pop! (call-graph-builder-nodes-visited call-graph-builder)))

(define (call-graph-builder-is-complete? call-graph-builder)
  (eq? root-node (top (call-graph-builder-nodes-visited call-graph-builder))))

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
         (lambda (child) (call-graph->all-arguments-top-down call-graph child))
         (sort (get-neighbors call-graph node) >
               #:key (lambda (child)
                       (length (get-neighbors (transpose call-graph) child)))))))


(define (call-graph->all-arguments-top-down call-graph (node root-node))
  (define children (get-neighbors call-graph node))
  (cond
    [(eq? node root-node)
     (call-graph->all-arguments-top-down call-graph (car (get-neighbors call-graph node)))]
    [(= 0 (length children))
     (list 'base-case (a-call-arguments node) (a-call-return-value node))]
    [else
     (begin
       (with-handlers ([exn:fail:no-subproblem-found?
                        (lambda (_)
                          (call-graph->all-arguments-top-down-last-step call-graph node))])
         (cons (list 'calculate (a-call-arguments node))
               (call-graph->all-arguments-top-down call-graph (get-single-subproblem call-graph node)))))]))

(define (call-graph->all-arguments-bottom-up . args)
  (reverse (apply call-graph->all-arguments-top-down args)))

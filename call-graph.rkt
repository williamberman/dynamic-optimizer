#lang racket

(require typed-stack
         graph
         "utils.rkt"
         data/gvector
         "advice.rkt"
         "additional-properties.rkt")

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
         constant-space-call-graph->all-arguments-top-down
         constant-space-call-graph->all-arguments-bottom-up
         save-and-display-call-graph
         call-graphs
         install-call-graph!
         uninstall-call-graph!
         base-case
         calculate
         call-graph->all-arguments-bottom-up)

(define base-case 'base-case)
(define calculate 'calculate)

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
  (with-handlers ([exn:fail:not-a-constant-space-call-graph? (lambda (_) #f)]
                  [exn:fail:no-subproblem-found? (lambda (_) #f)])
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

(define/contract (constant-space-call-graph->all-arguments-top-down-last-step call-graph node)
  (->i ([call-graph graph?]
        [node (call-graph) (curry all-children-are-leaves? call-graph)])
       [result (call-graph node) any/c])
  (cons (make-location-call node calculate)
        (map
         (lambda (child) (make-location-call child base-case))
         (sort (get-neighbors call-graph node) >
               #:key (lambda (child)
                       (length (get-neighbors (transpose call-graph) child)))))))


(define (constant-space-call-graph->all-arguments-top-down call-graph (node root-node))
  (define children (get-neighbors call-graph node))
  (cond
    [(eq? node root-node)
     (constant-space-call-graph->all-arguments-top-down call-graph (car (get-neighbors call-graph node)))]
    [(= 0 (length children))
     (list (make-location-call node base-case))]
    [else
     (with-handlers ([exn:fail:no-subproblem-found?
                      (lambda (_)
                        (constant-space-call-graph->all-arguments-top-down-last-step call-graph node))])
       (cons (make-location-call node calculate)
             (constant-space-call-graph->all-arguments-top-down
              call-graph
              (get-single-subproblem call-graph node))))]))

(define (make-location-call the-call the-location)
  (define the-new-call (struct-copy a-call the-call))
  (property-set! the-new-call 'location the-location)
  the-new-call)

(define (constant-space-call-graph->all-arguments-bottom-up . args)
  (reverse (apply constant-space-call-graph->all-arguments-top-down args)))

(define (call-graph->all-arguments-bottom-up call-graph)
  (map (lambda (the-call)
         (define the-location (if (= 0 (length (get-neighbors call-graph the-call)))
                                  base-case
                                  calculate))
         (make-location-call the-call the-location))
       (butlast (tsort (transpose call-graph)))))

(define (install-call-graph! receptive-function visit-completed-call-graph)
  (define call-graph-builder (make-call-graph-builder))

  (define around
    (make-keyword-procedure
     (lambda (kws kw-args the-function . args)
       (define the-argument-list (make-argument-list kws kw-args args))
       
       (call-graph-builder-pre-call call-graph-builder
                                    #:args the-argument-list)

       (define the-return-value (keyword-apply the-function kws kw-args args))

       (call-graph-builder-post-call call-graph-builder
                                     #:args the-argument-list
                                     #:return-value the-return-value)

       (when (call-graph-builder-is-complete? call-graph-builder)
         (visit-completed-call-graph (call-graph-builder-call-graph call-graph-builder))
         (set! call-graph-builder (make-call-graph-builder)))

       the-return-value)))

  (add-function 'around receptive-function around))

(define (uninstall-call-graph! receptive-function)
  (remove-function 'around receptive-function))

(define call-graphs (make-gvector))

(define (save-and-display-call-graph call-graph (output-file-path #f))
  (gvector-add! call-graphs call-graph)
  (when output-file-path
    (display-call-graph call-graph output-file-path)))

(define (display-call-graph call-graph output-file-path)
  (call-with-output-file output-file-path
      (lambda (out)
        (display (graphviz call-graph) out))
      #:exists 'replace))

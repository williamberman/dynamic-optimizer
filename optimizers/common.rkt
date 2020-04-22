#lang racket

(require data/gvector
         typed-stack
         "../additional-properties.rkt"
         "../call-graph.rkt"
         "../utils.rkt")

(provide get-subproblem-combination-function
         split-arguments-into-initial-values-and-need-to-calculate
         make-optimized-function-helper)

(define (get-subproblem-combination-function function-body
                                             function-identifier
                                             function-signature
                                             #:variable-number-of-arguments
                                             [variable-number-of-arguments #f])
  (define the-subproblem (get-subproblem function-body function-identifier))

  (define the-subproblem-arguments (make-hash))

  (define (mapper item)
    (if (list? item)
        (if (eq? (car item) function-identifier)
            (if (hash-has-key? the-subproblem-arguments (cdr item))
                (hash-ref the-subproblem-arguments (cdr item))
                (let ([an-argument (gensym 'arg)])
                  (hash-set! the-subproblem-arguments (cdr item) an-argument)
                  an-argument))
            (map mapper item))
        item))
  
  (define new-function-body (map mapper the-subproblem))

  (set! the-subproblem-arguments (hash-values the-subproblem-arguments))
  (when variable-number-of-arguments
    (set! the-subproblem-arguments (map (lambda (arg) (list arg #f))
                                        the-subproblem-arguments)))
  (define x-the-subproblem-arguments (make-gvector))
  (apply gvector-add! (cons x-the-subproblem-arguments the-subproblem-arguments))
  
  (define the-currently-computing-id (gensym 'currently-computing))
  (gvector-add! x-the-subproblem-arguments
                '#:currently-computing
                the-currently-computing-id)

  (set! new-function-body (tree-map (lambda (form)
                                      (aif (index-of function-signature form)
                                           `(list-ref ,the-currently-computing-id ,it)
                                           form))
                                    new-function-body))

  `(lambda ,(gvector->list x-the-subproblem-arguments) ,new-function-body))

(define (get-subproblem function-body function-name)
  (define the-stacks (list))

  (define (search current-stack current-expression)
    (when (list? current-expression)
        (if (eq? function-name
                 (car current-expression))
            (set! the-stacks
                  (cons (push current-stack current-expression)
                        the-stacks))
            (map (lambda (sub-expression) (search (push current-stack current-expression) sub-expression))
                 current-expression))))
  
  (search (make-stack) function-body)

  (define (find-subproblem)
    (if (andmap (lambda (a-stack) (eq? (top a-stack) (top (car the-stacks))))
                the-stacks)
        (top (car the-stacks))
        (let ([deepest-stack #f]
              [depth -inf.0])
         (for ([stack the-stacks])
           (when ((stack-length stack) . > . depth)
             (set! depth (stack-length stack))
             (set! deepest-stack stack)))
         (pop! deepest-stack)
         (find-subproblem))))

  (find-subproblem))

(define (split-arguments-into-initial-values-and-need-to-calculate all-arguments)
  (define initial-values (make-gvector))
  (define need-to-calculate (make-gvector))
  (for ([the-call all-arguments])
    (cond
      [(eq? base-case (property-ref the-call 'location))
       (gvector-add! initial-values the-call)]
      [(eq? calculate (property-ref the-call 'location))
       (gvector-add! need-to-calculate the-call)]
      [#t (raise (format "Not known location: ~a" (property-ref the-call 'location)))]))
  (list (gvector->list initial-values) (gvector->list need-to-calculate)))

(define (make-optimized-function-helper body)
  (define the-optimized-function-as-data
    `(lambda (,(gensym 'kws) ,(gensym 'kw-args) . ,(gensym 'rest))
       ,@body))
  
  (define the-optimized-function (eval `(make-keyword-procedure ,the-optimized-function-as-data)))

  (property-set! the-optimized-function 'body the-optimized-function-as-data)

  the-optimized-function)

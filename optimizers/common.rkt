#lang racket

(require data/gvector
         typed-stack
         "../additional-properties.rkt"
         "../call-graph.rkt")

(provide get-subproblem-combination-function
         split-arguments-into-initial-values-and-need-to-calculate
         make-optimized-function-helper)

(define (get-subproblem-combination-function function-body function-identifier)
  (define the-subproblem (get-subproblem function-body function-identifier))
  
  (define the-subproblem-arguments (make-gvector))

  (define (mapper item)
    (if (list? item)
        (if (eq? (car item) function-identifier)
            (let ([an-argument (gensym 'arg)])
              (gvector-add! the-subproblem-arguments an-argument)
              an-argument)
            (map mapper item))
        item))
  
  (define new-function-body (map mapper the-subproblem))

  `(lambda ,(gvector->list the-subproblem-arguments) ,new-function-body))

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
        (begin
          (map pop! the-stacks)
          (find-subproblem))))

  (find-subproblem))

(define (split-arguments-into-initial-values-and-need-to-calculate all-arguments)
  (define initial-values (make-gvector))
  (define need-to-calculate (make-gvector))
  (for ([arguments all-arguments])
    (cond
      [(eq? base-case (a-call-location arguments))
       (gvector-add! initial-values (a-call-arguments arguments))]
      [(eq? calculate (a-call-location arguments))
       (gvector-add! need-to-calculate (a-call-arguments arguments))]
      [#t (raise (format "Not known location: ~a" (a-call-location arguments)))]))
  (list (gvector->list initial-values) (gvector->list need-to-calculate)))

(define (make-optimized-function-helper body)
  (define the-optimized-function-as-data
    `(lambda (,(gensym 'kws) ,(gensym 'kw-args) . ,(gensym 'rest))
       ,@body))
  
  (define the-optimized-function (eval `(make-keyword-procedure ,the-optimized-function-as-data)))

  (property-set! the-optimized-function 'body the-optimized-function-as-data)

  the-optimized-function)

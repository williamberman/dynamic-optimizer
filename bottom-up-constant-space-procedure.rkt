#lang racket

(require data/gvector
         "call-graph.rkt"
         typed-stack)

(provide make-bottom-up-constant-space-procedure
         can-make-bottom-up-constant-space-procedure?
         get-subproblem-combination-function)

(define (can-make-bottom-up-constant-space-procedure? call-graph)
  (and (looks-like-function? call-graph) (uses-constant-space? call-graph)))

(define (make-bottom-up-constant-space-procedure all-arguments update-function)
  (match-let ([(list initial-values need-to-calculate)
               (split-arguments-into-initial-values-and-need-to-calculate all-arguments)])
    `(,@(make-definitions initial-values)
      (for ([i (quote ,need-to-calculate)])
        (define cur ,(make-current-update update-function (length initial-values)))
        ,@(make-definition-updates (length initial-values)))
      ,(number->definition-symbol 1))))

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
    (match arguments
      [(list 'base-case arguments value)
       (gvector-add! initial-values value)]
      [(list 'calculate arguments)
       (gvector-add! need-to-calculate arguments)]))
  (list (gvector->list initial-values) (gvector->list need-to-calculate)))

(define (make-definitions initial-values)
  (define counter (length initial-values))
  (map
   (lambda (value)
     (define return-value
       `(define ,(number->definition-symbol counter) ,value))
     (set! counter (- counter 1))
     return-value)
   initial-values))

(define (make-current-update update-function number-definitions)
  `(,update-function
    ,@(stream->list
       (stream-map
        number->definition-symbol
        (in-range 1 (+ 1 number-definitions))))))

(define (make-definition-updates number-definitions)
  (reverse
   (cons `(set! ,(number->definition-symbol 1) cur)
         (stream->list
          (stream-map
           (lambda (definition-number)
             `(set! ,(number->definition-symbol definition-number)
                    ,(number->definition-symbol (- definition-number 1))))
           (in-range 2 (+ 1 number-definitions)))))))

(define (number->definition-symbol definition-number)
  (string->symbol (string-append "previous-" (number->string definition-number))))

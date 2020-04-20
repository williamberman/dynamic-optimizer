#lang racket

(require "optimizer.rkt"
         "additional-properties.rkt"
         data/gvector
         racket/exn)

(provide make-optimizer-repl)

;; TODO something in here is causing the second read
;; to hang when running the repl in emacs.
(define (make-optimizer-repl function optimizer name)
  (define this-repl-state (repl-state function optimizer (make-hash)))

  (define (the-read)
    (display (string-append "optimizer:" name "> "))
    (read))

  (define (the-eval input)
    (match input
      [(list 'help) (help)]
      [(list 'call args ...) (apply call args)]
      [(list 'enable args ...) (enable args)]
      [(list 'disable args ...) (disable args)]
      [(list 'available) (available-optimizations)]
      [(list 'view args ...) (view-optimization args)]
      [(list 'changes) (changes)]
      [_ (displayln "Not valid input")]))

  (define (the-print value)
    (when (not (void? value))
      (pretty-print value)))
  
  (define (the-loop)
    (define input (the-read))
    (if (equal? input '(quit))
        (begin
          (the-print (changes))
          (displayln "")
          (displayln "Goodbye"))
        (begin
          (with-handlers ([(lambda (_) #t) (lambda (exn)
                                             (displayln "Internal error.")
                                             (displayln (exn->string exn)))])
           (the-print (the-eval input)))
          (the-loop))))

  (lambda ()
    (parameterize ([the-repl-state this-repl-state])
      (the-print (help))
      (the-loop))))

(struct repl-state (function optimizer changes))

(define the-repl-state (make-parameter #f))

(define-syntax-rule (with-repl-state . body)
  (begin
    (when (not (the-repl-state))
      (raise "parameter: the-repl-state has not been set"))
    .
    body))

(define (enable args)
  (with-repl-state
    (optimizer-enable-optimization! (repl-state-optimizer (the-repl-state)) args)
    (hash-set! (repl-state-changes (the-repl-state)) args 'enabled)))

(define (disable args)
  (with-repl-state
    (optimizer-disable-optimization! (repl-state-optimizer (the-repl-state)) args)
    (hash-set! (repl-state-changes (the-repl-state)) args 'disabled)))

(define (call . args)
  (with-repl-state
    (apply (repl-state-function (the-repl-state)) args)))

(define (available-optimizations)
  (with-repl-state
    (define the-available-optimizations
      (optimizer-get-disabled-optimizations (repl-state-optimizer (the-repl-state))))
    (if (= 0 (length the-available-optimizations))
        (displayln "There are no available optimizations")
        (begin
          (displayln "Available optimizations")
          (for ([args the-available-optimizations])
            (displayln args))))))

(define (view-optimization args)
  (with-repl-state
    (pretty-print (property-ref
                   (optimizer-get-optimization (repl-state-optimizer (the-repl-state)) args)
                   'body))))

(define (help)
  (displayln "(call argument ...): Call the function being optimized with the given arguments.

(available): Display the optimizations which have been discovered but not enabled.

(view argument ...): View the body of the optimization that was discovered for the given arguments. 
Arguments should be in the same order as displayed by (available).

(enable argument ...): Enable the optimization that was discovered for the given arguments. 
The optimization may be disabled later. Arguments should be in the same order as displayed by (available).

(disable argument ...): Disable the optimization that was discovered for the given arguments. 
The optimization may again enabled later. Arguments should be in the same order as displayed by (available).

(changes): Display the modifications made in this repl session to the function being optimized.

(help): Display this message.

(quit): Exit the repl."))

(define (changes)
  (define enabled (make-gvector))
  (define disabled (make-gvector))
  
  (with-repl-state
    (hash-for-each
     (repl-state-changes (the-repl-state))
     (lambda (args state)
       (case state
         ['enabled (gvector-add! enabled args)]
         ['disabled (gvector-add! disabled args)]
         [else (raise (string-append "Illegal change state: " state))]))))

  (if (and (= (gvector-count enabled) 0)
           (= (gvector-count disabled) 0))
      (displayln "No changes")

      (begin
        

        (displayln "Changes made:")

        (when (> (gvector-count enabled) 0)
          (displayln "enabled")
          (for ([args enabled])
            (displayln args)))

        (when (> (gvector-count disabled) 0)
          (displayln "disabled")
          (for ([args disabled])
            (displayln args))))))

#lang racket

(require "optimizer.rkt")
(require data/gvector)

;; TODO I'd like to not have to provide all symbols, and instead only have
;; to provide make-optimizer-repl
(provide make-optimizer-repl
         enable disable
         call
         available-optimizations
         view-optimization
         help
         changes)

;; TODO something in here is causing the second read
;; to hang when running the repl in emacs.
;; TODO the available optimizations don't seem to be giving
;; the correct optimizations
(define (make-optimizer-repl optimizer)
  (define this-repl-state (repl-state optimizer (make-hash)))

  (define (the-read)
    (display (string-append "optimizer:" (optimizer-name optimizer) "> "))
    (read))

  (define (the-eval input)
    (eval input))

  (define (the-print value)
    (when (not (void? value))
      (displayln value)))
  
  (define (the-loop)
    (define input (the-read))
    (if (equal? input '(quit))
        (begin
          (the-print (changes))
          (the-print "Goodbye"))
        (begin 
          (the-print (the-eval input))
          (the-loop))))

  (lambda ()
    (parameterize ([the-repl-state this-repl-state])
      (the-print (help))
      (the-loop))))

(struct repl-state (optimizer changes))

(define the-repl-state (make-parameter #f))

(define-syntax-rule (with-repl-state . body)
  (begin
    (when (not (the-repl-state))
      (raise "parameter: the-repl-state has not been set"))
    .
    body))

(define (enable . args)
  (with-repl-state
    (optimizer-enable-optimization! (repl-state-optimizer (the-repl-state)) args)
    (hash-set! (repl-state-changes (the-repl-state)) args 'enabled)))

(define (disable . args)
  (with-repl-state
    (optimizer-disable-optimization! (repl-state-optimizer (the-repl-state)) args)
    (hash-set! (repl-state-changes (the-repl-state)) args 'disabled)))

(define (call . args)
  (with-repl-state
    (apply (repl-state-optimizer (the-repl-state)) args)))

(define (available-optimizations)
  (with-repl-state
    (define the-available-optimizations
      (optimizer-get-available-optimizations (repl-state-optimizer (the-repl-state))))
    (if (= 0 (length the-available-optimizations))
        (displayln "There are no available optimizations")
        (begin
          (displayln "Available optimizations")
          (for ([args the-available-optimizations])
            (displayln args))))))

(define (view-optimization . args)
  (with-repl-state
    (pretty-print (optimizer-get-optimization (repl-state-optimizer (the-repl-state)) args))))

(define (help)
  "TODO: Insert standard help message here")

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

  (when (> (gvector-count enabled) 0)
    (displayln "enabled")
    (for ([args enabled])
      (displayln args)))

  (when (> (gvector-count disabled) 0)
   (displayln "disabled")
   (for ([args disabled])
     (displayln args)))

  (when (and (= (gvector-count enabled) 0)
             (= (gvector-count disabled) 0))
    (displayln "No changes")))

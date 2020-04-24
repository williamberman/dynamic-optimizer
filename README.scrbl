#lang scribble/manual

@(require (for-label racket)
          scribble/example
          racket/sandbox)

@title{Dynamic Optimizer}

@section{Introduction}

This is an educational framework for runtime optimization. The focus is on producing human readable optimizations. Human readable optimizations allow the programmer to modify the produced optimizations. Hand modifying optimizations might be helpful in situations such as: the optimization is close to correct but not exact; the optimization can be made more general.

@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (make-evaluator 'racket
                     "(set-optimizer-repl-display-initial-help-message! #f)"
                     #:requires '("optimizer.rkt"
                                  "optimizer-repl.rkt"
                                  "optimizations.rkt"
                                  "optimizer-plugins/bottom-up-constant-space-procedure.rkt"
                                  "advice.rkt"
                                  "additional-properties.rkt"))))

@examples[#:eval my-evaluator
          #:label "Example - optimizing the fibonacci sequence with dynamic programming:"
          (enable-optimizer-plugin! make-bottom-up-constant-space-procedure)
          (define/optimizable (fib n)
            (cond ((= n 0) 0)
                  ((= n 1) 1)
                  (#t (+ (fib (- n 1))
                         (fib (- n 2))))))
          (install-optimizer! fib)
          (fib 6)
          ((make-optimizer-repl fib '((available)
                                      (view 6)
                                      (enable 6)
                                      (call 6))))]

@section{API}

@defform[(define/optimizable (head args) body)
         ]{Acts similarly to the standard @racket[define] form. The bound procedure may only have one body form. The result conforms to @racket[optimizable-function?]}

@defproc[(install-call-graph! [function optimizable-function?] [visit-call-graph (-> graph? any?)])
         void?]{Call graphs produced by calls to @racket[function] will be passed to @racket[visit-call-graph.] Usually used in conjunction with @racket[display-call-graph] or @racket[save-and-display-call-graph]. Helpful for visualization purposes.}

@defproc[(enable-optimizer-plugin?
          [optimizer-plugin (-> graph? optimizable-function? (or/c? #f procedure?))])
         void?]{Makes @racket[optimizer-plugin] globally available for all installed optimizers.}

@defproc[(install-optimizer! [function optimizable-function?])
         void?]{Attach an optimizer to @racket[function]. Calls to @racket[function] will now be watched to see if any optimizations apply. Just installing an optimizer won't have any affect on the functions behavior because found optimizations have to be explicitly enabled.}

@defproc[(make-optimizer-repl [function optimizable-function?]
                              [commands-to-execute (listof any/c?)])
         (-> void?)]{Returns a thunk that runs an optimizer REPL for @racket[function] when called. A non-interactive repl can be created by passing @racket[commands-to-execute] which will be executed sequentially by the repl.}

@section{Optimizer REPL functions}

All optimizer repl functions are enabled while in an optimizer REPL session and apply to the function that was used to create the REPL.

@defproc[(call [argument any?] ...) any?]{Call the function with the given arguments}

@defproc[(available) void?]{Display the arguments for the optimizations which have been discovered but not enabled.}

@defproc[(view [argument any?] ...) void?]{View the body of the optimization that was discovered for the given arguments. Arguments should be in the same order as displayed by @racket[available].}

@defproc[(enable [argument any?] ...) void?]{Enable the optimization that was discovered for the given arguments. The optimization may be later disabled. Arguments should be in the same order as displayed by @racket[available].}

@defproc[(disable [argument any?] ...) void?]{Disable the optimization that was discovered for racket[argument]s. The optimization may again enabled later. Arguments should be in the same order as displayed by @racket[available].}

@defproc[(changes) void?]{Display the optimizations enabled/disabled during the current repl session.}

@defproc[(help) void?]{Display the repl documentation.}

@defproc[(quit) void?]{Exit the repl.}

@section{Optimizer Plugins}

Optimizer plugins look for optimizations.

@defproc[(make-bottom-up-constant-space-procedure [call-graph graph?] [function optimizable-function?])
         (or/c procedure? #f)]{Looks for a set of recursive calls with redundant work that can be unrolled into a for loop with no repeated work. The produced optimizied function uses a constant amount of storage.}

@defproc[(make-bottom-up-non-constant-space-procedure [call-graph graph?] [function optimizable-function?])
         (or/c procedure? #f)]{Looks for a set of recursive calls that can be unrolled into a for loop with no repeated work. The optimized function stores intermediate results such that the procedure uses linear space.}

@section{Notes}

The macros which produce the optimizations are quite hairy and play fast and loose with introducing new bindings. This requires the optimized functions to be written in a certain way. Time should be spent rewriting the macros instead of documenting their present edge cases and odd behavior.

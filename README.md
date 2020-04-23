TODO There's a vocab discrepancy for optimization, optimizer, and optimizable. Sort it out

The goal here is to do provide a framework for optimization of code at run time and provide
a few example optimizations that can be performed. Optimizations are more focused on being
human readable as opposed to being correct such that an individual can read and then modify
the optimization if they so wish.

Example optimization of the fibonacci sequence to take advantage of dynamic programming.
```
;; TODO format me
> (define/optimizable (fib n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (#t (+ (fib (- n 1))
                  (fib (- n 2))))))
> (install-optimizer! fib-3-back)
> (fib 5)
5
> ((make-optimizer-repl fib-3-back))
> (available)
Available optimizations
(5)
> (view 5)
'(lambda (kws276636 kw-args276637 . rest276638)
                  (define previous-2 0)
                  (define previous-1 1)
                  (for
                   ((computing-for '((2) (3) (4) (5))))
                   (define cur
                     (keyword-apply
                      (lambda (arg276633
                               arg276634
                               #:currently-computing
                               currently-computing276635)
                        (+ arg276633 arg276634))
                      '(#:currently-computing)
                      (list computing-for)
                      (list previous-1 previous-2)))
                   (set! previous-2 previous-1)
                   (set! previous-1 cur))
                  previous-1)
> (enable 5)
> (call 5)
5

```


The optimizations only work on a single function which calls itself recursively.

(define/optimizable (_head_ _args_) body)
Defines a function that may be optimized

(install-call-graph! <optimizable function> <receives call graph>)
Used to view the call graph for a series of calls to the given function. Useful for visualization puposes.

TODO, this name should be changed
(add-optimizer! <an optimizer>)
Makes an optimizer globally available for all installed optimizers. An optimizer is a function which
takes a call graph and an optimizable function and either returns an optimized function or #f to indicate
that an optimization could not be found.

(install-optimizer! <optimizable function>)
Attach an optimizer to the given function. Calls to this function will now be watched for the possibility
of applying optimizations. Just installing an optimizer won't have any affect on the functions behavior becase found optimizations have to be explicitly enabled.

(make-optimizer-repl <optimizable function>)
Provides a repl for interactive inspection of the optimizer.

The repl provides the following functionality

(call argument ...): Call the function being optimized with the given arguments.

(available): Display the optimizations which have been discovered but not enabled.

(view argument ...): View the body of the optimization that was discovered for the given arguments. 
Arguments should be in the same order as displayed by (available).

(enable argument ...): Enable the optimization that was discovered for the given arguments. 
The optimization may be disabled later. Arguments should be in the same order as displayed by (available).

(disable argument ...): Disable the optimization that was discovered for the given arguments. 
The optimization may again enabled later. Arguments should be in the same order as displayed by (available).

(changes): Display the modifications made in this repl session to the function being optimized.

(help): Display this message.

(quit): Exit the repl.


make-bottom-up-constant-space-procedure
make-bottom-up-non-constant-space-procedure

## 
```racket
```
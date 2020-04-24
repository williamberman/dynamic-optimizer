# Dynamic Optimizer

## 1. Introduction

This is an educational framework for runtime optimization. The focus is
on producing human readable optimizations. Human readable optimizations
allow the programmer to modify the produced optimizations. Hand
modifying optimizations might be helpful in situations such as: the
optimization is close to correct but not exact; the optimization can be
made more general.

Example - optimizing the fibonacci sequence with dynamic programming:

```racket
> (enable-optimizer-plugin! make-bottom-up-constant-space-procedure)
> (define/optimizable (fib n)                                       
    (cond ((= n 0) 0)                                               
          ((= n 1) 1)                                               
          (#t (+ (fib (- n 1))                                      
                 (fib (- n 2))))))                                  
> (install-optimizer! fib)                                          
> (fib 6)                                                           
8                                                                   
> ((make-optimizer-repl fib '((available)                           
                              (view 6)                              
                              (enable 6)                            
                              (call 6))))                           
optimizer:fib> (available)                                          
Available optimizations                                             
(6)                                                                 
optimizer:fib> (view 6)                                             
'(lambda (kws5155 kw-args5156 . rest5157)                           
   (define previous-2 0)                                            
   (define previous-1 1)                                            
   (for                                                             
    ((computing-for '((2) (3) (4) (5) (6))))                        
    (define cur                                                     
      (keyword-apply                                                
       (lambda (arg5152 arg5153 #:currently-computing               
currently-computing5154)                                            
         (+ arg5152 arg5153))                                       
       '(#:currently-computing)                                     
       (list computing-for)                                         
       (list previous-1 previous-2)))                               
    (set! previous-2 previous-1)                                    
    (set! previous-1 cur))                                          
   previous-1)                                                      
optimizer:fib> (enable 6)                                           
optimizer:fib> (call 6)                                             
8                                                                   
optimizer:fib> (quit)                                               
Changes made:                                                       
enabled                                                             
(6)                                                                 
                                                                    
Goodbye                                                             
```

## 2. API

```racket
(define/optimizable (head args) body)
```

Acts similarly to the standard `define` form. The bound procedure may
only have one body form. The result conforms to `optimizable-function?`

```racket
(install-call-graph! function                  
                     visit-call-graph) -> void?
  function : optimizable-function?             
  visit-call-graph : (-> graph? any?)          
```

Call graphs produced by calls to `function` will be passed to
`visit-call-graph.` Usually used in conjunction with
`display-call-graph` or `save-and-display-call-graph`. Helpful for
visualization purposes.

```racket
(enable-optimizer-plugin? optimizer-plugin) -> void?                        
  optimizer-plugin : (-> graph? optimizable-function? (or/c? #f procedure?))
```

Makes `optimizer-plugin` globally available for all installed
optimizers.

```racket
(install-optimizer! function) -> void?
  function : optimizable-function?    
```

Attach an optimizer to `function`. Calls to `function` will now be
watched to see if any optimizations apply. Just installing an optimizer
won’t have any affect on the functions behavior because found
optimizations have to be explicitly enabled.

```racket
(make-optimizer-repl function                          
                     commands-to-execute) -> (-> void?)
  function : optimizable-function?                     
  commands-to-execute : (or/c #f (listof any/c?))      
```

Returns a thunk that runs an optimizer REPL for `function` when called.
A non-interactive REPL can be created by passing `commands-to-execute`
which will be executed sequentially by the repl.

## 3. Optimizer REPL functions

All optimizer REPL functions are enabled while in an optimizer REPL
session and apply to the function that was used to create the REPL.

```racket
(call argument ...) -> any?
  argument : any?          
```

Call the function with `argument`s

```racket
(available) -> void?
```

Display the arguments for the optimizations which have been discovered
but not enabled.

```racket
(view argument ...) -> void?
  argument : any?           
```

View the body of the optimization that was discovered for `argument`s.
`argument`s should be in the same order as displayed by `available`.

```racket
(enable argument ...) -> void?
  argument : any?             
```

Enable the optimization that was discovered for `argument`s. The
optimization may be later disabled. `argument`s should be in the same
order as displayed by `available`.

```racket
(disable argument ...) -> void?
  argument : any?              
```

Disable the optimization that was discovered for `argument`s. The
optimization may again enabled later. `argument`s should be in the same
order as displayed by `available`.

```racket
(changes) -> void?
```

Display the optimizations enabled/disabled during the current REPL
session.

```racket
(help) -> void?
```

Display the REPL documentation.

```racket
(quit) -> void?
```

Exit the REPL.

## 4. Optimizer Plugins

Optimizer plugins look for optimizations.

```racket
(make-bottom-up-constant-space-procedure call-graph 
                                         function)  
 -> (or/c procedure? #f)                            
  call-graph : graph?                               
  function : optimizable-function?                  
```

Looks for a set of recursive calls with redundant work that can be
unrolled into a for loop with no repeated work. The produced optimizied
function uses a constant amount of storage. Returns `#f` if an
optimization cannot be found.

```racket
(make-bottom-up-non-constant-space-procedure call-graph 
                                             function)  
 -> (or/c procedure? #f)                                
  call-graph : graph?                                   
  function : optimizable-function?                      
```

Looks for a set of recursive calls that can be unrolled into a for loop
with no repeated work. The optimized function stores intermediate
results such that the procedure uses linear space. Returns `#f` if an
optimization cannot be found.

## 5. Notes

The macros which produce the optimizations are quite hairy and play fast
and loose with introducing new bindings. This requires the optimized
functions to be written in a certain way. Time should be spent rewriting
the macros instead of documenting their present edge cases and odd
behavior.

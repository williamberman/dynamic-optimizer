#lang racket

(provide make-receptive-function
         add-function
         remove-function)

(require "additional-properties.rkt"
         "utils.rkt")

;; Light weight implementation of CLOS/emacslisp function advice.
;; Only supports before, after, and around combinators.
;; Only supports a single advice function in each position.
;; Doesn't support depth of advice to control relative execution order.
;; Attempts to mimic the api here,
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
;; ~A function that may take advice is receptive. Get it?~
(define (make-receptive-function the-actual-function)
  (define the-receiver
    (make-keyword-procedure
     (lambda (kws kw-args . rest )
       
       (awhen (property-ref the-receiver 'before)
              (keyword-apply it
                             kws
                             kw-args
                             rest))

       (define the-return-value
         (aif (property-ref the-receiver 'around)
              (keyword-apply it
                             kws
                             kw-args
                             (cons the-actual-function rest))
              (keyword-apply the-actual-function
                             kws
                             kw-args
                             rest)))

       (awhen (property-ref the-receiver 'after)
              (keyword-apply it
                             kws
                             kw-args
                             rest))

       the-return-value)))

  (property-set! the-receiver 'before #f)
  (property-set! the-receiver 'around #f)
  (property-set! the-receiver 'after #f)

  the-receiver)

(define (add-function where the-receptive-function the-advice)
  (case where
    ['before (property-set! the-receptive-function 'before the-advice)]
    ['around (property-set! the-receptive-function 'around the-advice)]
    ['after (property-set! the-receptive-function 'after the-advice)]))

(define (remove-function where the-receptive-function)
    (case where
      ['before (property-remove! the-receptive-function 'before)]
      ['around (property-remove! the-receptive-function 'around)]
      ['after (property-remove! the-receptive-function 'after)]))

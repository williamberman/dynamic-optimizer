#lang racket

(provide make-receptive-function
         add-function)

;; Light weight implementation of CLOS/emacslisp function advice.
;; Only supports before, after, and around combinators.
;; Only supports a single advice function in each position.
;; Doesn't support depth of advice to control relative execution order.
;; Attempts to mimic the api here,
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
;; ~A function that may take advice is receptive. Get it?~
(define (make-receptive-function the-actual-function)
  ;; This is a placeholder. We Need a reference to the-receptive-function to define the-receiver.
  (define the-receptive-function #f)

  (define the-receiver
    (make-keyword-procedure
     (lambda (kws kw-args . rest )
       
       (when (receptive-function-before the-receptive-function)
         (keyword-apply (receptive-function-before the-receptive-function)
                        kws
                        kw-args
                        rest))

       (define the-return-value
         (if (receptive-function-around the-receptive-function)
             (keyword-apply (receptive-function-around the-receptive-function)
                            kws
                            kw-args
                            (cons the-actual-function rest))
             (keyword-apply the-actual-function
                            kws
                            kw-args
                            rest)))

       (when (receptive-function-after the-receptive-function)
         (keyword-apply (receptive-function-after the-receptive-function)
                        kws
                        kw-args
                        rest))

       the-return-value)))

  (set! the-receptive-function (receptive-function the-receiver #f #f #f))

  the-receptive-function)

(define (add-function where the-receptive-function the-advice)
  (case where
    ['before (set-receptive-function-before! the-receptive-function the-advice)]
    ['around (set-receptive-function-around! the-receptive-function the-advice)]
    ['after (set-receptive-function-after! the-receptive-function the-advice)]))

(struct receptive-function
  (receiver [before #:mutable] [after #:mutable] [around #:mutable])
  #:property prop:procedure (struct-field-index receiver))

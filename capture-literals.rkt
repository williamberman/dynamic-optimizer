#lang racket

(require (for-syntax racket/match syntax/parse))

(provide with-literal)

(begin-for-syntax
 (struct literal-result (literal-value transformed-datum) #:transparent)

 (struct literal-not-found-exn exn:fail (the-literal-name datum))

 (define (find-literal the-literal-name datum)
   (define found (find-literal% the-literal-name datum))
   (if (literal-result? found)
       found
       (raise (literal-not-found-exn "Could not find literal"
                                     (current-continuation-marks)
                                     the-literal-name
                                     datum))))


 (define (find-literal% the-literal-name datum)
   (if (not (list? datum))
       datum
       (let ([literal-was-found #f]
             [found-literal-value #f])
         
         (define transformed-datum
           (match datum
             [(list 'a-literal found-literal-name the-literal-value)
              #:when (and (eq? found-literal-name the-literal-name))
              (begin
                (set! literal-was-found #t)
                (set! found-literal-value the-literal-value)
                the-literal-value)]
             [_ datum]))
         
         (if literal-was-found
             (literal-result found-literal-value transformed-datum)
             (begin
               (set! transformed-datum
                     (map (lambda (sub-datum)
                            (define sub-result (find-literal% the-literal-name sub-datum))
                            (if (literal-result? sub-result)
                                (begin
                                  (set! literal-was-found #t)
                                  (set! found-literal-value (literal-result-literal-value sub-result))
                                  (literal-result-transformed-datum sub-result))
                                sub-result))
                          datum))
               (if literal-was-found
                   (literal-result found-literal-value transformed-datum)
                   transformed-datum)))))))

(define-syntax (with-literal stx)
  (syntax-parse stx
    [(_ variable-identifier:id contains-literal:expr)
     (let ([the-literal-result (find-literal (syntax->datum #'variable-identifier)
                                             (syntax->datum #'contains-literal))])
       (datum->syntax stx
                      `(begin
                         (define ,#'variable-identifier
                           ',(literal-result-literal-value the-literal-result))
                         ,(literal-result-transformed-datum the-literal-result))))]))

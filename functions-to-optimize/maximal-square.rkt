#lang racket

(require "../additional-properties.rkt"
         "../advice.rkt"
         "../optimizer.rkt")

(provide maximal-square-matrix
         example-matrix-1
         example-matrix-2
         make-matrix
         maximal-square)

(define maximal-square-matrix (make-parameter #f))

(define/optimizable (maximal-square row column)
  (if (or (= row 0) (= column 0))
      (list ((maximal-square-matrix) row column)
            ((maximal-square-matrix) row column))
      ((lambda dependent-on
         (define current (if (= 0 ((maximal-square-matrix) row column))
                            0
                            (+ 1 (apply min (map car dependent-on)))))
        
        (define max-seen (apply max (cons current (map cadr dependent-on))))
        
        (list current max-seen))

       (maximal-square (- row 1) column)
       (maximal-square row (- column 1))
       (maximal-square (- row 1) (- column 1)))))

(define (make-matrix . rows)
  (define the-matrix (make-vector (length rows)))
  (define (the-getter row-index column-index)
    (vector-ref (vector-ref the-matrix row-index)
                column-index))

  (for ([row rows]
        [row-index (in-range (length rows))])
    (vector-set! the-matrix row-index (make-vector (length row)))
    (for ([value row]
          [column-index (in-range (length row))])
      (vector-set! (vector-ref the-matrix row-index) column-index value)))
  
  the-getter)

(define example-matrix-1 (make-matrix '[1 0 1 0 0]
                                      '[1 0 1 1 1]
                                      '[1 1 1 1 1]
                                      '[1 0 0 1 0]))

(define example-matrix-2 (make-matrix '[1 1 1 0 0]
                                      '[1 1 1 0 0]
                                      '[1 1 1 0 0]
                                      '[0 0 0 1 1]
                                      '[0 0 0 1 1]))

#lang racket

;; Set of properties stored away from a given object.  This is considered
;; bad programming style, but it is more composable than defining a new
;; datatype anytime you want to augment the behavior of an object at
;; runtime.

(provide property-set!
         property-ref
         property-remove!)

(define properties (make-weak-hasheq))

(define (property-set! object property value)
  (ensure-property-hash object)
  (hash-set! (hash-ref properties object)
             property
             value))

(define (property-ref object property)
  (ensure-property-hash object)
  (hash-ref (hash-ref properties object)
            property))

(define (property-remove! object property)
  (ensure-property-hash object)
  (hash-remove! (hash-ref properties object)
                property))

(define (ensure-property-hash object)
  (when (not (hash-has-key? properties object))
    (hash-set! properties object (make-hash))))

#lang racket

(provide
 (contract-out
  [prefab? predicate/c]
  [prefab-fields (-> prefab? list?)]))


(define (prefab? x)
  (if (prefab-struct-key x) #t #f))

(define (prefab-fields x)
  (match (struct->vector x)
    [(vector _ fields ...) fields]))

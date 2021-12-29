#lang racket

(provide
 (contract-out
  [sexpr? predicate/c]))

(require "prefab.rkt")

(define/match (sexpr? x)
  [((or (? boolean?)
        (? number?)
        (? char?)
        (? string?)
        (? symbol?)
        (? bytes?)
        (? regexp?)
        (? byte-regexp?)
        (? path?)
        (list (? sexpr?) ...)
        (vector (? sexpr?) ...)
        (hash-table ((? sexpr?) (? sexpr?)) ...))) #t]
  [((and table (? hash?)))
   (for/fold ([result #t])
       ([(k v) table]
        #:break (not result))
     (and (sexpr? k) (sexpr? v)))]
  [((? prefab?)) (sexpr? (prefab-fields x))]
  [(_) #f])

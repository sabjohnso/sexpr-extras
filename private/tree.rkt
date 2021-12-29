#lang racket

(provide
 (contract-out
  [sexp-tree? predicate/c]
  [make-sexp-tree  (-> sexpr? (and/c sexp-tree? sexpr?))]
  [sexp-tree-data  (-> (and/c sexp-tree? sexpr?) sexpr?)]
  [sexp-tree-open  (-> sexpr? (and/c sexp-tree? sexpr?) (and/c sexp-tree? sexpr?))]
  [sexp-tree-close (-> (and/c sexp-tree? sexpr?) (and/c sexp-tree? sexpr?))]
  [sexp-tree-path  (-> (and/c sexp-tree? sexpr?) (listof sexpr?))]
  [sexp-tree-put   (-> sexpr? (and/c sexp-tree? sexpr?) (and/c sexp-tree? sexpr?))]
  [sexp-tree-write (-> (listof sexpr?) sexpr? (and/c sexp-tree? sexpr?) (and/c sexp-tree? sexpr?))]))

(require
 racket/vector
 "prefab.rkt" "sexpr.rkt")

(define (vector-set xs index x)
  (vector-append (vector-take xs index)
                 (vector x)
                 (vector-drop xs (add1 index))))

(module+ test
  (require rackunit))

(struct ctx (key data tainted?) #:prefab)
(struct sexp-tree (data context) #:prefab)

(define (make-sexp-tree sexp)
  (sexp-tree sexp '()))

(define (sexp-tree-open key tree)
  (match-let ([(sexp-tree data context) tree])
    (sexp-tree
     (match data
       [(? list?)   (list-ref   data key)]
       [(? vector?) (vector-ref data key)]
       [(? hash?)   (hash-ref   data key)]
       [(? prefab?) (list-ref (prefab-fields data) key)])
     (cons (ctx key data #f) context))))

(module+ test
  (check-equal?
   (sexp-tree-open 2 (make-sexp-tree (list 1 2 3 4)))
   (sexp-tree 3 (list (ctx 2 (list 1 2 3 4) #f))))

  (check-equal?
   (sexp-tree-open 2 (make-sexp-tree (vector 1 2 3 4)))
   (sexp-tree 3 (list (ctx 2 (vector 1 2 3 4) #f))))

  (check-equal?
   (sexp-tree-open 'x (make-sexp-tree (make-immutable-hash '((x . 1) (y . 2)))))
   (sexp-tree 1 (list (ctx 'x (make-immutable-hash '((x . 1) (y . 2))) #f)))))

(define (sexp-tree-close tree)
  (match tree
    [(sexp-tree _ (list (ctx _ data #f) more-context ...))
     (sexp-tree data more-context)]
    [(sexp-tree data (list (ctx key prev-data #t) more-context ...))
     (sexp-tree
      (match prev-data
        [(?   list?) (list-set   prev-data key data)]
        [(? vector?) (vector-set prev-data key data)]
        [(?   hash?) (hash-set   prev-data key data)])
      more-context)]))

(define (sexp-tree-path tree)
  (define (aux context accum)
    (match tree
      [(list (ctx key _ _) context ...)
       (aux context (cons key accum))]
      ['() accum]))
  (aux (sexp-tree-context tree) '()))

(define (sexp-tree-open-path sexp-path tree)
  (match sexp-path
    [(list key more-keys ...)
     (sexp-tree-open-path more-keys (sexp-tree-open key tree))]))

(define (sexp-tree-root tree)
  (match tree
    [(sexp-tree data (list _ _ ...)) (sexp-tree-root (sexp-tree-close tree))]
    [(sexp-tree _ '()) tree]))

(module+ test
  (check-equal?
   (sexp-tree-root (make-sexp-tree '(1 2 3 4)))
   (sexp-tree '(1 2 3 4) '()))

  (check-equal?
   (sexp-tree-root (sexp-tree-open 1 (make-sexp-tree '(1 2 3 4))))
   (sexp-tree '(1 2 3 4) '())))

(define (sexp-tree-put value tree)
  (match tree
    [(sexp-tree _ (list (ctx key data _) more-context ...))
     (sexp-tree value (cons (ctx key data #t) more-context))]
    [(sexp-tree _ '())
     (sexp-tree value '())]))

(define (sexp-tree-write sexp-path value tree)
  (match sexp-path
    [(list key more-keys ...)
     (sexp-tree-write more-keys value (sexp-tree-open key tree))]
    ['() (sexp-tree-put value tree)]))


(define (sexp-tree-ref sexp-path tree)
  (match sexp-path
    [(list key keys ...) (sexp-tree-ref keys (sexp-tree-open key tree))]
    ['() (sexp-tree-data tree)]))



(module+ test
  (check-equal?
   (sexp-tree-data
    (sexp-tree-root (sexp-tree-root  (sexp-tree-put 3 (sexp-tree-open 'y (make-sexp-tree (make-immutable-hash '((x . 1) (y . 2)))))))))
   (make-immutable-hash '((x . 1) (y . 3)))))

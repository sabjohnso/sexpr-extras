#lang racket

(require
 (for-syntax racket racket/syntax syntax/parse)
  racket/generic racket/fasl net/base64
  "prefab.rkt"
  "sexpr.rkt")

(provide
 sexpr-serializable-struct
 (struct-out exn:fail:sexpr-deserialize)
 (contract-out
  [sexpr-serializable? predicate/c]
  [sexpr-serialize (-> sexpr-serializable? sexpr?)]
  [sexpr-deserializable? predicate/c]
  [sexpr-deserialize (-> sexpr?  any/c)]
  [sexpr-deserialize-sexpr       (-> sexpr? (or/c (listof sexpr?)))]
  [sexpr-deserialize-boolean     (-> sexpr? (or/c (listof boolean?) null?))]
  [sexpr-deserialize-number      (-> sexpr? (or/c (listof number?) null?))]
  [sexpr-deserialize-char        (-> sexpr? (or/c (listof char?) null?))]
  [sexpr-deserialize-string      (-> sexpr? (or/c (listof string?) null?))]
  [sexpr-deserialize-symbol      (-> sexpr? (or/c (listof symbol?) null?))]
  [sexpr-deserialize-regexp      (-> sexpr? (or/c (listof regexp?) null?))]
  [sexpr-deserialize-byte-regexp (-> sexpr? (or/c (listof byte-regexp?) null?))]
  [sexpr-deserialize-path        (-> sexpr? (or/c (listof path?) null?))]
  [sexpr-deserialize-list        (-> sexpr? (or/c (listof (listof sexpr?)) null?))]
  [sexpr-deserialize-vector      (-> sexpr? (or/c (listof (vectorof sexpr?)) null?))]
  [sexpr-deserialize-hash        (-> sexpr? (or/c (listof hash?) null?))]
  [sexpr-serialize/fasl     (-> sexpr-serializable? bytes?)]
  [sexpr-serialize/base64   (-> sexpr-serializable? bytes?)]
  [sexpr-deserialize/fasl   (-> bytes? any/c)]
  [sexpr-deserialize/base64 (-> bytes? any/c)]
  [current-sexpr-deserializers (parameter/c (listof (-> sexpr? list?)))]
  [sexpr-deserializer (-> sexpr-deserializable? (-> sexpr? list?))]))

(define/match (sexpr? x)
  [((or (? number?) (? char?) (? string?) (? symbol?))) #t]
  [((list (? sexpr?) ...))   #t]
  [((vector (? sexpr?) ...)) #t]
  [((? prefab?)) (sexpr? (prefab-fields x))]
  [(_) #f])

(define-generics sexpr-serializable
  (sexpr-serialize sexpr-serializable)
  #:fast-defaults
  ([sexpr? (define sexpr-serialize identity)]))

(struct exn:fail:sexpr-deserialize
  exn:fail () #:transparent)

(define-syntax sexpr-deserialize-error
  (syntax-parser
   [(_ sexpr:id)
    #'(raise (exn:fail:sexpr-deserialize
              (format (string-join
                       '("Failed to deserialize s-expression:"
                         "\ts-expression: ~a"
                         "\ts-deserializers: ~a") "~%")
                      sexpr
                      (current-sexpr-deserializers))
              (current-continuation-marks)))]))



(begin-for-syntax
 (define-splicing-syntax-class field-options
   [pattern (~seq
             (~or (~optional (~and #:auto auto:keyword))
                  (~optional (~and #:mutable mutable:keyword))) ...)])

 (define-syntax-class field-spec
   [pattern name:id]
   [pattern (name:id options:field-options)])

(define-splicing-syntax-class struct-options
  [pattern (~seq (~or (~optional (~and #:mutable mutable:keyword))
                      (~optional (~seq #:super super:expr))
                      (~optional (~seq #:inspector inspector:expr))
                      (~optional (~seq #:auto-value auto:expr))
                      (~optional (~seq #:guard guard:expr))
                      (~optional (~seq #:property prop-name:expr prop-val:expr))
                      (~optional (~and #:transparent transparent:keyword))
                      (~optional (~and #:prefab prefab:keyword))
                      (~optional (~and #:authentic authentic:keyword))
                      (~optional (~seq #:name name:id ))
                      (~optional (~seq #:extra-name extra-name:id))
                      (~optional (~seq #:constructor-name constructor-name:id))
                      (~optional (~seq #:extra-constructor-name extra-constructor-name:id))
                      (~optional (~seq #:reflection-name reflection-name:expr))
                      (~optional (~and #:omit-define-syntaxes))
                      (~optional (~and #:omit-define-values))
                      (~seq #:methods gen-name:id methods:expr)) ...)]))

(define-values (imp-prop:sexpr-deserializer sexpr-deserializable? sexpr-deserializer)
  (make-impersonator-property 'sexpr-deserializer))

(define (sexpr-deserialize-sexpr sexpr)
  (list sexpr))

(define (sexpr-deserialize-boolean sexpr)
  (if (boolean? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-number sexpr)
  (if (number? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-char sexpr)
  (if (char? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-string sexpr)
  (if (string? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-symbol sexpr)
  (if (symbol? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-bytes sexpr)
  (if (bytes? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-regexp sexpr)
  (if (regexp? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-byte-regexp sexpr)
  (if (byte-regexp? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-path sexpr)
  (if (path? sexpr) (list sexpr) '()))

(define (sexpr-deserialize-list sexpr)
  (if (list? sexpr) (list (map sexpr-deserialize sexpr)) '()))

(define (sexpr-deserialize-vector sexpr)
  (if (vector? sexpr) (list (vector-map sexpr-deserialize sexpr)) '()))

(define (sexpr-deserialize-hash sexpr)
  (if (hash? sexpr)
      (list (for/hash ([(k v) sexpr])
              (values (sexpr-deserialize k)
                      (sexpr-deserialize v))))
    '()))

(define current-sexpr-deserializers
  (make-parameter
   (list
    sexpr-deserialize-boolean
    sexpr-deserialize-number
    sexpr-deserialize-char
    sexpr-deserialize-string
    sexpr-deserialize-symbol
    sexpr-deserialize-regexp
    sexpr-deserialize-byte-regexp
    sexpr-deserialize-path
    sexpr-deserialize-list
    sexpr-deserialize-vector
    sexpr-deserialize-hash
    sexpr-deserialize-sexpr)))

(define (sexpr-deserialize sexpr)
  (let ([result (for/fold ([result '()])
                    ([deserializer (current-sexpr-deserializers)]
                     #:break (not (null? result)))
                  (deserializer sexpr))])
    (if (null? result) (sexpr-deserialize-error sexpr)
      (car result))))

(define-syntax sexpr-serializable-struct
  (syntax-parser
   [(_ name:id (field:field-spec ...)
       options:struct-options)
    (with-syntax ([field-count (length (syntax-e #'(field ...)))]
                  [(option ...) #'options]
                  [(field-accessor ...) (map (λ (field-name) (format-id field-name "~a-~a" #'name field-name))
                                          (syntax-e #'(field.name ...)))]
                  [recur (generate-temporary)]
                  [this (generate-temporary)]
                  [constructor (generate-temporary)]
                  [hidden-name (generate-temporary)]
                  [sexpr (generate-temporary)]
                  [field-values (generate-temporary)])
      #`(begin
          (struct name (field ...) option ...
            #:constructor-name constructor
            #:name hidden-name
            #:methods gen:sexpr-serializable
            ((define/generic recur sexpr-serialize)
             (define (sexpr-serialize this)
               (make-prefab-struct 'name (recur (field-accessor this)) ...))))
          (define name
            (let ()
              (define (name field.name ...)
                (constructor field.name ...))
              (chaperone-procedure name #f
                                   imp-prop:sexpr-deserializer
                                   (lambda (sexpr)
                                     (if (and (prefab? sexpr) (eq? (prefab-struct-key sexpr) 'name))
                                         (let ((field-values (prefab-fields sexpr)))
                                           (if (= (length field-values) field-count)
                                               (list (apply constructor (map sexpr-deserialize field-values)))
                                             '()))
                                       '())))))
          (current-sexpr-deserializers
           (cons (sexpr-deserializer name)
                 (current-sexpr-deserializers)))))]))

(define (sexpr-deserialize/fasl bytes)
  (sexpr-deserialize (fasl->s-exp bytes)))

(define (sexpr-serialize/fasl v)
  (s-exp->fasl (sexpr-serialize v)))

(define (sexpr-serialize/base64 v [newline #"\r\n"])
  (base64-encode (sexpr-serialize/fasl v) newline))

(define (sexpr-deserialize/base64 bytes)
  (base64-decode (sexpr-deserialize/fasl bytes)))

#lang racket

(module+ test
  (require rackunit
           "../main.rkt"
           (rename-in "../main.rkt" [sexpr-serializable-struct struct]))

  (struct point
    (x y)
    #:transparent)

  (struct triangle
    (v1 v2 v3)
    #:transparent)

  (check-equal? (sexpr-serialize (point 3 4)) #s(point 3 4))
  (check-equal? (sexpr-deserialize #s(point 3 4)) (point 3 4))

  (check-equal?
   (sexpr-serialize (triangle (point 0 0) (point 0 1) (point 1 0)))
   #s(triangle #s(point 0 0)
               #s(point 0 1)
               #s(point 1 0)))

  (check-equal?
   (sexpr-deserialize
    #s(triangle #s(point 0 0)
                #s(point 0 1)
                #s(point 1 0)))
   (triangle (point 0 0) (point 0 1) (point 1 0)))

  (check-equal?
   (sexpr-deserialize
    (map sexpr-serialize (list (point 0 0) (point 0 1) (point 1 0))))
   (list (point 0 0) (point 0 1) (point 1 0)))

  (check-equal?
   (sexpr-deserialize
    (vector-map sexpr-serialize (vector (point 0 0) (point 0 1) (point 1 0))))
   (vector (point 0 0) (point 0 1) (point 1 0)))

  (parameterize ([current-sexpr-deserializers
                  (list (sexpr-deserializer point)
                        (sexpr-deserializer triangle)
                        sexpr-deserialize-number)])
    (check-equal?
     (sexpr-deserialize
      #s(triangle #s(point 0 0)
                  #s(point 0 1)
                  #s(point 1 0)))
     (triangle (point 0 0) (point 0 1) (point 1 0))))

  (parameterize ([current-sexpr-deserializers '()])
    (check-exn exn:fail:sexpr-deserialize?
               (thunk (sexpr-deserialize
                       #s(triangle #s(point 0 0)
                                   #s(point 0 1)
                                   #s(point 1 0))))))

  (let ([t (triangle (point 0 0) (point 0 1) (point 1 0))])
    (check-equal? (sexpr-deserialize/fasl (sexpr-serialize/fasl t)) t)
    (check-equal? (sexpr-deserialize/base64 (sexpr-serialize/base64 t)) t)))

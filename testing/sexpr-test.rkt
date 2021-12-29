#lang racket

(module+ test
  (require rackunit "../main.rkt")
  (check-true (sexpr? #t))
  (check-true (sexpr? 3))
  (check-true (sexpr? #\x))
  (check-true (sexpr? "x"))
  (check-true (sexpr? 'x))
  (check-true (sexpr? #"x"))
  (check-true (sexpr? #rx"x"))
  (check-true (sexpr? #px"x"))
  (check-true (sexpr? #px#"x"))
  (check-true (sexpr? (string->path "/x")))
  (check-true (sexpr? '(x)))
  (check-true (sexpr? #(x)))
  (check-true (sexpr? #hash((x . y) (y . z))))
  (check-true (sexpr? #s(thing 1)))
  (check-true (sexpr? (make-immutable-hash
                       '((x . #s(x 1))
                         (y . #s(y 2))))))

  (check-false (sexpr? (λ (x) x)))
  (check-false (sexpr? object%))
  (check-false (sexpr? (new object%)))
  (struct thing (value))
  (check-false (sexpr? (thing 1))))

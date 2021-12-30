#lang scribble/manual
@require[@for-label["../main.rkt"
                    racket/base]]

@title{sexp-extras}
@author{sbj}

@defmodule[sexp-extras]

@defproc[(sexpr? [v any/c]) boolean?]{
 Return @racket[#t] if @racket[v] is an S-expression. Otherwise, return @racket[#f].
}

@defproc[(prefab? [v any/c]) boolean?]{
 Return @racket[#t] if @racket[v] is an instance of a
  @secref["prefab structure type" #:doc '(lib "scribblings/guide/guide.scrbl")]. Otherwise, return  @racket[#f].
}

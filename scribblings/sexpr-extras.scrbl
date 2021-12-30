#lang scribble/manual
@require[@for-label[sexpr-extras
                    racket/base]]

@title{sexp-extras}
@author{sbj}

@defmodule[sexpr-extras]

@defproc[(sexpr? [v any/c]) boolean?]{
 Return @racket[#t] if @racket[v] is an S-expression. Otherwise, return @racket[#f].
}

@defproc[(prefab? [v any/c]) boolean?]{
 Return @racket[#t] if @racket[v] is an instance of a prefab struct type (See @secref["prefab-struct" #:doc '(lib "scribblings/guide/guide.scrbl")]). Otherwise, return  @racket[#f].
}

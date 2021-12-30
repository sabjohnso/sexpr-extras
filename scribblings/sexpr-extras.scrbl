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

@defproc[(sexpr-serialize [v sexpr-serializable?]) sexpr?]{
 Return an @racket[sexpr?] representation of the input value.
}

@defproc[(sexpr-deserialize [v sexpr?]) any/c]{
 Deserialize the input @racket[sexpr?].
}

@defproc[(sexpr-serialize/fasl [v sexpr-serializable?]) bytes?]{
 Return @racket[bytes?] containing the fasl (see  @secref["fasl" #:doc '(lib "scribblings/reference/reference.scrbl")])
 representation of the input value.
}

@defproc[(sexpr-deserialize/fasl [v bytes?]) any/c]{
 Return a value deserialized from the input fasl byte string.
}


@defproc[(sexpr-serialize/base64 [v sexpr-serializable?]) bytes]{
 Return the base64 encoding of the fasl serialization of the input value.
}

@defproc[(sexpr-deserialize/base64 [v bytes?]) any/c]{
 Return the value deserialized from the input base64 encoded fasl byte string.
}



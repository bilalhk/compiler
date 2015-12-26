type t

val to_string : t -> string

val to_int : t -> int

val of_string : string -> t

val t_of_sexp : Sexplib.Sexp.t -> t

val sexp_of_t : t -> Sexplib.Sexp.t

val compare : t -> t -> int
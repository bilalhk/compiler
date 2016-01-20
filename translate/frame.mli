type t

type access

val new_frame : (Symbol.t * bool) list -> t

val alloc_local : t -> bool -> access
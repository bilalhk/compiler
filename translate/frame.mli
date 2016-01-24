type t

type access

val fp : Temp.reg

val sp: Temp.reg

val staticLinkOffset : int

val new_frame : (Symbol.t * bool) list -> t

val alloc_local : t -> bool -> access
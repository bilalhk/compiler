type access =
	| InFrame of int
	| InReg of Temp.reg

type t = {formals: (Symbol.t * access) list; localCount: int ref}

val fp : Temp.reg

val sp: Temp.reg

val staticLinkOffset : int

val wordSize : int

val new_frame : (Symbol.t * bool) list -> t

val alloc_local : t -> bool -> access

val external_call : string -> Tree.exp list -> Tree.exp
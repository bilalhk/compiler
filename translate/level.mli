type t = {name: Symbol.t; parent: t; frame: Frame.t; label: Temp.label}

type access = Frame.access * t

val mainLevel : t

val new_level : Symbol.t -> t -> (Symbol.t * bool) list -> t

val allocl_local : t -> bool -> access
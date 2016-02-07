type t = {name: Symbol.t; parent: t option; frame: Frame.t; label: Temp.label; levelId: int}

type access = Frame.access * t

val mainLevel : t

val new_level : Symbol.t -> t -> (Symbol.t * bool) list -> t

val alloc_local : t -> bool -> access
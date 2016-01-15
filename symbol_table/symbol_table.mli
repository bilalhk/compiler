open Core.Std

type 'v t

exception Empty_table

val empty : 'v t

val push_scope : 'v t -> (Symbol.t * 'v) list -> 'v t

val add : Symbol.t -> 'v -> 'v t -> 'v t

val pop_scope_exn : 'v t -> 'v t

val pop_scope : 'v t -> 'v t

val find : 'v t -> Symbol.t -> 'v option

val in_current_scope : Symbol.t -> 'v t -> bool
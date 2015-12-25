open Core.Std

type 'v t

exception Empty_table

val empty : 'v t

val push : 'v t -> (Symbol.Map.Key.t * 'v) list -> 'v t

val pop_exn : 'v t -> 'v t

val pop : 'v t -> 'v t

val find : 'v t -> Symbol.Map.Key.t -> 'v option
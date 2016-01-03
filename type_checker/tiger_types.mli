type unique

type type_entry =
	| Int
	| String
	| Record of (Symbol.t * type_entry) list * unique
	| Array of type_entry * unique
	| Nil
	| Unit
	| Name of Symbol.t * type_entry option ref

type var_entry =
	| VarEntry of type_entry
	| FunEntry of type_entry list * type_entry

val make_unique : unit -> unique
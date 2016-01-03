type unique = int ref

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

let next_int = ref 0

let make_unique () =
	let unique = ref !next_int in
	next_int := !next_int + 1;
	unique
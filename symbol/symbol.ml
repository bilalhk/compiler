open Core.Std
open Sexplib.Sexp

type t = int * string

let next_int = ref 0

let table = String.Table.create ()

let create_and_return_symbol str =
	match Hashtbl.add table str !next_int with
	| `Ok -> next_int := !next_int + 1;
			 (!next_int - 1, str)
	| `Duplicate -> assert false

let int_of_sexp = function
	| Atom int_str -> Int.of_string int_str
	| _ -> assert false

let str_of_sexp = function
	| Atom str -> str
	| _ -> assert false

let to_string = function (_, sym_str) -> sym_str

let to_int = function (sym_int, _) -> sym_int

let of_string str = 
	match Hashtbl.find table str with
	| None -> create_and_return_symbol str
	| Some sym_int -> (sym_int, str)

let t_of_sexp = function 
	| Atom _ -> assert false
	| List (sexp_int::sexp_str::[]) -> (int_of_sexp sexp_int, str_of_sexp sexp_str)
	| _ -> assert false

let sexp_of_t = function (sym_int, sym_str) -> 
	let sexp_int = Atom (Int.to_string sym_int) in
	let sexp_str = Atom sym_str in
	Sexp.List [sexp_int; sexp_str]

let compare sym1 sym2 =
	let int1 = to_int sym1 in
	let int2 = to_int sym2 in
	Int.compare int1 int2;;
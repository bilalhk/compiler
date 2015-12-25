open Core.Std

(* module Symbol_map = Map.Make (Symbol) *)

type 'a t = 'a Symbol.Map.t list

exception Empty_table

let empty : 'a t = []

let push table assoc_lst =
	let scope = Symbol.Map.of_alist_exn assoc_lst in
	scope::table

let pop_exn = function
	| hd::tl -> tl
	| [] -> raise Empty_table

let pop = function
	| hd::tl -> tl
	| [] -> []

let find table sym =
	let relevant_scope = List.find table ~f:(fun scope -> Map.mem scope sym) in
	match relevant_scope with
	| Some scope -> Map.find scope sym
	| None -> None
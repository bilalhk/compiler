open Core.Std

module Symbol_map = Map.Make (Symbol)

type 'v t = 'v Symbol_map.t list

exception Empty_table

let empty : 'v t = []

let add key value = function
	| curr_scope::higher_scopes ->
		let augmented_curr_scope = Map.add curr_scope ~key:key ~data:value in
		augmented_curr_scope::higher_scopes
	| [] -> raise Empty_table

let push_scope table assoc_lst =
	let scope = Symbol_map.of_alist_exn assoc_lst in
	scope::table

let pop_scope_exn = function
	| hd::tl -> tl
	| [] -> raise Empty_table

let pop_scope = function
	| hd::tl -> tl
	| [] -> []

let find table sym =
	let relevant_scope = List.find table ~f:(fun scope -> Map.mem scope sym) in
	match relevant_scope with
	| Some scope -> Map.find scope sym
	| None -> None

let in_current_scope key = function
	| curr_scope::_ ->
		(match Map.find curr_scope key with
		| Some _ -> true
		| None -> false)
	| [] -> false
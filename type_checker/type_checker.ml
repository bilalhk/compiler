open Tiger_types
open Ast
open Core.Std

exception Type_mismatch
exception Unknown_type
exception Invalid_application

let rec type_of_exp tenv venv = function
	| LetExp (decs, exp) -> type_of_let tenv venv decs exp
	| VarExp (var) -> type_of_var tenv venv var
	| CallExp (name, params) -> type_of_call tenv venv name params
	| OpExp (l_exp, oper, r_exp) -> type_of_oper tenv venv l_exp r_exp oper
	| RecordExp (name, fields) -> type_of_record tenv venv name fields
	| SeqExp (exps) -> type_of_sequence tenv venv exps
	| AssignExp (var, exp) ->  type_of_assign tenv venv exp var
	| IfExp (ifExp) -> type_of_ifExp tenv venv ifExp
	| WhileExp (whileExp) -> type_of_while tenv venv whileExp
	| ForExp (forExp) -> type_of_forExp tenv venv forExp
	| ArrayExp (arrayExp) -> type_of_array tenv venv arrayExp
	| NilExp -> Nil
	| IntExp _ -> Int
	| StringExp _ -> String
	| BreakExp -> Unit

and type_of_let tenv venv decs exp =
	let tenv' = Symbol_table.push_scope tenv [] in
	let venv' = Symbol_table.push_scope venv [] in
	let (tenv'', venv'') = List.fold_left ~f:add_dec_to_env ~init:(tenv', venv') decs in
	type_of_exp tenv'' venv'' exp

and type_of_var tenv venv = function
	| SimpleVar (sym) -> type_of_simple_var venv sym
	| FieldVar (var, sym) -> type_of_fieldVar tenv venv var sym
	| SubscriptVar (var, exp) -> type_of_subscriptVar tenv venv var exp

and type_of_call tenv venv name paramExps =
	let type_of_param = type_of_exp tenv venv in
	let givenParamTyEntries = List.map paramExps ~f:type_of_param in
	match Symbol_table.find venv name with
	| FunEntry (expectedParamTyEntries, returnTyEntry) ->
		let () =
			try
				List.iter2_exn givenParamTyEntries expectedParamTyEntries ~f:check_equal_types
			with
			| Invalid_argument e -> raise Invalid_application in
		return_ty_entry
	| VarEntry -> raise Type_mismatch

and type_of_oper tenv venv lExp rExp oper =
	let lExpTyEntry = type_of_exp tenv venv lExp in
	let rExpTyEntry = type_of_exp tenv venv rExp in
	match oper with
	| Plus | Minus | Mult | Div | And | Or -> type_of_arithmetic_oper lExpTyEntry rExpTyEntry
	| Eq | Neq | Lt | LtEq | Gt | GtEq -> type_of_comparison_oper lExpTyEntry rExpTyEntry

and type_of_record tenv venv name givenFields =
	let recordTyEntry = base_ty (Symbol_table.find tenv name) in
	match recordTyEntry with
	| Record (expectedNameTyEntryPairs, _) ->
		List.iter2_exn expectedNameTyEntryPairs givenFields ~f:(fun (expectedName, expectedTyEntry) givenField ->
			let givenTyEntry = type_of_exp tenv venv givenField.value in
			check_equal_names expectedName givenField.name;
			check_equal_types expectedTyEntry givenTyEntry);
		recordTyEntry
	| Int | String | Array _ | Nil | Unit | Name _ -> raise Type_mismatch

and type_of_sequence tenv venv exps =
	List.fold_left exps Unit ~f:(fun tyEntry exp -> type_of_exp tenv venv exp)

and type_of_assign tenv venv exp var =
	let expTyEntry = type_of_exp tenv venv exp in
	let varTyEntry = type_of_var tenv venv var in
	check_equal_types varTyEntry expTyEntry;
	Unit

and type_of_ifExp tenv venv = function
	| IfExp ifExp -> 
		let testTyEntry = type_of_exp tenv venv ifExp.testExp in
		let thenTyEntry = type_of_exp tenv venv ifExp.thenExp in
		let elseTyEntryOpt = type_of_exp tenv venv ifExp.elseExp in
		check_equal_types testTyEntry Int;
		match elseTyEntryOpt with
		| Some elseTyEntry ->
			check_equal_types thenTyEntry elseTyEntry;
			thenTyEntry
		| None -> thenTyEntry
	| _ -> raise Type_mismatch

and type_of_while tenv venv (test, body) =
	let testTyEntry = type_of_exp tenv venv test in
	let body_ty_entry = type_of_exp tenv venv body in
	check_equal_types testTyEntry Int;
	body_ty_entry

and type_of_forExp tenv venv (sym, lo, hi, body) =
	let loTyEntry = type_of_exp tenv venv lo in
	let hiTyEntry = type_of_exp tenv venv hi in
	check_equal_types loTyEntry Int;
	check_equal_types hiTyEntry Int;
	let venv' = Symbol_table.add sym loTyEntry venv in
	let bodyTyEntry = type_of_exp tenv venv' body in
	check_equal_types bodyTyEntry Unit;
	Unit

and type_of_array tenv venv (typ, sizeExp, initExp) =
	let tyEntry = Symbol_table.find tenv typ in
	match tyEntry with
	| Array (arrayTyEntry, _) ->
		let sizeExpTyEntry = type_of_exp tenv venv sizeExp in
		let initExpTyEntry = type_of_exp tenv venv initExp in
		check_equal_types sizeExpTyEntry Int;
		check_equal_types initExpTyEntry arrayTyEntry;
		base_ty tyEntry
	| _ -> raise Type_mismatch

and type_of_simple_var venv sym =
	match Symbol_table.find venv sym with
	| Some (VarEntry tyEntry) -> tyEntry
	| Some (FunEntry _) -> raise Type_mismatch
	| None -> raise Unknown_type

and type_of_fieldVar tenv venv var fieldSym =
	match type_of_var tenv venv var with
	| Record (symTyEntryPairs, _) ->
		let (sym, tyEntry) = List.find symTyEntryPairs ~f:(fun (sym, tyEntry) -> fieldSym = sym) in
		tyEntry
	| _ -> raise Type_mismatch

and type_of_subscriptVar tenv venv var exp =
	match type_of_var tenv venv var with
	| Array (arrayTyEntry, _) ->
		let expTyEntry = type_of_exp tenv venv exp in
		check_equal_types expTyEntry Int;
		arrayTyEntry
	| _ -> raise Type_mismatch

and type_of_arithmetic_oper lExpTyEntry rExpTyEntry =
	match lExpTyEntry, rExpTyEntry with
	| Int, Int -> Int
	| _, _ -> raise Type_mismatch

and type_of_comparison_oper lExpTyEntry rExpTyEntry =
	match lExpTyEntry, rExpTyEntry with
	| Record _, Record _ ->
		check_equal_types lExpTyEntry rExpTyEntry;
		Int
	| Int, Int -> Int
	| String, String -> Int
	| _, _ -> raise Type_mismatch

and add_dec_to_env (tenv, venv) = function
	| TypeDec (sym, ty) ->
		 let tenv' = add_type_to_env tenv sym ty in
		 (tenv', venv)
	| VarDec (varDec) ->
		 let venv' = add_var_to_env tenv venv varDec in
		 (tenv, venv')
	| FunctionDec (fundec) ->
		 let venv' = add_fun_to_env tenv venv fundec in
		 (tenv, venv')

and add_type_to_env tenv sym = function
	| NameTy (name) -> add_nameTy_to_env sym name tenv
	| RecordTy (fields) -> add_recordTy_to_env sym fields tenv
	| ArrayTy (name) -> add_arrayTy_to_env sym name tenv

and add_var_to_env tenv venv (name, _, typ, init) =
	let init_ty = type_of_exp tenv venv init in
	match typ with
	| Some sym ->
		match Symbol_table.find tenv sym with
		| Some explicit_ty_entry ->
			if explicit_ty_entry = init_ty
			then Symbol_table.add name (VarEntry (init_ty)) venv
			else raise Type_mismatch
		| None -> raise Unknown_type
	| None -> Symbol_table.add name (VarEntry (init_ty)) venv

(* Too big. Too ugly. Refactor *)
and add_fun_to_env tenv venv (name, params, result, body) =
	let name_ty_entry_pairs = List.map params ~f:(fun param -> (param.name, Symbol_table.find tenv param.typ)) in
	let ty_entries = List.map name_ty_entry_pairs ~f:(fun (name, ty_entry) -> ty_entry) in
		List.iter name_ty_entry_pairs ~f:(fun (_, ty_entry) -> if Option.is_none ty_entry then raise Unknown_type);
		let add_to_venv env (name, ty_entry) = Symbol_table.add name ty_entry env in
		let venv' = List.fold_left name_ty_entry_pairs ~init:venv ~f:add_to_venv in
		let body_ty_entry = type_of_exp tenv venv' body in
		match result with
		| Some return_ty ->
			let return_ty_entry = Symbol_table.find tenv return_ty in
				check_equal_types return_ty_entry body_ty_entry;
				let funEntry = FunEntry (ty_entries, return_ty_entry) in
				Symbol_table.add name funEntry venv
		| None ->
			check_equal_types body_ty_entry Unit;
			let funEntry = FunEntry (ty_entries, body_ty_entry) in
			Symbol_table.add name funEntry venv

and add_nameTy_to_env sym name tenv =
	match Symbol_table.find tenv name with
	| Some ty_entry -> Symbol_table.add sym (Name (name, ref ty_entry)) tenv
	| None -> Symbol_table.add sym (Name (name, ref None)) tenv

and add_recordTy_to_env sym fields tenv =
	let field_to_sym_ty_entry_pair field =
		match Symbol_table.find tenv field.typ with
		| Some ty_entry -> (field.name, ty_entry)
		| None -> (field.name, Name (field.typ, ref None)) in
	let sym_ty_entry_pairs = List.map fields ~f:field_to_sym_ty_entry_pair in
	let unique = make_unique () in
	Symbol_table.add sym (Record (sym_ty_entry_pairs, unique)) tenv

and add_arrayTy_to_env sym name tenv =
	match Symbol_table.find tenv name with
	| Some ty_entry ->
		let array_entry = Array (ty_entry, make_unique()) in
		Symbol_table.add sym array_entry tenv
	| None ->
		let array_entry = Array (Name (name, ref None), make_unique ()) in
		Symbol_table.add sym array_entry tenv

and check_equal_types ty1 ty2 =
	let base_ty1 = base_ty ty1 in
	let base_ty2 = base_ty ty2 in
	if base_ty1 <> base_ty2	then raise Type_mismatch

and base_ty = function
	| Int -> Int
	| String -> String
	| Nil -> Nil
	| Unit -> Unit
	| Array (ty_entry, unique) -> (base_ty ty_entry, unique)
	| Name (name, ty_entry_ref) ->
		match !ty_entry_ref with
		| Some ty_entry -> base_ty ty_entry
		| None -> Name (name, ty_entry_ref)
	| Record (name_ty_pairs, unique) ->
		let base_name_ty_pairs = List.map name_ty_pairs ~f:(fun (name, ty_entry) -> (name, base_ty ty_entry)) in
		Record (base_name_ty_pairs, unique)
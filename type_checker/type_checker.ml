open Tiger_types
open Ast
open Core.Std

exception Type_mismatch
exception Unknown_type
exception Invalid_application

let initialTenv =
	let tenv = Symbol_table.push_scope (Symbol_table.empty) [] in
	let intSym = Symbol.of_string "int" in
	let strSym = Symbol.of_string "string" in
	let nilSym = Symbol.of_string "nil" in
	let tenv1 = Symbol_table.add intSym Int tenv in
	let tenv2 = Symbol_table.add strSym String tenv1 in
	let tenv' = Symbol_table.add nilSym Nil tenv2 in
	tenv'

let initialVenv = Symbol_table.empty

let rec type_of_exp tenv venv = function
	| LetExp (decs, exp) -> type_of_let tenv venv decs exp
	| VarExp (var) -> type_of_var tenv venv var
	| CallExp (name, params) -> type_of_call tenv venv name params
	| OpExp (l_exp, oper, r_exp) -> type_of_oper tenv venv l_exp r_exp oper
	| RecordExp (name, fields) -> type_of_record tenv venv name fields
	| SeqExp (exps) -> type_of_sequence tenv venv exps
	| AssignExp (var, exp) ->  type_of_assign tenv venv exp var
	| IfExp (ifExp) -> type_of_if tenv venv ifExp
	| WhileExp (whileExp) -> type_of_while tenv venv whileExp
	| ForExp (forExp) -> type_of_for tenv venv forExp
	| ArrayExp (arrayExp) -> type_of_array tenv venv arrayExp
	| NilExp -> Nil
	| IntExp _ -> Int
	| StringExp _ -> String
	| BreakExp -> Unit

and type_of_let tenv venv decs exp =
	let tenvWithEmptyScope = Symbol_table.push_scope tenv [] in
	let venvWithEmptyScope = Symbol_table.push_scope venv [] in
	let (tenv', venv') = List.fold_left decs ~f:add_dec_to_env ~init:(tenvWithEmptyScope, venvWithEmptyScope) in
	type_of_exp tenv' venv' exp

and type_of_var tenv venv = function
	| SimpleVar (sym) -> base_ty (type_of_simple_var venv sym)
	| FieldVar (var, sym) -> base_ty (type_of_fieldVar tenv venv var sym)
	| SubscriptVar (var, exp) -> base_ty (type_of_subscriptVar tenv venv var exp)

and type_of_call tenv venv funSym paramExps =
	let type_of_param = type_of_exp tenv venv in
	let givenParamTyEntries = List.map paramExps ~f:type_of_param in
	match Symbol_table.find venv funSym with
	| Some (FunEntry (expectedParamTyEntries, returnTyEntry)) ->
		let () =
			try
				List.iter2_exn givenParamTyEntries expectedParamTyEntries ~f:check_equal_types
			with
			| Invalid_argument e -> raise Invalid_application in
		returnTyEntry	
	| Some (VarEntry _) -> raise Type_mismatch
	| None -> raise Unknown_type

and type_of_oper tenv venv lExp rExp oper =
	let lExpTyEntry = type_of_exp tenv venv lExp in
	let rExpTyEntry = type_of_exp tenv venv rExp in
	match oper with
	| Plus | Minus | Mult | Div | And | Or -> type_of_arithmetic_oper lExpTyEntry rExpTyEntry
	| Eq | Neq | Lt | LtEq | Gt | GtEq -> type_of_comparison_oper lExpTyEntry rExpTyEntry

and type_of_record tenv venv recSym givenFields =
	match Symbol_table.find tenv recSym with
	| Some tyEntry ->
		let baseTyEntry = base_ty tyEntry in
		(match baseTyEntry with
		| Record (expectedSymTyEntryPairs, _) ->
			let check_equal_field (expectedSym, expectedTyEntry) givenField =
				let givenTyEntry = type_of_exp tenv venv givenField.value in
				if (givenField.name <> expectedSym) then raise Unknown_type;
				check_equal_types expectedTyEntry givenTyEntry in
			List.iter2_exn expectedSymTyEntryPairs givenFields ~f:check_equal_field;
			baseTyEntry
		| _ -> raise Type_mismatch)
	| None -> raise Unknown_type

and type_of_sequence tenv venv exps =
	List.fold_left exps ~init:Unit ~f:(fun tyEntry exp -> type_of_exp tenv venv exp)

and type_of_assign tenv venv exp var =
	let expTyEntry = type_of_exp tenv venv exp in
	let varTyEntry = type_of_var tenv venv var in
	check_equal_types varTyEntry expTyEntry;
	Unit

and type_of_if tenv venv {testExp; thenExp; elseExp = elseExpOpt} =
	let testTyEntry = type_of_exp tenv venv testExp in
	let thenTyEntry = type_of_exp tenv venv thenExp in
	check_equal_types testTyEntry Int;
	match elseExpOpt with
	| Some elseExp ->
		let elseTyEntry = type_of_exp tenv venv elseExp in
		check_equal_types thenTyEntry elseTyEntry;
		base_ty thenTyEntry
	| None -> base_ty thenTyEntry

and type_of_while tenv venv {test; body} =
	let testTyEntry = type_of_exp tenv venv test in
	let bodyTyEntry = type_of_exp tenv venv body in
	check_equal_types testTyEntry Int;
	check_equal_types bodyTyEntry Unit;
	Unit

and type_of_for tenv venv forExp =
	let loTyEntry = type_of_exp tenv venv forExp.lo in
	let hiTyEntry = type_of_exp tenv venv forExp.hi in
	check_equal_types loTyEntry Int;
	check_equal_types hiTyEntry Int;
	let venv' = Symbol_table.add forExp.var (VarEntry Int) venv in
	let bodyTyEntry = type_of_exp tenv venv' forExp.body in
	check_equal_types bodyTyEntry Unit;
	Unit

and type_of_array tenv venv {typ; size; init} =
	match Symbol_table.find tenv typ with
	| Some tyEntry ->
		(match base_ty tyEntry with
		| Array (arrayTyEntry, unique) ->
			let sizeTyEntry = type_of_exp tenv venv size in
			let initTyEntry = type_of_exp tenv venv init in
			check_equal_types sizeTyEntry Int;
			check_equal_types initTyEntry arrayTyEntry;
			Array (arrayTyEntry, unique)
		| _ -> raise Type_mismatch)
	| None -> raise Unknown_type

and type_of_simple_var venv sym =
	match Symbol_table.find venv sym with
	| Some (VarEntry tyEntry) -> tyEntry
	| Some (FunEntry _) -> raise Type_mismatch
	| None -> raise Unknown_type

and type_of_fieldVar tenv venv var fieldSym =
	match type_of_var tenv venv var with
	| Record (symTyEntryPairs, _) ->
		let symTyEntryPairOpt = List.find symTyEntryPairs ~f:(fun (sym, _) -> fieldSym = sym) in
		(match symTyEntryPairOpt with
		| Some (_, tyEntry) -> tyEntry
		| None -> raise Unknown_type)
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
	| TypeDec (tySym, ty) ->
		 let tenv' = add_type_to_env tenv tySym ty in
		 (tenv', venv)
	| VarDec (varDec) ->
		 let venv' = add_var_to_env tenv venv varDec in
		 (tenv, venv')
	| FunctionDec (fundec) ->
		 let venv' = add_fun_to_env tenv venv fundec in
		 (tenv, venv')

and add_type_to_env tenv tySym = function
	| NameTy (originalTySym) -> add_nameTy_to_env tySym originalTySym tenv
	| RecordTy (tyFields) -> add_recordTy_to_env tySym tyFields tenv
	| ArrayTy (arrayTySym) -> add_arrayTy_to_env tySym arrayTySym tenv

and add_var_to_env tenv venv varDec =
	let initTyEntry = type_of_exp tenv venv varDec.init in
	match varDec.typ with
	| Some tySym ->
		(match Symbol_table.find tenv tySym with
		| Some explicitTyEntry ->
			check_equal_types initTyEntry explicitTyEntry;
			Symbol_table.add varDec.name (VarEntry initTyEntry) venv
		| None -> raise Unknown_type)
	| None -> Symbol_table.add varDec.name (VarEntry initTyEntry) venv

and add_fun_to_env tenv venv funDec =
	let paramTyEntries = List.map funDec.params ~f:(fun param -> 
		match Symbol_table.find tenv param.typ with
		| Some paramTyEntry -> paramTyEntry
		| None -> raise Unknown_type) in
	let venvWithEmptyScope = Symbol_table.push_scope venv [] in
	let venv' = List.fold2_exn funDec.params paramTyEntries ~init:venvWithEmptyScope ~f:(fun venvAcc param paramTyEntry ->
		Symbol_table.add param.name (VarEntry paramTyEntry) venvAcc) in
	let bodyTyEntry = type_of_exp tenv venv' funDec.body in
	let funEntry = FunEntry (paramTyEntries, bodyTyEntry) in
	match funDec.result with
	| Some returnTySym ->
		(match Symbol_table.find tenv returnTySym with
		| Some returnTyEntry ->
			check_equal_types bodyTyEntry returnTyEntry;
			Symbol_table.add funDec.name funEntry venv
		| None -> raise Unknown_type)
	| None -> Symbol_table.add funDec.name funEntry venv

and add_nameTy_to_env tySym originalTySym tenv =
	match Symbol_table.find tenv originalTySym with
	| Some originalTyEntry -> Symbol_table.add tySym (Name (originalTySym, ref (Some originalTyEntry))) tenv
	| None -> Symbol_table.add tySym (Name (originalTySym, ref None)) tenv

and add_recordTy_to_env tySym tyFields tenv =
	let tyField_to_symTyEntryPair (tyField : recordTyField) =
		match Symbol_table.find tenv tyField.typ with
		| Some fieldTyEntry -> (tyField.name, fieldTyEntry)
		| None -> (tyField.name, Name (tyField.typ, ref None)) in
	let symTyEntryPairs = List.map tyFields ~f:tyField_to_symTyEntryPair in
	let recordTyEntry = Record (symTyEntryPairs, make_unique ()) in
	Symbol_table.add tySym recordTyEntry tenv

and add_arrayTy_to_env tySym arrayTySym tenv =
	match Symbol_table.find tenv arrayTySym with
	| Some tyEntry ->
		let arrayTyEntry = Array (tyEntry, make_unique()) in
		Symbol_table.add tySym arrayTyEntry tenv
	| None ->
		let arrayTyEntry = Array (Name (arrayTySym, ref None), make_unique ()) in
		Symbol_table.add tySym arrayTyEntry tenv

and check_equal_types ty1 ty2 =
	let base_ty1 = base_ty ty1 in
	let base_ty2 = base_ty ty2 in
	if base_ty1 <> base_ty2	then raise Type_mismatch

and base_ty = function
	| Int -> Int
	| String -> String
	| Nil -> Nil
	| Unit -> Unit
	| Array (ty_entry, unique) -> Array (base_ty ty_entry, unique)
	| Name (name, ty_entry_ref) ->
		(match !ty_entry_ref with
		| Some ty_entry -> base_ty ty_entry
		| None -> Name (name, ty_entry_ref))
	| Record (name_ty_pairs, unique) ->
		let base_name_ty_pairs = List.map name_ty_pairs ~f:(fun (name, ty_entry) -> (name, base_ty ty_entry)) in
		Record (base_name_ty_pairs, unique)
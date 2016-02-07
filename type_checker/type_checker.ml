open Tiger_types
open Ast
open Core.Std

module St = Symbol_table

exception Type_mismatch
exception Unknown_type
exception Invalid_application

let initialTenv =
	let tenv = St.push_scope (St.empty) [] in
	let intSym = Symbol.of_string "int" in
	let strSym = Symbol.of_string "string" in
	let nilSym = Symbol.of_string "nil" in
	let tenv1 = St.add intSym Int tenv in
	let tenv2 = St.add strSym String tenv1 in
	let tenv' = St.add nilSym Nil tenv2 in
	tenv'

let initialVenv = St.empty

let rec type_of_prog exp =
	type_of_exp initialTenv initialVenv exp

and type_of_exp tenv venv = function
	| LetExp (decs, exp) -> type_of_let tenv venv decs exp
	| VarExp (var) -> type_of_var tenv venv var
	| CallExp (name, params) -> type_of_call tenv venv name params
	| OpExp (l_exp, oper, r_exp) -> type_of_oper tenv venv l_exp r_exp oper
	| RecordExp (name, fields) -> type_of_record tenv venv name fields
	| SeqExp (exps) -> type_of_sequence tenv venv exps
	| AssignExp (var, exp) -> type_of_assign tenv venv exp var
	| IfExp (ifExp) -> type_of_if tenv venv ifExp
	| WhileExp (whileExp) -> type_of_while tenv venv whileExp
	| ForExp (forExp) -> type_of_for tenv venv forExp
	| ArrayExp (arrayExp) -> type_of_array tenv venv arrayExp
	| NilExp -> (Nil, tenv, venv)
	| IntExp _ -> (Int, tenv, venv)
	| StringExp _ -> (String, tenv, venv)
	| BreakExp -> (Unit, tenv, venv)

and type_of_let tenv venv decs exp =
	let tenvWithEmptyScope = St.push_scope tenv [] in
	let venvWithEmptyScope = St.push_scope venv [] in
	let (tenv', venv') = List.fold_left decs ~f:add_dec_to_env ~init:(tenvWithEmptyScope, venvWithEmptyScope) in
	let (tyEntry, _, _) = type_of_exp tenv' venv' exp in
	(tyEntry, tenv', venv')

and type_of_var tenv venv = function
	| SimpleVar (sym) ->
		let tyEntry = base_ty (type_of_simple_var venv sym) in
		(tyEntry, tenv, venv)
	| FieldVar (var, sym) -> 
		let tyEntry = base_ty (type_of_fieldVar tenv venv var sym) in
		(tyEntry, tenv, venv)
	| SubscriptVar (var, exp) ->
		let tyEntry = base_ty (type_of_subscriptVar tenv venv var exp) in
		(tyEntry, tenv, venv)

and type_of_call tenv venv funSym paramExps =
	let givenParamTyEntries = List.map paramExps ~f:(fun paramExp ->
		let (paramTyEntry, _, _) = type_of_exp tenv venv paramExp in
		paramTyEntry) in
	match St.find venv funSym with
	| Some (FunEntry (expectedParamTyEntries, returnTyEntry)) ->
		let () =
			try
				List.iter2_exn givenParamTyEntries expectedParamTyEntries ~f:check_equal_types
			with
			| Invalid_argument e -> raise Invalid_application in
		(returnTyEntry, tenv, venv)	
	| Some (VarEntry _) -> raise Type_mismatch
	| None -> raise Unknown_type

and type_of_oper tenv venv lExp rExp oper =
	let (lExpTyEntry, _, _) = type_of_exp tenv venv lExp in
	let (rExpTyEntry, _, _) = type_of_exp tenv venv rExp in
	match oper with
	| Plus | Minus | Mult | Div | And | Or ->
		let tyEntry = type_of_arithmetic_oper lExpTyEntry rExpTyEntry in
		(tyEntry, tenv, venv)
	| Eq | Neq | Lt | LtEq | Gt | GtEq ->
		let tyEntry = type_of_comparison_oper lExpTyEntry rExpTyEntry in
		(tyEntry, tenv, venv)

and type_of_record tenv venv recSym givenFields =
	match St.find tenv recSym with
	| Some tyEntry ->
		let baseTyEntry = base_ty tyEntry in
		(match baseTyEntry with
		| Record (expectedSymTyEntryPairs, _) ->
			let check_equal_field (expectedSym, expectedTyEntry) givenField =
				let (givenTyEntry, _, _) = type_of_exp tenv venv givenField.value in
				if (givenField.name <> expectedSym) then raise Unknown_type;
				check_equal_types expectedTyEntry givenTyEntry in
			List.iter2_exn expectedSymTyEntryPairs givenFields ~f:check_equal_field;
			(baseTyEntry, tenv, venv)
		| _ -> raise Type_mismatch)
	| None -> raise Unknown_type

and type_of_sequence tenv venv exps =
	List.fold_left exps ~init:(Unit, tenv, venv) ~f:(fun _ exp -> type_of_exp tenv venv exp)

and type_of_assign tenv venv exp var =
	let (expTyEntry, _, _) = type_of_exp tenv venv exp in
	let (varTyEntry, _, _) = type_of_var tenv venv var in
	check_equal_types varTyEntry expTyEntry;
	(Unit, tenv, venv)

and type_of_if tenv venv {testExp; thenExp; elseExp = elseExpOpt} =
	let (testTyEntry, _, _) = type_of_exp tenv venv testExp in
	let (thenTyEntry, _, _) = type_of_exp tenv venv thenExp in
	check_equal_types testTyEntry Int;
	match elseExpOpt with
	| Some elseExp ->
		let (elseTyEntry, _, _) = type_of_exp tenv venv elseExp in
		check_equal_types thenTyEntry elseTyEntry;
		(base_ty thenTyEntry, tenv, venv)
	| None -> (Unit, tenv, venv)

and type_of_while tenv venv {test; body} =
	let (testTyEntry, _, _) = type_of_exp tenv venv test in
	let (bodyTyEntry, _, _) = type_of_exp tenv venv body in
	check_equal_types testTyEntry Int;
	check_equal_types bodyTyEntry Unit;
	(Unit, tenv, venv)

and type_of_for tenv venv forExp =
	let (loTyEntry, _, _) = type_of_exp tenv venv forExp.lo in
	let (hiTyEntry, _, _) = type_of_exp tenv venv forExp.hi in
	check_equal_types loTyEntry Int;
	check_equal_types hiTyEntry Int;
	let venv' = St.add forExp.var (VarEntry Int) venv in
	let (bodyTyEntry, _, _) = type_of_exp tenv venv' forExp.body in
	check_equal_types bodyTyEntry Unit;
	(Unit, tenv, venv)

and type_of_array tenv venv {typ; size; init} =
	match St.find tenv typ with
	| Some tyEntry ->
		(match base_ty tyEntry with
		| Array (arrayTyEntry, unique) ->
			let (sizeTyEntry, _, _) = type_of_exp tenv venv size in
			let (initTyEntry, _, _) = type_of_exp tenv venv init in
			check_equal_types sizeTyEntry Int;
			check_equal_types initTyEntry arrayTyEntry;
			(Array (arrayTyEntry, unique), tenv, venv)
		| _ -> raise Type_mismatch)
	| None -> raise Unknown_type

and type_of_simple_var venv sym =
	match St.find venv sym with
	| Some (VarEntry tyEntry) -> tyEntry
	| Some (FunEntry _) -> raise Type_mismatch
	| None -> raise Unknown_type

and type_of_fieldVar tenv venv var fieldSym =
	let (varTyEntry, _, _) = type_of_var tenv venv var in
	match varTyEntry with
	| Record (symTyEntryPairs, _) ->
		let symTyEntryPairOpt = List.find symTyEntryPairs ~f:(fun (sym, _) -> fieldSym = sym) in
		(match symTyEntryPairOpt with
		| Some (_, tyEntry) -> tyEntry
		| None -> raise Unknown_type)
	| _ -> raise Type_mismatch

and type_of_subscriptVar tenv venv var exp =
	let (varTyEntry, _, _) = type_of_var tenv venv var in
	match varTyEntry with
	| Array (arrayTyEntry, _) ->
		let (expTyEntry, _, _) = type_of_exp tenv venv exp in
		check_equal_types expTyEntry Int;
		arrayTyEntry
	| _ -> raise Type_mismatch

and type_of_arithmetic_oper lExpTyEntry rExpTyEntry =
	match lExpTyEntry, rExpTyEntry with
	| Int, Int -> Int
	| _, _ -> raise Type_mismatch

(* Should only be able to compare two records for equality and inequality. *)
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
	let (initTyEntry, _, _) = type_of_exp tenv venv varDec.init in
	match varDec.typ with
	| Some tySym ->
		(match St.find tenv tySym with
		| Some explicitTyEntry ->
			check_equal_types initTyEntry explicitTyEntry;
			St.add varDec.name (VarEntry initTyEntry) venv
		| None -> raise Unknown_type)
	| None -> St.add varDec.name (VarEntry initTyEntry) venv

and add_fun_to_env tenv venv funDec =
	let paramTyEntries = List.map funDec.params ~f:(fun param -> 
		match St.find tenv param.typ with
		| Some paramTyEntry -> paramTyEntry
		| None -> raise Unknown_type) in
	let venvWithEmptyScope = St.push_scope venv [] in
	let venv' = List.fold2_exn funDec.params paramTyEntries ~init:venvWithEmptyScope ~f:(fun venvAcc param paramTyEntry ->
		St.add param.name (VarEntry paramTyEntry) venvAcc) in
	let (bodyTyEntry, _, _) = type_of_exp tenv venv' funDec.body in
	let funEntry = FunEntry (paramTyEntries, bodyTyEntry) in
	match funDec.result with
	| Some returnTySym ->
		(match St.find tenv returnTySym with
		| Some returnTyEntry ->
			check_equal_types bodyTyEntry returnTyEntry;
			St.add funDec.name funEntry venv
		| None -> raise Unknown_type)
	| None -> St.add funDec.name funEntry venv

and add_nameTy_to_env tySym originalTySym tenv =
	match St.find tenv originalTySym with
	| Some originalTyEntry -> St.add tySym (Name (originalTySym, ref (Some originalTyEntry))) tenv
	| None -> St.add tySym (Name (originalTySym, ref None)) tenv

and add_recordTy_to_env tySym tyFields tenv =
	let tyField_to_symTyEntryPair (tyField : recordTyField) =
		match St.find tenv tyField.typ with
		| Some fieldTyEntry -> (tyField.name, fieldTyEntry)
		| None -> (tyField.name, Name (tyField.typ, ref None)) in
	let symTyEntryPairs = List.map tyFields ~f:tyField_to_symTyEntryPair in
	let recordTyEntry = Record (symTyEntryPairs, make_unique ()) in
	St.add tySym recordTyEntry tenv

and add_arrayTy_to_env tySym arrayTySym tenv =
	match St.find tenv arrayTySym with
	| Some tyEntry ->
		let arrayTyEntry = Array (tyEntry, make_unique()) in
		St.add tySym arrayTyEntry tenv
	| None ->
		let arrayTyEntry = Array (Name (arrayTySym, ref None), make_unique ()) in
		St.add tySym arrayTyEntry tenv

and add_params_to_env tenv venv (params : formalParam list) =
	let symTyEntryPairs = List.map params ~f:(fun param ->
		match St.find tenv param.typ with
		| Some tyEntry -> (param.name, tyEntry)
		| None -> raise Unknown_type) in
	St.push_scope venv symTyEntryPairs

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
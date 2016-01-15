(* Traverses AST and sets escape values for formal parameters and local variables. A parameter has 'escaped' if it gets
   used in a function nested deeper than the current scope *)

type entry =
	| FormalParam of Ast.formalParam
	| LocalVar of Ast.varDec

let rec set_escapes exp =
	set_escapes_with_env Symbol_table.empty exp

and set_escapes_with_env env = function
	| LetExp (decs, exp) -> set_letExp_escapes env decs exp
	| VarExp (var) -> set_varExp_escapes env var
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


and set_letExp_escapes env decs exp =
	let augmentedEnv = List.fold_left decs ~init:env ~f:set_dec_escapes_and_augment_env in
	set_escapes_with_env augmentedEnv exp

(* For a variable declaration, calculate escapes for init exp in current scope, then add variable declaration to current scope.
   For a function declaration, create new scope and add formal params to new scope, calculate escapes for function body in new scope, then remove scope *)
and set_dec_escapes_and_augment_env env = function
	| VarDec varDec ->
		set_escapes_with_env env init;
		Symbol_table.add varDec.name (LocalVar varDec) env
	| FunDec funDec ->
		let formalParamAssocList = List.map funDec.params ~f:(fun param -> (param.name, param)) in
		let functionEnv = Symbol_table.push env formalParamAssocList in
		set_escapes_with_env functionEnv funDec.body;
		functionEnv.pop_scope

and set_varExp_escapes env = function
	| SimpleVar sym -> set_simpleVar_escape env sym 
	| FieldVar (var, sym) ->
	| SubscriptVar (var, exp) ->

and set_simpleVar_escape env sym =
	if not (Symbol_table.in_current_scope sym env)
	then match Symbol_table.find env sym with
	| Some (FormalParam param) -> param.escape := true
	| None -> assert false

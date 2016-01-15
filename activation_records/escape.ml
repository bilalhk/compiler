(* Traverses AST and sets escape values for formal parameters and local variables. A parameter has 'escaped' if it gets
   used in a function nested deeper than the current scope *)

type entry =
	| FormalParam of Ast.formalParam
	| LocalVar of Ast.varDec
	| ForVar of Ast.forExp

let rec set_escapes exp =
	set_escapes_with_env Symbol_table.empty exp

and set_escapes_with_env env = function
	| LetExp (decs, exp) -> set_letExp_escapes env decs exp
	| VarExp (var) -> set_varExp_escape env var
	| CallExp (name, params) -> set_callExp_escapes env params
	| OpExp (l_exp, oper, r_exp) -> set_opExp_escapes env l_exp r_exp
	| RecordExp (name, fields) -> ()
	| SeqExp (exps) -> set_seqExp_escapes env exps
	| AssignExp (var, exp) ->  set_assignExp_escapes env var exp
	| IfExp (ifExp) -> set_ifExp_escapes env ifExp
	| WhileExp (whileExp) -> set_whileExp_escapes env whileExp
	| ForExp (forExp) -> set_forExp_escapes env forExp
	| ArrayExp (arrayExp) -> set_arrayExp_escapes env arrayExp
	| NilExp -> ()
	| IntExp _ -> ()
	| StringExp _ -> ()
	| BreakExp -> ()


and set_letExp_escapes env decs exp =
	let augmentedEnv = List.fold_left decs ~init:env ~f:set_dec_escapes_and_augment_env in
	set_escapes_with_env augmentedEnv exp

and set_callExp_escapes env params =
	List.iter params ~f:(fun exp -> set_escapes_with_env env exp)

and set_opExp_escapes env l_exp r_exp =
	set_escapes_with_env env l_exp;
	set_escapes_with_env env r_exp;

and set_seqExp_escapes env exps =
	List.iter exps ~f:(fun exp -> set_escapes_with_env env exp)

and set_assignExp_escapes env var exp =
	set_escapes_with_env env exp;
	set_varExp_escape env var

and set_ifExp_escapes env {testExp; thenExp; elseExp} =
	set_escapes_with_env env testExp;
	set_escapes_with_env env thenExp;
	match elseExp with
	| Some exp -> set_escapes_with_env env exp
	| None _ -> ()

and set_whileExp_escapes env {test; body} =
	set_escapes_with_env env test;
	set_escapes_with_env env body

and set_forExp_escapes env forExp =
	set_escapes_with_env env forExp.lo;
	set_escapes_with_env env forExp.hi;
	let augmentedEnv = Symbol_table.add forExp.var (ForVar forExp) env in
	set_escapes_with_env augmentedEnv forExp.body

and set_arrayExp_escapes env {typ; size = sizeExp; init = initExp} =
	set_escapes_with_env env sizeExp;
	set_escapes_with_env env initExp

(* For a variable declaration, calculate escapes for init exp in current scope, then add variable declaration to current scope.
   For a function declaration, create new scope and add formal params to new scope, calculate escapes for function body in new scope, then remove scope *)
and set_dec_escapes_and_augment_env env = function
	| VarDec varDec ->
		set_escapes_with_env env varDec.init;
		Symbol_table.add varDec.name (LocalVar varDec) env
	| FunDec funDec ->
		let formalParamAssocList = List.map funDec.params ~f:(fun param -> (param.name, param)) in
		let functionEnv = Symbol_table.push env formalParamAssocList in
		set_escapes_with_env functionEnv funDec.body;
		functionEnv.pop_scope

and set_varExp_escape env = function
	| SimpleVar sym -> set_simpleVar_escape env sym 
	| FieldVar (var, sym) -> set_varExp_escape env var
	| SubscriptVar (var, exp) -> set_subscriptVar_escape env var exp

and set_simpleVar_escape env sym =
	if not (Symbol_table.in_current_scope sym env)
	then match Symbol_table.find env sym with
		 | Some (FormalParam param) -> param.escape := true
		 | Some (LocalVar varDec) -> varDec.escape := true
		 | Some (ForVar forExp) -> forExp.escape := true
		 | None -> assert false

and set_subscriptVar_escape env var exp =
	set_escapes_with_env env exp;
	set_varExp_escape env var
module T = Tree
module Tt = Tiger_types
module Tc = Type_checker
module L = Level
module St = Symbol_table
module F = Frame

open Core.Std

type transEntry =
	| VarEntry of L.access
	| FunEntry of L.t

type exp =
	| Ex of T.exp
	| Nx of T.stm
	| Cx of (Temp.label -> Temp.label -> T.stm)

type fragment = 
	| Proc of T.stm * L.t
	| String of Temp.label * string

let fragments = ref []

let initialTransEnv = St.empty

let unEx = function
	| Ex exp -> exp
	| Nx stm -> T.ESeq (stm, T.Const 0)
	| Cx genStm ->
		let returnReg = T.Temp (Temp.new_reg ()) in
		let tLabel = Temp.new_label () in
		let fLabel = Temp.new_label () in
		T.ESeq (T.Seq (T.Move (returnReg, T.Const 0),
					   T.Seq (genStm tLabel fLabel,
					   		  T.Seq (T.Label tLabel,
					   		  		 T.Seq (T.Move (returnReg, T.Const 1),
					   		  				T.Label fLabel)))),
				returnReg)

let unNx = function
	| Ex exp -> T.Exp exp
	| Nx stm -> stm
	| Cx genStm ->
		let tLabel = Temp.new_label () in
		let fLabel = Temp.new_label () in
		T.Seq (genStm tLabel fLabel,
			   T.Seq (T.Label tLabel, T.Label fLabel))

let unCx = function
	| Ex (T.Const x) ->
		if x = 0 then
			fun tLabel fLabel -> T.Jump (T.Name fLabel, [fLabel])
		else
			fun tLabel fLabel -> T.Jump (T.Name tLabel, [tLabel])
	| Ex exp -> fun tLabel fLabel -> T.CJump (T.EQ, exp, T.Const 0, fLabel, tLabel)
	| Cx genStm -> genStm
	| Nx _ -> assert false

let rec translate_prog exp =
	let (_, typeEnv, varEnv) = Tc.type_of_prog exp in
	translate_exp L.mainLevel (initialTransEnv, typeEnv, varEnv) None exp

and translate_exp level envs breakLabel = function
	| VarExp var -> translate_var level envs breakLabel var
	| IntExp num -> translate_int num
	| StringExp str -> assert false (* Will handle later *)
	| CallExp (funSym, args) -> translate_call_exp level envs funSym args breakLabel
	| OpExp (lExp, oper, rExp) -> translate_op_exp level envs lExp rExp oper breakLabel
	| RecordExp (recSym, fields) -> translate_record_exp level envs breakLabel recSym fields
	| SeqExp exps -> translate_seq_exp level envs exps breakLabel
	| AssignExp (var, exp) -> translate_assign_exp level envs var exp breakLabel
	| IfExp ifExp -> translate_if_exp level envs ifExp breakLabel
	| WhileExp whileExp -> translate_while_exp level envs whileExp breakLabel
	| ForExp forExp -> translate_for_exp level envs breakLabel forExp
	| LetExp (decs, exp) -> translate_let_exp level envs breakLabel decs exp
	| ArrayExp arrayExp -> translate_array_exp level envs breakLabel arrayExp
	| BreakExp -> translate_break_exp breakLabel
	| NilExp -> translate_nil_exp ()

and translate_int num =
	Ex (T.Const num)

and translate_var level (transEnv, typeEnv, varEnv) breakLabel = function
	| SimpleVar sym -> translate_simpleVar level transEnv sym
	| FieldVar (var, sym) -> translate_fieldVar level (transEnv, typeEnv, varEnv) var sym breakLabel
	| SubscriptVar (var, exp) -> translate_subscriptVar level (transEnv, typeEnv, varEnv) var exp breakLabel

and translate_call_exp level envs funSym args breakLabel =
	let translatedArgs = List.map args ~f:(fun argExp -> unEx (translate_exp level envs breakLabel argExp)) in
	let fragment = List.find !fragments ~f:(fun frag ->
		match frag with
		| Proc (_, level) -> level.name = funSym
		| String _ -> false) in
	match fragment with
	| Some (Proc (stm, funLevel)) ->
		let static_link_address = create_static_link funLevel level in
		let augmentedArgs = static_link_address::translatedArgs in
		Ex (T.Call (T.Name funLevel.label, augmentedArgs))
	| Some (String _) | None -> assert false

and translate_record_exp level envs breakLabel recSym fields =
	let fieldValueExps = List.map fields ~f:(fun field -> field.value) in
	let translatedFieldValues = List.map fieldValueExps ~f:(fun fieldValue -> unEx (translate_exp level envs breakLabel fieldValue)) in
	Ex (F.external_call "initRecord" translatedFieldValues)

and translate_array_exp level envs breakLabel {typ = tySym; size = sizeExp; init = initExp} =
	let translatedSize = unEx (translate_exp level envs breakLabel sizeExp) in
	let translatedInit = unEx (translate_exp level envs breakLabel initExp) in
	let translatedArgs = [translatedSize; translatedInit] in
	Ex (F.external_call "initArray" translatedArgs)

and create_static_link funLevel callerLevel =
	let rec link_to_def_level defLevel currentLevel currentFpAddress =
		if L.equal currentLevel defLevel then
			currentFpAddress
		else
			match currentLevel.parent with
			| Some parentLevel -> link_to_def_level defLevel parentLevel (T.Mem currentFpAddress)
			| None -> assert false in
	match funLevel.parent with
	| Some defLevel -> link_to_def_level defLevel callerLevel (T.Temp F.fp)
	| None -> assert false

and translate_op_exp level (transEnv, typeEnv, varEnv) lExp rExp op breakLabel =
	let translatedLExp = translate_exp level (transEnv, typeEnv, varEnv) breakLabel lExp in
	let translatedRExp = translate_exp level (transEnv, typeEnv, varEnv) breakLabel rExp in
	match op with
	| Plus | Minus | Mult | Div -> translate_arithmetic_op translatedLExp translatedRExp op
	| Eq | Neq | Lt | LtEq | Gt | GtEq -> translate_comparison_op translatedLExp translatedRExp op
	| And | Or -> translate_boolean_op translatedLExp translatedRExp op

(* Incorrect. Need to translate into T.ESEQ *)
and translate_seq_exp level envs exps breakLabel =
	List.fold_left exps ~init:(Ex (T.Const 0)) ~f:(fun _ exp -> translate_exp level envs breakLabel exp)

and translate_assign_exp level envs var exp breakLabel =
	let varLocation = unEx (translate_var level envs breakLabel var) in
	let translatedExp = unEx (translate_exp level envs breakLabel exp) in
	Nx (T.Move (varLocation, translatedExp))

(* Can be optimized for the case of Nx test or Cx branches. *)
and translate_if_exp level envs {testExp; thenExp; elseExp = elseExpOpt} breakLabel =
	let conditional = unCx (translate_exp level envs breakLabel testExp) in
	let translatedThenExp = unEx (translate_exp level envs breakLabel thenExp) in
	let trueLabel = Temp.new_label () in
	let falseLabel = Temp.new_label () in
	let joinLabel = Temp.new_label () in
	let resultReg = Temp.new_reg () in
	let trueBranch = T.Seq (T.Label trueLabel,
							T.Seq (T.Move (T.Temp resultReg, translatedThenExp),
								   T.Jump (T.Name joinLabel, [joinLabel]))) in
	match elseExpOpt with
	| Some elseExp ->
		let translatedElseExp = unEx (translate_exp level envs breakLabel elseExp) in
		let falseBranch = T.Seq (T.Label falseLabel,
								 T.Seq (T.Move (T.Temp resultReg, translatedElseExp),
										T.Jump (T.Name joinLabel, [joinLabel]))) in
		let translatedIfExp = T.ESeq (T.Seq (conditional trueLabel falseLabel,
											 T.Seq (trueBranch,
											 		T.Seq (falseBranch, T.Label joinLabel))),
									  T.Temp resultReg) in
		Ex translatedIfExp
	| None ->
		let falseBranch = T.Exp (T.Const 0) in
		let translatedIfExp = T.Seq (conditional trueLabel falseLabel,
							  		 T.Seq (trueBranch,
											T.Seq (falseBranch, T.Label joinLabel))) in
		Nx translatedIfExp

and translate_while_exp level envs {test = testExp; body = bodyExp} breakLabel =
	let conditional = unCx (translate_exp level envs breakLabel testExp) in
	let testLabel = Temp.new_label () in
	let bodyLabel = Temp.new_label () in
	let doneLabel = Temp.new_label () in
	let translatedBodyExp = unNx (translate_exp level envs (Some doneLabel) bodyExp) in
	let bodyBranch = T.Seq (T.Label bodyLabel,
							T.Seq (translatedBodyExp, T.Jump (T.Name testLabel, [testLabel]))) in
	let translatedWhileExp = T.Seq (conditional bodyLabel doneLabel,
		   							T.Seq (bodyBranch, T.Label doneLabel)) in
	Nx translatedWhileExp

and translate_break_exp = function
	| Some label -> Nx (T.Jump (T.Name label, [label]))
	| None -> assert false

and translate_for_exp level envs breakLabel {var; escape; lo = loExp; hi = hiExp; body = bodyExp} =
	let loReg = T.Temp (Temp.new_reg ()) in
	let testLabel = Temp.new_label () in
	let bodyLabel = Temp.new_label () in
	let doneLabel = Temp.new_label () in
	let translatedBodyExp = unNx (translate_exp level envs breakLabel bodyExp) in
	let translatedLoExp = unEx (translate_exp level envs breakLabel loExp) in
	let translatedHiExp = unEx (translate_exp level envs breakLabel hiExp) in
	let conditionalJump = T.Seq (T.Label testLabel, T.CJump (T.LE, loReg, translatedHiExp, bodyLabel, doneLabel)) in
	let indexInitialization = T.Move (loReg, translatedLoExp) in
	let incrementIndex = T.Move (loReg, T.Binop (T.PLUS, loReg, T.Const 1)) in
	let bodyBranch = T.Seq (T.Label bodyLabel, translatedBodyExp) in
	let translatedForExp = T.Seq (indexInitialization,
								  T.Seq (conditionalJump,
								  		 T.Seq (bodyBranch,
								  		 		T.Seq (incrementIndex,
								  		 			   T.Seq (T.Jump (T.Name testLabel, [testLabel]),
								  		 			   		  T.Label doneLabel))))) in
	Nx translatedForExp

and translate_let_exp level (transEnv, typeEnv, varEnv) breakLabel decs exp =
	let (_, typeEnv', varEnv') = Tc.type_of_exp typeEnv varEnv exp in
	let translate_dec (transEnv, translatedDecs) = function
		| Ast.FunctionDec funDec -> translate_fun_dec level (transEnv, typeEnv', varEnv') translatedDecs breakLabel funDec
		| VarDec varDec -> translate_var_dec level (transEnv, typeEnv', varEnv') translatedDecs breakLabel varDec
		| TypeDec _ -> (transEnv, translatedDecs) in
	let (transEnv', translatedDecs) = List.fold_left decs ~init:(St.push_scope transEnv [], None) ~f:translate_dec in
	let translatedExp = unEx (translate_exp level (transEnv', typeEnv', varEnv') breakLabel exp) in
	match translatedDecs with
	| Some decs -> Ex (T.ESeq (decs, translatedExp))
	| None -> Ex translatedExp

and translate_nil_exp () =
	Ex (T.Const 0)

and translate_fun_dec level (transEnv, typeEnv, varEnv) translatedDecs breakLabel {name; params; result; body = bodyExp} =
	let symEscapePairs = List.map params ~f:(fun param -> (param.name, !(param.escape))) in
	let funLevel = L.new_level name level symEscapePairs in
	let symTransEntryPairs = List.map funLevel.frame.formals ~f:(fun (sym, frameAccess) -> (sym, VarEntry (frameAccess, funLevel))) in
	let funTransEnv = St.push_scope transEnv symTransEntryPairs in
	let funVenv = Tc.add_params_to_env typeEnv varEnv params in
	let translatedBodyExp = translate_exp funLevel (funTransEnv, typeEnv, funVenv) breakLabel bodyExp in
	let transEnv' = St.add name (FunEntry funLevel) transEnv in
	let fragment = Proc (unNx translatedBodyExp, funLevel) in
	fragments := fragment::!fragments;
	(transEnv', translatedDecs)

and translate_var_dec level envs translatedDecs breakLabel {name; escape; typ; init = initExp} =
	let (transEnv, typeEnv, varEnv) = envs in
	let translatedInitExp = unEx (translate_exp level envs breakLabel initExp) in
	let (frameAccess, varLevel) = L.alloc_local level !escape in
	let transEnv' = St.add name (VarEntry (frameAccess, varLevel)) transEnv in
	let varExp = match frameAccess with
				 | InFrame offset -> mem_node_from_offset varLevel varLevel offset
				 | InReg reg -> T.Temp reg in
	let translatedDec = T.Move (varExp, translatedInitExp) in
	match translatedDecs with
	| Some decs -> (transEnv', Some (T.Seq (decs, translatedDec)))
	| None -> (transEnv', Some translatedDec)

and translate_arithmetic_op lExp rExp op =
	match op with
	| Plus -> Ex (T.Binop (T.PLUS, unEx lExp, unEx rExp))
	| Minus -> Ex (T.Binop (T.MINUS, unEx lExp, unEx rExp))
	| Mult -> Ex (T.Binop (T.MUL, unEx lExp, unEx rExp))
	| Div -> Ex (T.Binop (T.DIV, unEx lExp, unEx rExp))

and translate_comparison_op lExp rExp op =
	match op with
	| Eq -> Cx (fun tLabel fLabel -> T.CJump (T.EQ, unEx lExp, unEx rExp, tLabel, fLabel))
	| Neq -> Cx (fun tLabel fLabel -> T.CJump (T.NE, unEx lExp, unEx rExp, tLabel, fLabel))
	| Lt -> Cx (fun tLabel fLabel -> T.CJump (T.LT, unEx lExp, unEx rExp, tLabel, fLabel))
	| LtEq -> Cx (fun tLabel fLabel -> T.CJump (T.LE, unEx lExp, unEx rExp, tLabel, fLabel))
	| Gt -> Cx (fun tLabel fLabel -> T.CJump (T.GT, unEx lExp, unEx rExp, tLabel, fLabel))
	| GtEq -> Cx (fun tLabel fLabel -> T.CJump (T.GE, unEx lExp, unEx rExp, tLabel, fLabel))

and translate_boolean_op lExp rExp op =
	let lConditional = unCx lExp in
	let rConditional = unCx rExp in
	let tempLabel = Temp.new_label () in
	match op with
	| And -> Cx (fun tLabel fLabel -> T.Seq (lConditional tempLabel fLabel,
											 T.Seq (T.Label tempLabel, rConditional tLabel fLabel)))
	| Or -> Cx (fun tLabel fLabel -> T.Seq (lConditional tempLabel fLabel,
											T.Seq (T.Label tempLabel, rConditional tLabel fLabel)))

and translate_simpleVar refLevel transEnv sym =
	match St.find transEnv sym with
	| Some (VarEntry (F.InFrame offset, decLevel)) -> Ex (mem_node_from_offset decLevel refLevel offset)
	| Some (VarEntry (F.InReg reg, decLevel)) ->
		assert (L.equal refLevel decLevel);
		Ex (T.Temp reg)
	| Some _ -> assert false
	| None -> assert false

and translate_fieldVar level (transEnv, typeEnv, varEnv) var sym breakLabel =
	let calculate_field_offset sym = function
		| Tt.Record (symTyEntryPairs, _) -> T.Const (index_of_sym sym symTyEntryPairs)
		| Array _ | Int | String | Nil | Unit | Name _ -> assert false in
	let (tyEntry, _, _) = Tc.type_of_var typeEnv varEnv var in
	let baseAddress = unEx (translate_var level (transEnv, typeEnv, varEnv) breakLabel var) in
	let fieldOffset = calculate_field_offset sym tyEntry in
	let fieldAddress = T.Binop (T.PLUS, baseAddress, fieldOffset) in
	Ex (T.Mem fieldAddress)

and translate_subscriptVar level (transEnv, typeEnv, varEnv) var exp breakLabel =
	let baseAddress = unEx (translate_var level (transEnv, typeEnv, varEnv) breakLabel var) in
	let index = unEx (translate_exp level (transEnv, typeEnv, varEnv) breakLabel exp) in
	let arrayOffset = T.Binop (T.MUL, T.Const F.wordSize, index) in
	let indexAddress = T.Binop (T.PLUS, baseAddress, arrayOffset) in
	Ex (T.Mem indexAddress)

and mem_node_from_offset decLevel refLevel offset =
	let rec create_fp_address decLevel refLevel fpAddress =
		if L.equal refLevel decLevel then
			fpAddress
		else
			match refLevel.parent with
			| Some parentLevel ->
				let parentFpAddress = T.Mem (T.Binop (T.PLUS, fpAddress, T.Const F.staticLinkOffset)) in
				create_fp_address decLevel parentLevel parentFpAddress
			| None -> assert false in
	let fpAddress = create_fp_address decLevel refLevel (T.Temp F.fp) in 
	T.Mem (T.Binop (T.PLUS, fpAddress, (T.Const offset)))

and index_of_sym sym = function
	| (fieldSym, _)::tl ->
		if fieldSym = sym then
			0
		else
			1 + (index_of_sym sym tl)
	| [] -> assert false
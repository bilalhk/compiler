module T = Tree
module L = Level
module St = Symbol_table
module F = Frame

type entry =
	| VarEntry of L.access
	| FunEntry of L.access

type exp =
	| Ex of T.exp
	| Nx of T.stm
	| Cx of Temp.label * Temp.label -> T.stm

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
			Cx (fun tLabel fLabel -> T.Jump (T.Name fLabel, [fLabel]))
		else
			Cx (fun tLabel fLabel -> T.Jump (T.Name tLabel, [tLabel]))
	| Ex exp -> Cx (fun tLabel fLabel -> T.CJump (T.EQ, exp, T.Const 0, fLabel, tLabel))
	| Cx genStm -> genStm
	| Nx _ -> assert false

let translate exp =
	translate_with_env L.mainLevel St.empty exp

and translate_with_env level env = function
	| VarExp var -> translate_var level env var
	| IntExp num ->
	| StringExp str ->
	| CallExp (funSym, args) ->
	| OpExp (lExp, oper, rExp) ->
	| RecordExp (recSym, fields) ->
	| SeqExp exps ->
	| AssignExp (var, exp) ->
	| IfExp ifExp -> 
	| WhileExp whileExp -> 
	| ForExp forExp -> 
	| LetExp (decs, exp) ->
	| ArrayExp arrayExp -> 
	| BreakExp -> 
	| NilExp -> 

and translate_var level env = function
	| SimpleVar sym -> translate_simpleVar level env sym
	| FieldVar (var, sym) ->
	| SubscriptVar (var, exp) ->

and translate_simpleVar level env sym =
	match St.find env sym with
	| Some (VarEntry (F.InFrame offset, decLevel)) -> mem_node_from_offset decLevel level offset
	| Some (VarEntry (F.InReg reg, decLevel)) ->
		assert (L.equal refLevel decLevel);
		T.Temp reg
	| Some _ -> assert false
	| None -> assert false

and mem_node_from_offset decLevel refLevel offset =
	let staticLinkOffset = F.staticLinkOffset in
	let create_fp_address decLevel refLevel fpAddress =
		if L.equal refLevel decLevel then
			fpAddress
		else
			match refLevel.parent with
			| Some parentLevel ->
				let parentFpAddress = T.Mem (T.Binop (T.PLUS, fpAddress, T.Const staticLinkOffset)) in
				create_fp_address decLevel parentLevel parentFpAddress
			| None -> assert false
	let fpAddress = create_fp_address decLevel refLevel (T.Temp F.fp) in 
	T.Mem (T.Binop T.PLUS fpAddress (T.Const offset))
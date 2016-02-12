module T = Tree

open Core.Std

let rec do_stm = function
	| T.Jump (exp, labels) -> reorder_stm [exp] (function [exp'] -> T.Jump (exp', labels) | _ -> assert false)
	| T.CJump (op, lExp, rExp, tLabel, fLabel) ->
		reorder_stm [lExp; rExp] (function [lExp'; rExp'] -> T.CJump (op, lExp', rExp', tLabel, fLabel)	| _ -> assert false)
	| T.Exp exp -> reorder_stm [exp] (function [exp'] -> T.Exp exp' | _ -> assert false)
	| T.Seq (stm1, stm2) ->
		let stm1' = do_stm stm1 in
		let stm2' = do_stm stm2 in
		T.Seq (stm1', stm2')
	| T.Label label -> T.Label label
	| T.Move (T.Temp reg, exp) -> reorder_stm [exp] (function [exp'] -> T.Move (T.Temp reg, exp') | _ -> assert false)
	| T.Move (T.Mem addressExp, exp) ->
		reorder_stm [addressExp; exp] (function [addressExp'; exp'] -> T.Move (T.Mem addressExp', exp')	| _ -> assert false)
	| T.Move _ -> assert false

and do_exp = function
	| T.Binop (op, lExp, rExp) -> reorder_exp [lExp; rExp] (function
		| [lExp'; rExp'] -> T.Binop (op, lExp', rExp')
		| _ -> assert false)
	| T.Mem exp -> reorder_exp [exp] (function [exp'] -> T.Mem exp' | _ -> assert false)
	| T.Call (exp, exps) -> reorder_exp (exp::exps) (function (exp'::exps') -> T.Call (exp', exps') | _ -> assert false)
	| T.ESeq (stm, exp) ->
		let stm' = do_stm stm in
		reorder_exp [exp] (function [exp'] -> T.ESeq (stm', exp') | _ -> assert false)
	| T.Const num -> (T.Exp (T.Const 0), T.Const num)
	| T.Name label -> (T.Exp (T.Const 0), T.Name label)
	| T.Temp reg -> (T.Exp (T.Const 0), T.Temp reg)

and reorder exps =
	let reorderedExps = List.map exps (fun exp ->
		let (stm, exp) = do_exp exp in
		match stm with
		| T.Exp (T.Const _) -> exp
		| _ -> T.ESeq (stm, exp)) in
	let (stm, exps') = List.fold_left reorderedExps ~init:(T.Exp(T.Const 0), []) ~f:remove_eseq in
	(stm, exps')

and reorder_stm exps build =
	let (stm, exps') = reorder exps in
	T.Seq (stm, build exps')

and reorder_exp exps build =
	let (stm, exps') = reorder exps in
	(stm, build exps')

and remove_eseq (stmAcc, exps) exp =
	match exp with
	| T.ESeq (stm, exp) ->
		let (_, stm', exps') = List.fold_left exps ~init:(stm, T.Exp (T.Const 0), []) ~f:pre_evaluate_exp in
		(T.Seq (stmAcc, stm'), exps')
	| exp -> (stmAcc, exps @ [exp])

and pre_evaluate_exp (stm, stmAcc, exps) exp =
	if not (commute stm exp) then
		let temp = T.Temp (Temp.new_reg ()) in
		let preEvaluationStm = T.Move (temp, exp) in
		(stm, T.Seq (stmAcc, preEvaluationStm), exps @ [temp])
	else
		(stm, stmAcc, exps @ [exp])

and commute stm exp =
	match stm, exp with
	| T.Exp (T.Const _), _ -> true
	| _, T.Name _ -> true
	| _, T.Const _ -> true
	| _, _ -> false
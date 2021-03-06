module T = Tree

open Core.Std

let rec linearize stm =
	let rec linear stm linearizedStms =
		match stm with
		| T.Seq (stm1, stm2) -> linear stm1 (linear stm2 linearizedStms)
		| _ -> stm::linearizedStms in
	let eseqLessStm = do_stm stm in 
	let linearizedStms = linear eseqLessStm [] in
	let cleanedStms = List.filter linearizedStms ~f:(function T.Exp (T.Const 0) -> false | _ -> true) in
	cleanedStms

and basic_blocks stms =
	let firstStm::restStms = stms in
	let initialBlock = label_initial_block firstStm in	
	let (lastBlock, previousBlocks) = List.fold_left restStms ~init:(initialBlock, []) ~f:add_to_block in
	let doneLabel = Temp.new_label () in
	let lastBlockWithJump = lastBlock @ [(T.Jump (T.Name doneLabel, [doneLabel]))] in
	let blocks = previousBlocks @ [lastBlockWithJump] in
	let firstBlock::restBlocks = blocks in
	let borderedBlocks = List.fold_left restBlocks ~init:[firstBlock] ~f:border_block in
	(borderedBlocks, doneLabel)

and trace_schedule blocks doneLabel =
	let initialTrace::remainingBlocks = blocks in
	let reversedTraces = build_trace doneLabel initialTrace [] remainingBlocks in
	List.rev reversedTraces

(* Flattens traces and makes sure a CJump is always followed by its false label. Also removes a Jump if immediately followed by its label. *)
and fix_stms traces =
	let firstStm::remainingStms = List.fold_left traces ~init:[] ~f:(fun flattenedTraces trace -> flattenedTraces @ trace) in
	let fixedStms = List.fold_left remainingStms ~init:[firstStm] ~f:fix_stm in
	fixedStms

(* Precondition: previousStms is not empty. *)
and fix_stm previousStms currentStm =
	let previousStm = List.last_exn previousStms in
	match previousStm, currentStm with
	| T.CJump _, _-> fix_CJump previousStms previousStm currentStm
	| T.Jump (T.Name jumpLabel, _), T.Label label ->
		if Temp.equal_labels jumpLabel label then
			let lastLessPreviousStms = remove_last_elm previousStms in
			lastLessPreviousStms @ [currentStm]
		else
			previousStms @ [currentStm]
	| _ -> previousStms @ [currentStm]

and fix_CJump previousStms cJumpStm currentStm =
	let lastLessPreviousStms = remove_last_elm previousStms in
	match cJumpStm, currentStm with
	| T.CJump (op, lExp, rExp, tLabel, fLabel), T.Label label ->
		if (Temp.equal_labels fLabel label) then
			previousStms @ [currentStm]
		else if (Temp.equal_labels tLabel label) then
			let negatedCJumpStm = negateCJumpStm cJumpStm in
			(lastLessPreviousStms @ [negatedCJumpStm]) @ [currentStm]
		else
			let fLabel' = Temp.new_label () in
			let cJumpStm' = T.CJump (op, lExp, rExp, tLabel, fLabel') in
			let labelStm = T.Label fLabel' in
			let jumpStm = T.Jump (T.Name fLabel', [fLabel']) in
			(((lastLessPreviousStms @ [cJumpStm']) @ [labelStm]) @ [jumpStm]) @ [currentStm]
	| _, _ -> assert false

(* Precondition: currentTrace is not empty. *)
and build_trace doneLabel currentTrace completedTraces remainingBlocks =
	match remainingBlocks with
	| nextBlock::remainingBlocks ->
		(match List.last_exn currentTrace with
		| T.Jump (_, [doneLabel]) -> build_trace_done_jump doneLabel currentTrace completedTraces remainingBlocks
		| T.Jump (_, labels) -> build_trace_jump doneLabel currentTrace completedTraces remainingBlocks labels
		| T.CJump (_, _, _, tLabel, fLabel) -> build_trace_jump doneLabel currentTrace completedTraces remainingBlocks [fLabel])
	| [] -> currentTrace::completedTraces

(* Precondition: unnamed last argument is not empty *)
and build_trace_done_jump doneLabel currentTrace completedTraces = function
	| nextBlock::remainingBlocks ->
		build_trace doneLabel nextBlock (currentTrace::completedTraces) remainingBlocks
	| [] -> assert false

(* Precondition: remainingBlocks is not empty. *)
and build_trace_jump doneLabel currentTrace completedTraces remainingBlocks labels =
	let filteredLabels = List.filter labels ~f:(fun label -> not (Temp.equal_labels doneLabel label)) in
	let is_next_block ((T.Label label)::_) =
		List.mem ~equal:Temp.equal_labels filteredLabels label in
	let nextBlock = List.find remainingBlocks ~f:is_next_block in
	match nextBlock with
	| Some nextBlock ->
		let (T.Label nextLabel)::stms = nextBlock in
		let not_next_block ((T.Label label)::_) =
			not (Temp.equal_labels label nextLabel) in
		let filteredRemainingBlocks = List.filter remainingBlocks ~f:not_next_block in
		let newCurrentTrace = currentTrace @ nextBlock in
		build_trace doneLabel newCurrentTrace completedTraces filteredRemainingBlocks
	| None ->
		let newCompletedTraces = currentTrace::completedTraces in
		let newCurrentTrace::newRemainingBlocks = remainingBlocks in
		build_trace doneLabel newCurrentTrace newCompletedTraces newRemainingBlocks

and label_initial_block stm =
	match stm with
	| T.Label _ -> [stm]
	| _ ->
		let labelStm = T.Label (Temp.new_label ()) in
		[labelStm; stm]

and add_to_block (currentBlock, previousBlocks) stm =
	match stm with
	| T.CJump _ | T.Jump _ ->
		let finishedBlock = currentBlock @ [stm] in
		([], previousBlocks @ [finishedBlock])
	| T.Label _ ->
		let newBlock = [stm] in
		(newBlock, previousBlocks @ [currentBlock])
	| _ -> (currentBlock @ [stm], previousBlocks)

and border_block previousBlocks currentBlock =
	let currentFirstStm::_ = currentBlock in
	let previousBlock = List.last_exn previousBlocks in
	let previousLastStm = List.last_exn previousBlock in
	match previousLastStm, currentFirstStm with
	| T.Jump _, T.Label _ | T.CJump _, T.Label _ -> previousBlocks @ [currentBlock]
	| T.Jump _, _  | T.CJump _, _ ->
		let labelStm = T.Label (Temp.new_label ()) in
		let labeledCurrentBlock = labelStm::currentBlock in
		previousBlocks @ [labeledCurrentBlock]
	| _, T.Label label ->
		let jumpStm = T.Jump (T.Name label, [label]) in
		let previousBlockWithJump = previousBlock @ [jumpStm] in
		let _::reversedLastLessPreviousBlocks = List.rev previousBlocks in
		let lastLessPreviousBlocks = List.rev reversedLastLessPreviousBlocks in
		(lastLessPreviousBlocks @ [previousBlockWithJump]) @ [currentBlock]

and do_stm = function
	| T.Jump (exp, labels) -> reorder_stm [exp] (function [exp'] -> T.Jump (exp', labels) | _ -> assert false)
	| T.CJump (op, lExp, rExp, tLabel, fLabel) ->
		reorder_stm [lExp; rExp] (function [lExp'; rExp'] -> T.CJump (op, lExp', rExp', tLabel, fLabel)	| _ -> assert false)
	| T.Exp (T.Call (exp, exps)) ->
		reorder_stm (exp::exps) (function exp'::exps' -> T.Exp (T.Call (exp', exps')) | _ -> assert false)
	| T.Exp exp -> reorder_stm [exp] (function [exp'] -> T.Exp exp' | _ -> assert false)
	| T.Seq (stm1, stm2) ->
		let stm1' = do_stm stm1 in
		let stm2' = do_stm stm2 in
		T.Seq (stm1', stm2')
	| T.Label label -> T.Label label
	| T.Move (T.Mem addressExp, exp) ->
		reorder_stm [addressExp; exp] (function [addressExp'; exp'] -> T.Move (T.Mem addressExp', exp')	| _ -> assert false)
	| T.Move (T.Temp reg, T.Call (exp, exps)) ->
		reorder_stm (exp::exps) (function exp'::exps' -> T.Move (T.Temp reg, T.Call (exp', exps')) | _ -> assert false)
	| T.Move (T.Temp reg, exp) -> reorder_stm [exp] (function [exp'] -> T.Move (T.Temp reg, exp') | _ -> assert false)
	| T.Move _ -> assert false

and do_exp = function
	| T.Binop (op, lExp, rExp) ->
		reorder_exp [lExp; rExp] (function [lExp'; rExp'] -> T.Binop (op, lExp', rExp') | _ -> assert false)
	| T.Mem exp -> reorder_exp [exp] (function [exp'] -> T.Mem exp' | _ -> assert false)
	| T.Call (exp, exps) -> reorder_exp (exp::exps) (function (exp'::exps') -> T.Call (exp', exps') | _ -> assert false)
	| T.ESeq (stm, exp) ->
		let stm' = do_stm stm in
		reorder_exp [exp] (function [exp'] -> T.ESeq (stm', exp') | _ -> assert false)
	| T.Const num -> (T.Exp (T.Const 0), T.Const num)
	| T.Name label -> (T.Exp (T.Const 0), T.Name label)
	| T.Temp reg -> (T.Exp (T.Const 0), T.Temp reg)

and reorder exps =
	let callRVTempedExps = List.map exps ~f:assign_temp_for_call in
	let percolatedExps = List.map callRVTempedExps ~f:percolateExp in
	let (stm, exps') = List.fold_left percolatedExps ~init:(T.Exp(T.Const 0), []) ~f:remove_eseq in
	(stm, exps')

and reorder_stm exps build =
	let (stm, exps') = reorder exps in
	T.Seq (stm, build exps')

and reorder_exp exps build =
	let (stm, exps') = reorder exps in
	(stm, build exps')

and assign_temp_for_call exp =
	match exp with
	| T.Call _ ->
		let temp = T.Temp (Temp.new_reg ()) in
		T.ESeq (T.Move (temp, exp), temp)
	| _ -> exp

and percolateExp exp =
	let (stm, exp) = do_exp exp in
	match stm, exp with
	| T.Exp (T.Const _), _ -> exp
	| _, T.ESeq (stm', exp') -> T.ESeq (T.Seq (stm', stm), exp)
	| _, _ -> T.ESeq (stm, exp)

and remove_eseq (stmAcc, exps) exp =
	match exp with
	| T.ESeq (stm, exp) ->
		let (_, stm', exps') = List.fold_left exps ~init:(stm, T.Exp (T.Const 0), []) ~f:pre_evaluate_exp in
		let stmAcc' = T.Seq (stmAcc, T.Seq (stm', stm)) in
		(stmAcc', exps' @ [exp])
	| _ -> (stmAcc, exps @ [exp])

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

and remove_last_elm lst =
	let _::reversedListWithoutLastElm = List.rev lst in
	List.rev reversedListWithoutLastElm

and negateCJumpStm = function
	| T.CJump (T.EQ, lExp, rExp, tLabel, fLabel) -> T.CJump (T.NE, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.NE, lExp, rExp, tLabel, fLabel) -> T.CJump (T.EQ, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.LT, lExp, rExp, tLabel, fLabel) -> T.CJump (T.GE, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.GT, lExp, rExp, tLabel, fLabel) -> T.CJump (T.LE, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.LE, lExp, rExp, tLabel, fLabel) -> T.CJump (T.GT, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.GE, lExp, rExp, tLabel, fLabel) -> T.CJump (T.LT, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.ULT, lExp, rExp, tLabel, fLabel) -> T.CJump (T.UGE, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.ULE, lExp, rExp, tLabel, fLabel) -> T.CJump (T.UGT, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.UGT, lExp, rExp, tLabel, fLabel) -> T.CJump (T.ULE, lExp, rExp, fLabel, tLabel)
	| T.CJump (T.UGE, lExp, rExp, tLabel, fLabel) -> T.CJump (T.ULT, lExp, rExp, fLabel, tLabel)
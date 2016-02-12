module T = Tree

open Core.Std

type t = {formals: (Symbol.t * access) list; localCount: int ref}

and access =
	| InFrame of int
	| InReg of Temp.reg

let wordSize = 4

let fp = Temp.new_reg ()

let sp = Temp.new_reg ()

let staticLinkOffset = wordSize

let new_frame symEscapePairs =
	let update_pair_acc (sym, escape) (pairs, inFrameCount) =
		if escape then
			let newInFrameCount = inFrameCount + 1 in
			let offset = wordSize + (wordSize * newInFrameCount) in
			let pair = (sym, InFrame offset) in
			(pair::pairs, newInFrameCount)
		else
			let reg = Temp.new_reg () in
			let pair = (sym, InReg reg) in
			(pair::pairs, inFrameCount) in
	let (symEscapePairs, _) = List.fold_right symEscapePairs ~init:([], 0) ~f:update_pair_acc in
	{formals = symEscapePairs; localCount = ref 0}

let alloc_local frame escape =
	if escape then
		let localCount = !(frame.localCount) in
		let offset = -(wordSize + (wordSize * localCount)) in
		frame.localCount := localCount + 1;
		InFrame offset
	else
		let reg = Temp.new_reg () in
		InReg reg

let external_call functionName args =
	let label = T.Name (Temp.named_label functionName) in
	T.Call (label, args)
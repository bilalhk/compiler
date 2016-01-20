type t = {formals: (Symbol.t * access) list; localCount: int ref}

and access =
	| InFrame of int
	| InReg of Temp.reg

let wordSize = 4

let new_frame symEscapePairs =
	let update_pair_acc (pairs, inFrameCount) (sym, escape) =
		if escape then
			let newInFrameCount = inFrameCount + 1 in
			let offset = wordSize + (wordSize * newInFrameCount) in
			let pair = (sym, InFrame offset) in
			(pair::pairs, newInFrameCount)
		else
			let reg = Temp.new_reg () in
			let pair = (sym, InReg reg) in
			(pair::pairs, inFrameCount) in
	let symAccessPairs = List.fold_left symEscapePairs init:([], 0) ~f:update_pair_acc in
	{formals = symAccessPairs; locals = []}

let alloc_local frame escape =
	if escape then
		let offset = -(wordSize + (wordSize * frame.localCount)) in
		frame.localCount := !frame.localCount + 1;
		InFrame offset
	else
		let reg = Temp.new_reg () in
		InReg reg
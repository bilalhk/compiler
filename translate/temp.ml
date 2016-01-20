(* type definition for temporary register *)
type reg = int

let next_reg = ref 0

let new_reg () : reg =
	next_reg := !next_reg + 1;
	!next_reg - 1

let reg_to_string reg =
	"t" ^ (Int.to_string !reg)

(* type definition for temporary label *)
type label = Symbol.t

let next_label = ref 0

let new_label () : label =
	next_label := !next_label + 1;
	let label_string = "L" ^ (Int.of_string (!next_label - 1)) in
	Symbol.of_string label_string

let named_label name =
	Symbol.of_string name
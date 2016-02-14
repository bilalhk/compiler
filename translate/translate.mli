type fragment = 
	| Proc of Tree.stm * Level.t
	| String of Temp.label * string

type exp =
	| Ex of Tree.exp
	| Nx of Tree.stm
	| Cx of (Temp.label -> Temp.label -> Tree.stm)

val translate_prog : Ast.exp -> fragment list

val fragments : (fragment list) ref
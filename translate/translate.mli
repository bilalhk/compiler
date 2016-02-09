type exp =
	| Ex of Tree.exp
	| Nx of Tree.stm
	| Cx of (Temp.label -> Temp.label -> Tree.stm)

val translate_prog : Ast.exp -> exp
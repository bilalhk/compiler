type exp =
	| Const of int
	| Name of Temp.label
	| Temp of Temp.reg
	| Binop of binop * exp * exp
	| Mem of exp
	| Call of exp * exp list
	| ESeq of stm * exp

and stm =
	| Move of exp * exp
	| Exp of exp
	| Jump of exp * Temp.label list
	| CJump of relop * exp * exp * Temp.label * Temp.label
	| Seq of stm * stm
	| Label of Temp.label

and binop =
	| PLUS | MINUS | MUL | DIV
	| AND | OR | XOR | LSHIFT | RSHIFT | ARSHIFT

and relop =
	| EQ | NE | LT | GT | LE | GE
	| ULT | ULE | UGT | UGE
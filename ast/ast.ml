type pos = int

and var = 
	| SimpleVar of string
	| FieldVar of var * string
	| SubscriptVar of var * exp

and exp =
	| VarExp of var
	| NilExp
	| IntExp of int
	| StringExp of string
	| CallExp of string * exp list
	| OpExp of exp * oper * exp
	| RecordExp of string * field list
	| SeqExp of exp list
	| AssignExp of var * exp
	| IfExp of ifExp
	| WhileExp of whileExp
	| ForExp of forExp
	| BreakExp
	| LetExp of dec list * exp
	| ArrayExp of arrayExp

and dec = 
	| FunctionDec of fundec
	| VarDec of varDec
	| TypeDec of string * ty

and ty =
	| NameTy of string
	| RecordTy of tyField list
	| ArrayTy of string

and oper =
	| PlusOp | MinusOp | MultOp | DivOp | EqOp | NeqOp | LtOp | LtEqOp | GtOp | GtEqOp | And | Or

and tyField = {name: string; escape: bool ref; typ: string}

and field = {name: string; escape: bool ref; value: exp}

and fundec = {name: string; params: tyField list; result: string option; body: exp}

and ifExp = {test: exp; then': exp; else': exp option}

and whileExp = {test: exp; body: exp}

and forExp = {var: string; escape: bool ref; lo: exp; hi: exp; body: exp}

and arrayExp = {typ: string; size: exp; init: exp}

and varDec = {name: string; escape: bool ref; typ: string option; init: exp}
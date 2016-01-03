type symbol = Symbol.t

type pos = int

and var = 
	| SimpleVar of symbol
	| FieldVar of var * symbol
	| SubscriptVar of var * exp

and exp =
	| VarExp of var
	| NilExp
	| IntExp of int
	| StringExp of string
	| CallExp of symbol * exp list
	| OpExp of exp * oper * exp
	| RecordExp of symbol * recordField list
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
	| TypeDec of symbol * ty

and ty =
	| NameTy of symbol
	| RecordTy of recordTyField list
	| ArrayTy of symbol

and oper =
	| Plus | Minus | Mult | Div | Eq | Neq | Lt | LtEq | Gt | GtEq | And | Or

and recordTyField = {name: symbol; typ: symbol}

and formalParam = {name: symbol; escape: bool ref; typ: symbol}

and recordField = {name: symbol; escape: bool ref; value: exp}

and fundec = {name: symbol; params: formalParam list; result: symbol option; body: exp}

and ifExp = {testExp: exp; thenExp: exp; elseExp: exp option}

and whileExp = {test: exp; body: exp}

and forExp = {var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp}

and arrayExp = {typ: symbol; size: exp; init: exp}

and varDec = {name: symbol; escape: bool ref; typ: symbol option; init: exp}
%{
	open Ast
%}

%token EQUALS NOT_EQUALS LESS_THAN GREATER_THAN GREATER_THAN_EQUALS LESS_THAN_EQUALS
%token AND OR
%token PLUS MINUS
%token MULT DIV
%token <string> Id
%token <int> Int_literal
%token <string> String_literal
%token RIGHT_BRACE
%token LEFT_BRACE
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_PAREN RIGHT_PAREN
%token TYPE
%token COLON
%token SEMI_COLON
%token ARRAY
%token OF
%token COMMA
%token VAR
%token ASSIGNMENT_OP
%token NIL
%token FUNCTION
%token IF THEN ELSE
%token WHILE DO
%token FOR TO
%token BREAK
%token DOT
%token LET IN END
%token EOF

%start prog
%type <(Ast.exp)> prog

%%

prog:
	exp EOF	{$1}
;

dec:
	| TYPE Id EQUALS ty {Ast.TypeDec($2, $4)} /* Type Declaration */
	| VAR Id ASSIGNMENT_OP exp {Ast.VarDec({name = $2; escape = ref false; typ = None; init = $4})} /* Variable Declaration */
	| VAR Id COLON Id ASSIGNMENT_OP exp {Ast.VarDec({name = $2; escape = ref false; typ = Some $4; init = $6})} /* Variable Declaration With Type */
	| FUNCTION Id LEFT_PAREN tyFields RIGHT_PAREN EQUALS exp {Ast.FunctionDec({name = $2; params = $4; result = None; body = $7})} /* Function Declaration */
	| FUNCTION Id LEFT_PAREN tyFields RIGHT_PAREN COLON Id EQUALS exp {Ast.FunctionDec({name = $2; params = $4; result = Some $7; body = $9})} /* Function Declaration With Return Type */
;

exp:
	| LET decs IN exp END {Ast.LetExp($2, $4)} /* Let Expression With Declarations */
	| LET IN exp END {Ast.LetExp([], $3)} /* Let Expression With No Declarations */
	| LEFT_PAREN exprList RIGHT_PAREN {Ast.SeqExp($2)} /* Expression Sequence */
	| lvalue ASSIGNMENT_OP exp {Ast.AssignExp($1, $3)} /* Assignment Expression */
	| lvalue {Ast.VarExp($1)} /* Variable Expression */
	| NIL {Ast.NilExp} /* Nil Expression */
	| Int_literal {Ast.IntExp($1)} /* Integer Expression */
	| String_literal {Ast.StringExp($1)} /* String Expression */
	| Id LEFT_PAREN paramList RIGHT_PAREN {Ast.CallExp($1, $3)} /* Function Application With Params */
	| Id LEFT_PAREN RIGHT_PAREN {Ast.CallExp($1, [])} /* Function Application With No Params*/
	| exp oper exp {Ast.OpExp($1, $2, $3)} /* Operator Expression */
	| Id LEFT_BRACE fieldList RIGHT_BRACE {Ast.RecordExp($1, $3)} /* Record Creation Expression*/
	| Id LEFT_BRACE RIGHT_BRACE {Ast.RecordExp($1, [])} /* Empty Record Creation */
	| IF exp THEN exp ELSE exp {Ast.IfExp({test = $2; then' = $4; else' = Some $6})} /* If-Then-Else Expression */
	| IF exp THEN exp {Ast.IfExp({test = $2; then' = $4; else' = None})} /* If-Then Expression */
	| WHILE exp DO exp {Ast.WhileExp({test = $2; body = $4})} /* While Expression */
	| FOR Id ASSIGNMENT_OP exp TO exp DO exp {Ast.ForExp({var = $2; escape = ref false; lo = $4; hi = $6; body = $8})} /* For Expression */
	| BREAK {Ast.BreakExp} /* Break Expression */
	| Id LEFT_BRACKET exp RIGHT_BRACKET OF exp {Ast.ArrayExp({typ = $1; size = $3; init = $6})} /* Array Creation Expression */
;

ty:
	| Id {Ast.NameTy($1)} /* Simple Type Variable */
	| LEFT_BRACE tyFields RIGHT_BRACE {Ast.RecordTy($2)} /* Record Type */
	| ARRAY OF Id {Ast.ArrayTy($3)} /* Array Type */
;

tyField:
	| Id COLON Id {{name = $1; escape = ref false; typ = $3}}
;

tyFields:
	| {[]}
	| tyField {$1::[]}
	| tyField COMMA tyFields {$1::$3}
;

exprList:
	| exp {$1::[]}
	| exp SEMI_COLON exprList {$1::$3}
;

decs:
	| dec {$1::[]}
	| dec decs {$1::$2}
;

paramList:
	| exp {$1::[]}
	| exp COMMA paramList {$1::$3}
;

lvalue:
	| Id {Ast.SimpleVar($1)} /* Simple Variable */
	| lvalue DOT Id {Ast.FieldVar($1, $3)} /* Record Field Variable */
	| lvalue LEFT_BRACKET exp RIGHT_BRACKET {Ast.SubscriptVar($1, $3)} /* Array Subscript Variable */
;

field:
	| Id EQUALS exp {{name = $1; escape = ref false; value = $3}}
;

fieldList:
	| field {$1::[]}
	| field COMMA fieldList {$1::$3}
;

oper:
	| PLUS {Ast.PlusOp}
	| MINUS {Ast.MinusOp}
	| MULT {Ast.MultOp}
	| DIV {Ast.DivOp}
	| EQUALS {Ast.EqOp}
	| LESS_THAN {Ast.LtOp}
	| GREATER_THAN {Ast.GtOp}
	| NOT_EQUALS {Ast.NeqOp}
	| GREATER_THAN_EQUALS {Ast.GtEqOp}
	| LESS_THAN_EQUALS {Ast.LtEqOp}
	| AND {Ast.And}
	| OR {Ast.Or}
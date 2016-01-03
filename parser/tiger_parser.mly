%{
	open Ast
%}

%token EQUALS NOT_EQUALS LESS_THAN GREATER_THAN GREATER_THAN_EQUALS LESS_THAN_EQUALS
%token AND OR
%token PLUS MINUS
%token MULT DIV
%token <Symbol.t> Id
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

%right IF THEN ELSE

%right DO

%right OF

%nonassoc Id
%right LEFT_BRACKET

%right ASSIGNMENT_OP
%left AND OR
%right EQUALS NOT_EQUALS LESS_THAN GREATER_THAN GREATER_THAN_EQUALS LESS_THAN_EQUALS
%left PLUS MINUS
%left MULT DIV

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
	| FUNCTION Id LEFT_PAREN formalParamList RIGHT_PAREN EQUALS exp {Ast.FunctionDec({name = $2; params = $4; result = None; body = $7})} /* Function Declaration */
	| FUNCTION Id LEFT_PAREN formalParamList RIGHT_PAREN COLON Id EQUALS exp {Ast.FunctionDec({name = $2; params = $4; result = Some $7; body = $9})} /* Function Declaration With Return Type */
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
	
	/* Operator Expressions */
	| exp PLUS exp {Ast.OpExp($1, Ast.Plus, $3)}
	| exp MINUS exp {Ast.OpExp($1, Ast.Minus, $3)}
	| exp MULT exp {Ast.OpExp($1, Ast.Mult, $3)}
	| exp DIV exp {Ast.OpExp($1, Ast.Div, $3)}
	| exp AND exp {Ast.OpExp($1, Ast.And, $3)}
	| exp OR exp {Ast.OpExp($1, Ast.Or, $3)}
	| exp EQUALS exp {Ast.OpExp($1, Ast.Eq, $3)}
	| exp NOT_EQUALS exp {Ast.OpExp($1, Ast.Neq, $3)}
	| exp LESS_THAN exp {Ast.OpExp($1, Ast.Lt, $3)}
	| exp GREATER_THAN exp {Ast.OpExp($1, Ast.Gt, $3)}
	| exp GREATER_THAN_EQUALS exp {Ast.OpExp($1, Ast.GtEq, $3)}
	| exp LESS_THAN_EQUALS exp {Ast.OpExp($1, Ast.LtEq, $3)}

	| Id LEFT_BRACE recordFieldList RIGHT_BRACE {Ast.RecordExp($1, $3)} /* Record Creation Expression*/
	| Id LEFT_BRACE RIGHT_BRACE {Ast.RecordExp($1, [])} /* Empty Record Creation */
	| IF exp THEN exp ELSE exp {Ast.IfExp({testExp = $2; thenExp = $4; elseExp = Some $6})} /* If-Then-Else Expression */
	| IF exp THEN exp {Ast.IfExp({testExp = $2; thenExp = $4; elseExp = None})} /* If-Then Expression */
	| WHILE exp DO exp {Ast.WhileExp({test = $2; body = $4})} /* While Expression */
	| FOR Id ASSIGNMENT_OP exp TO exp DO exp {Ast.ForExp({var = $2; escape = ref false; lo = $4; hi = $6; body = $8})} /* For Expression */
	| BREAK {Ast.BreakExp} /* Break Expression */
	| Id LEFT_BRACKET exp RIGHT_BRACKET OF exp {Ast.ArrayExp({typ = $1; size = $3; init = $6})} /* Array Creation Expression */
;

formalParamList:
	| {[]}
	| formalParam {$1::[]}
	| formalParam COMMA formalParamList {$1::$3}

formalParam:
	| Id COLON Id {{name = $1; escape = ref false; typ = $3}}

ty:
	| Id {Ast.NameTy($1)} /* Simple Type Variable */
	| LEFT_BRACE recordTyFieldList RIGHT_BRACE {Ast.RecordTy($2)} /* Record Type */
	| ARRAY OF Id {Ast.ArrayTy($3)} /* Array Type */
;

recordTyField:
	| Id COLON Id {{name = $1; typ = $3}}
;

recordTyFieldList:
	| {[]}
	| recordTyField {$1::[]}
	| recordTyField COMMA recordTyFieldList {$1::$3}
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

recordField:
	| Id EQUALS exp {{name = $1; escape = ref false; value = $3}}
;

recordFieldList:
	| recordField {$1::[]}
	| recordField COMMA recordFieldList {$1::$3}
;
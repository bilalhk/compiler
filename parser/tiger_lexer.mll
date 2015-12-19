{
	open Tiger_parser;;

	exception SyntaxError of string
}

let digits = ['0'-'9']+
let id = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
	| '{' {LEFT_BRACE}
	| '}' {RIGHT_BRACE}
	| '[' {LEFT_BRACKET}
	| ']' {RIGHT_BRACKET}
	| '(' {LEFT_PAREN}
	| ')' {RIGHT_PAREN}
	| '.' {DOT}
	| '&' {AND}
	| '|' {OR}
	| '+' {PLUS}
	| '-' {MINUS}
	| '*' {MULT}
	| '/' {DIV}
	| '=' {EQUALS}
	| '<' {LESS_THAN}
	| '>' {GREATER_THAN}
	| ':' {COLON}
	| ';' {SEMI_COLON}
	| ',' {COMMA}
	| '"' {read_string (Buffer.create 16) lexbuf}
	| "type" {TYPE}
	| "array" {ARRAY}
	| "of" {OF}
	| "var" {VAR}
	| ":=" {ASSIGNMENT_OP}
	| "nil" {NIL}
	| "<>" {NOT_EQUALS}
	| ">=" {GREATER_THAN_EQUALS}
	| "<=" {LESS_THAN_EQUALS}
	| "function" {FUNCTION}
	| "if" {IF}
	| "then" {THEN}
	| "else" {ELSE}
	| "while" {WHILE}
	| "do" {DO}
	| "for" {FOR}
	| "to" {TO}
	| "break" {BREAK}
	| "let" {LET}
	| "in" {IN}
	| "end" {END}
	| [' ' '\t' '\n'] {token lexbuf}
	| id as str		{Id str}
	| digits as str 	{Int_literal (int_of_string str)}
	| eof 	{EOF}


and read_string buf = parse
	| '"'	{String_literal (Buffer.contents buf)}
	| '\\' 'n'	{Buffer.add_char buf '\n'; read_string buf lexbuf}
	| '\\' 't'	{Buffer.add_char buf '\t'; read_string buf lexbuf}
	| '\\' '"'	{Buffer.add_char buf '\"'; read_string buf lexbuf}
	| '\\' '\\'	{Buffer.add_char buf '\\'; read_string buf lexbuf}
	| [^ '\\' '"']+	as str 	{Buffer.add_string buf str; read_string buf lexbuf}
	| _  as str 	{raise (SyntaxError ("Unexpected character: " ^ (String.make 1 str)))}
	| eof 	{raise (SyntaxError "String not terminated before eof")}
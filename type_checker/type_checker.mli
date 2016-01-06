open Tiger_types

val initialTenv : type_entry Symbol_table.t

val initialVenv : var_entry Symbol_table.t

val type_of_exp : type_entry Symbol_table.t -> var_entry Symbol_table.t -> Ast.exp -> type_entry
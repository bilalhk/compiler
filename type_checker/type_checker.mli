open Tiger_types

val initialTenv : type_entry Symbol_table.t

val initialVenv : var_entry Symbol_table.t

val type_of_exp : type_entry Symbol_table.t -> var_entry Symbol_table.t -> Ast.exp -> type_entry * type_entry Symbol_table.t * var_entry Symbol_table.t

val type_of_prog : Ast.exp -> type_entry * type_entry Symbol_table.t * var_entry Symbol_table.t

val add_params_to_env: type_entry Symbol_table.t -> var_entry Symbol_table.t -> Ast.formalParam list -> var_entry Symbol_table.t

val type_of_var : type_entry Symbol_table.t -> var_entry Symbol_table.t -> Ast.var -> type_entry * type_entry Symbol_table.t * var_entry Symbol_table.t
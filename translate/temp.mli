type reg = int

val new_reg : unit -> reg

val reg_to_string : reg -> string

type label = Symbol.t

val new_label : unit -> label

val named_label : string -> label
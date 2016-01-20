type reg

val new_reg : unit -> reg

val reg_to_string : reg -> string

type label

val new_label : unit -> label

val named_label : string -> label
(** Generates a random for loop statement that mutates some local or global
    data in the function. A generated for can be both: generic or numeric type. *)
val generate : Context.t -> Ast.env -> Ast.stmt

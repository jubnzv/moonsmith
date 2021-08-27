(** Generates a random conditional statement that mutates some local or global
    data in the function. *)
val generate : Context.t -> Ast.env -> Ast.stmt

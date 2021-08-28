(** Generates a random mutation statement that never changes control flow
    (i.e. no loops, no conditions). *)
val generate : Context.t -> Ast.env -> Ast.stmt

(** Generates sequence of [num] random linear statements. *)
val generate_stmts : Context.t -> Ast.env -> int -> Ast.stmt list

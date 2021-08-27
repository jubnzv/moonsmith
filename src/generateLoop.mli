(** Generates a random loop (until or while) statement that mutates some local
    or global data in the function. The resulted statements also may include
    definition of some control variables, defined out of the loop. The
    generated loop always terminates. *)
val generate : Ast.env -> Ast.stmt list

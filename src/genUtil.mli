(** This module contains various utilities used in generating a random AST. *)

(** Randomly generates an essential Lua type.
    NOTE: userdata and thread types are unsupported. *)
val gen_ty : unit -> Ast.ty

(** Randomly generates on of the following Lua types: Number (integer or
    float), Boolean or String. *)
val gen_simple_ty : unit -> Ast.ty

(** Generates a simple random expression: boolean, number, nil, string.  *)
val gen_simple_expr : unit -> Ast.expr

(** Generates an expression with the given type [ty]. *)
val gen_simple_typed_expr : Ast.ty -> Ast.expr

(** Generates a random init for a table expression.
    It can be both: an array or a hash map. *)
val gen_random_table_init : unit -> Ast.expr

(** Creates a new identifier in the [env].

    The created identifier won't implicitly added to the environment, because
    in some cases we should delay this. For example, we want to avoid the
    following situation:
      local a1, a2 = "foo", a1
    a1 is used before it was defined, so this causes a problem.

    So, user *must* call [Ast.env_flush_pending_bindings] after adding statements
    when [add_now] is false. *)
val gen_ident : ?add_now:bool -> ?name:(string option) -> Ast.env -> Ast.expr

(** Generates a [BlockStmt] without any nested statements. *)
val gen_dummy_block : unit -> Ast.stmt

(** Generates an appropriate rhs for assign the given expression. There
    can be global datums [ctx.ctx_datum_stmts] and local definitions from
    the given environment in the rhs.
    The expression must be [IdentExpr] with known type. *)
val gen_rhs_to_assign_ident : Context.t -> Ast.env -> Ast.expr -> Ast.expr

(** Generates an initializer statement for the identifier with known type.
    The generated initializer is just a random expression without any use of
    the contextual information.
    NOTE: userdata and thread types are unsupported. *)
val gen_init_stmt_for_ident : ?assign_local:bool -> Ast.expr -> Ast.stmt

(** Takes a random binding from the [env] or one of its parents.
    Returns None if environment and parents are empty. *)
val take_random_binding : Ast.env -> Ast.expr ref option

(** Takes a list of expression and combines them to a single expression that
    will have requested result type. *)
val combine_to_typed_expr : Context.t -> Ast.ty -> Ast.expr list -> Ast.expr option

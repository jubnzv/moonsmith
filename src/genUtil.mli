(** This module contains various utilities used in generating a random AST. *)

(** Randomly generates an essential Lua type.
    NOTE: userdata and thread types are unsupported. *)
val gen_ty : unit -> Ast.ty

(** Randomly generates on of the following types:
    + Boolean
    + Int
    + Float
    + String *)
val gen_simple_ty : unit -> Ast.ty

(** Generates a random init for an array table expression.
    See: https://www.lua.org/pil/11.1.html *)
val gen_array_table_init : unit -> Ast.expr

(** Generates a random init for an hash table expression. *)
val gen_hash_table_init : ?ty:Ast.ty -> unit -> Ast.expr

(** Generates a simple random expression of the given type.
    If type is not specified, chooses randomly between:
      + Boolean
      + Int
      + Float
      + String *)
val gen_simple_expr : ?ty:Ast.ty -> ?always_positive:bool -> unit -> Ast.expr

(** Generates comparison operator that could be applied to two expressions with
    the given type. *)
val gen_compare_binop : Ast.ty -> Ast.operator

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
    The expression must be [IdentExpr] with a known type. *)
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

(** Extends the BlockStmt [block] adding the given [stmts] to the end of the
    block. *)
val extend_block_stmt : Ast.stmt -> Ast.stmt list -> Ast.stmt

(** Generates an empty block statement. *)
val gen_empty_block : ?is_loop:bool -> Ast.env -> Ast.stmt

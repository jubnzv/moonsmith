open Core_kernel

(** Random code generation context. *)
type t = {
  (** Statements that defines a global data on the top-level. *)
  mutable ctx_datum_stmts: Ast.stmt list;
  (** Functions and methods defined on the top-level. *)
  ctx_funcdef_stmts: Ast.stmt list;
  (** Function calls defined on the top-level. *)
  ctx_call_stmts: Ast.stmt list;
  (** Statements that combine and print result data. *)
  ctx_result_stmts: Ast.stmt list;
  (** Global environment for the top-level. *)
  ctx_global_env: Ast.env;
  (** Definitions of standard functions. *)
  ctx_standard_functions: Ast.stmt list;
  (** User-defined configuration. *)
  ctx_config : Config.t;
  (** Map that associates ids of OOP tables with definitions of their methods. *)
  mutable ctx_oop_table_methods_map: (int, Ast.stmt ref list, Int.comparator_witness) Base.Map.t;
  (** Map that associates ids of FuncDefStmts with pointer to their AST nodes. *)
  mutable ctx_func_def_map: (int, Ast.stmt ref, Int.comparator_witness) Base.Map.t;
  (** Seed used to initialize PRG. *)
  ctx_seed: int; }

val mk_context : Config.t -> t

(** Returns a list of available tables defined in [ctx.ctx_datum_stmts]. *)
val get_datum_tables : t -> Ast.stmt list

(** Peeks random lhs of [ctx.ctx_datum_stmts] which has requested type.
    Returns None if there is no datums with such type. *)
val peek_typed_datum : t -> Ast.ty -> Ast.expr option

(** Same as [get_datum_tables], but also returns indexes in the
    [ctx.ctx_datum_stmts]. *)
val get_datum_tables_i : t -> (int * Ast.expr) list

(** Generates the unique index. *)
val get_free_idx : unit -> int

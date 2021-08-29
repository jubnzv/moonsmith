(** Essential types present in Lua. *)
type ty =
  | TyNil
  | TyBoolean
  | TyInt
  | TyFloat
  | TyIntString
  | TyString
  | TyUserdata
  | TyFunction
  | TyThread
  | TyTable
  | TyAny
[@@deriving eq, show]

(** See: https://www.lua.org/manual/5.3/manual.html#3.4.1 *)
type operator =
  | OpAdd     (* + *)
  | OpSub     (* - *)
  | OpMul     (* * *)
  | OpDiv     (* / *)
  | OpFloor   (* // *)
  | OpPow     (* ^ *)
  | OpMod     (* % *)
  | OpConcat  (* .. *)
  | OpLt      (* < *)
  | OpLte     (* <= *)
  | OpGt      (* > *)
  | OpGte     (* >= *)
  | OpEq      (* == *)
  | OpNeq     (* ~= *)
  | OpBAnd    (* & *)
  | OpBOr     (* ~ *)
  | OpBRs     (* >> *)
  | OpBLs     (* << *)
  | OpBNot    (* ~ *)
  | OpAnd     (* and *)
  | OpOr      (* or *)
  | OpNot     (* not *)
  | OpLen     (* # *)

type env = { mutable env_bindings: expr ref list;
             mutable env_pending_bindings: expr ref list;
             mutable env_parent: env ref option;
             mutable env_children: env ref list; }

and expr =
  | TrueExpr
  | FalseExpr
  | NilExpr
  | IntExpr of int
  | FloatExpr of float
  | StringExpr of string
  | IdentExpr of { id_id: int;
                   id_name: string;
                   id_ty: ty }
  | AttrGetExpr of { ag_obj: expr;
                     ag_key: expr }
  | TableExpr of table_ty
  | LambdaExpr of { lambda_args: expr list;
                    lambda_body: stmt; }
  | FuncCallExpr of { fc_id: int;
                      fc_ty: func_call;
                      fc_args: expr list }
  | UnExpr of { un_op: operator;
                un_expr: expr }
  | BinExpr of { bin_lhs: expr;
                 bin_op: operator;
                 bin_rhs: expr }
and table_ty =
  | TArray of { table_elements: expr list }
  | THashMap of { table_fields: table_field list }
and table_field =
  { tf_key: expr;
    tf_value: expr }
and func_call =
  | FCMethod of { fcm_receiver: string;
                  fcm_method: string; }
  | FCFunc of { fcf_func: expr }

and stmt =
  | AssignStmt of { assign_local: bool;
                    assign_lhs: expr list;
                    assign_rhs: expr list }
  | FuncCallStmt of { fc_expr: expr }
  | DoBlockStmt of { do_block: stmt }
  | LoopStmt of { loop_cond: expr;
                  loop_block: stmt;
                  loop_ty: loop_type }
  | IfStmt of { if_cond: expr;
                if_body: stmt;
                if_else: stmt option }
  | NumForStmt of { nfor_name: string;
                    nfor_init: expr;
                    nfor_limit: expr;
                    nfor_step: expr;
                    nfor_body: stmt }
  | ForStmt of { for_names: expr list;
                 for_exprs: expr list;
                 for_body: stmt }
  | FuncDefStmt of { fd_id: int;
                     (** Receiver (object) which this function belongs to.
                         If not None, the function is a method. *)
                     fd_local: bool;
                     fd_receiver: string option;
                     fd_name: string;
                     fd_args: expr list;
                     fd_has_varags: bool;
                     fd_body: stmt;
                     fd_ty: ty list }
  | ReturnStmt of { return_exprs: expr list }
  | BreakStmt
  | BlockStmt of { block_stmts: stmt list;
                   block_is_loop: bool;
                   block_env: env }
and loop_type =
  | While
  | Repeat

(** Generates unique identifier used in hash tables. *)
val mki : unit -> int

val ty_to_s : ty -> string

(** Returns type of the given [expr] when it is known. *)
val get_essential_ty : expr -> ty option

(** Returns true iff given two types can be used in comparison operations. *)
val types_are_comparable : ty -> ty -> bool

(** Helper function to gen an environment from the BlockStmt.
    Throws an exception if the given [stmt] is not BlockStmt. *)
val get_block_env_exn : stmt -> env

val op_to_s : operator -> string

val env_mk : unit -> env
val env_empty : env -> bool
val env_has_parent : env -> bool
val env_get_parent_exn : env -> env
val env_peek_random_exn : env -> expr ref
val env_shuffle_local_bindings : env -> expr list

(** Find binding with apropriate type in the given environment and its parents
    up to [depth]. If [depth] not set, it searches in all available parents. *)
val env_find_binding_with_ty : ?depth:int -> env -> ty -> expr option

val env_add_binding : env -> expr -> unit
val env_add_pending_binding : env -> expr -> unit
val env_flush_pending_bindings : env -> unit
val env_add_child : env -> env -> unit

val stmt_to_s : ?cr:bool -> ?depth:int -> Config.t -> stmt -> string

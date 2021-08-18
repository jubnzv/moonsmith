(** Essential types present in Lua. *)
type ty =
  | TyNil
  | TyBoolean
  | TyNumber
  | TyString
  | TyUserdata
  | TyFunction
  | TyThread
  | TyTable

type operator =
  | OpAdd     (* + *)
  | OpSub     (* - *)
  | OpMul     (* * *)
  | OpDiv     (* / *)
  | OpPow     (* ^ *)
  | OpMod     (* % *)
  | OpConcat  (* .. *)
  | OpLt      (* < *)
  | OpLte     (* <= *)
  | OpGt      (* > *)
  | OpGte     (* >= *)
  | OpEq      (* == *)
  | OpNeq     (* ~= *)
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
  | NumberExpr of float
  | StringExpr of string
  | IdentExpr of { id_name: string;
                   id_ty: ty }
  | AttrGetExpr of { ag_obj: expr;
                     ag_key: expr }
  | TableExpr of table_ty
  | LambdaExpr of { lambda_args: expr list;
                    lambda_body: stmt; }
  | FuncCallExpr of func_call
  | UnExpr of { un_op: operator;
                un_expr: expr }
  | BinExpr of { bin_lhs: expr;
                 bin_op: operator;
                 bin_rhs: expr }
and table_ty =
  | TArray of { table_elements: expr list }
  | THashMap of { table_fields: table_field list }
and table_field =
  { tf_key: expr; tf_value: expr }
and func_call =
  | FCMethod of { fcm_receiver: expr;
                  fcm_method: string;
                  fcm_args: expr list }
  | FCFunc of { fcf_func: expr;
                fcf_args: expr list }

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
  | FuncDefStmt of { fd_name: string;
                     fd_args: expr list;
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
val env_take_rand_exn : env -> expr ref
val env_add_binding : env -> expr -> unit
val env_add_pending_binding : env -> expr -> unit
val env_flush_pending_bindings : env -> unit
val env_add_child : env -> env -> unit

val stmt_to_s : ?cr:bool -> ?depth:int -> Config.t -> stmt -> string

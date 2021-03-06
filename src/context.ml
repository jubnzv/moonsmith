open Core_kernel

(** Random code generation context. *)
type t = {
  mutable ctx_datum_stmts: Ast.stmt list;
  (** Statements that defines a global data on the top-level. *)

  ctx_funcdef_stmts: Ast.stmt list;
  (** Functions and methods defined on the top-level. *)

  ctx_call_stmts: Ast.stmt list;
  (** Function calls defined on the top-level. *)

  ctx_result_stmts: Ast.stmt list;
  (** Statements that combine and print result data. *)

  mutable ctx_global_env: Ast.env;
  (** Global environment for the top-level. *)

  ctx_config : Config.t;
  (** User-defined configuration. *)

  mutable ctx_table_fields_map: (int, int list, Int.comparator_witness) Base.Map.t;
  (** Map that associates ids of OOP tables with ids of definitions of their
      fields. *)

  mutable ctx_func_def_map: (int, Ast.stmt ref, Int.comparator_witness) Base.Map.t;
  (** Map that associates ids of FuncDefStmts with pointer to their AST nodes. *)

  ctx_seed: int;
  (** Seed used to initialize PRG. *)
}

let mk_context (c : Config.t) =
  let ctx_global_env = Ast.env_mk ()
  and ctx_seed =
    match c.c_seed with Some(v) -> v | _ -> Random.bits ()
  and ctx_func_def_map = Map.empty (module Int)
  and ctx_table_fields_map = Map.empty (module Int) in
  let ctx = { ctx_datum_stmts = [];
              ctx_funcdef_stmts = [];
              ctx_call_stmts = [];
              ctx_result_stmts = [];
              ctx_config = c;
              ctx_func_def_map;
              ctx_table_fields_map;
              ctx_global_env;
              ctx_seed; }
  in
  ctx
(* let ctx = gen_standard_functions ctx in *)

let add_to_global_env ctx expr =
  Ast.env_add_binding ctx.ctx_global_env expr

let get_datum_tables ctx =
  let ff = function
    | Ast.AssignStmt assign -> begin
        if not @@ phys_equal 1 @@ List.length assign.assign_lhs then false
        else match List.nth_exn assign.assign_lhs 0 with
          | Ast.IdentExpr id -> begin
              match id.id_ty with
              | Ast.TyTable -> true
              | _ -> false
            end
          | _ -> false
      end
    | _ -> false
  in List.filter ctx.ctx_datum_stmts ~f:ff

let peek_random_datum_exn ctx =
  let open Ast in
  match Util.choose_one_exn ctx.ctx_datum_stmts with
  | AssignStmt assign -> List.nth_exn assign.assign_lhs 0
  | _ -> failwith "Expected AssignStmt"

let peek_typed_datum ctx ty =
  let open Ast in
  let filter stmt =
    match stmt with
    | AssignStmt assign -> begin
        match List.nth_exn assign.assign_lhs 0 with
        | IdentExpr id -> begin
            if equal_ty id.id_ty ty then true
            else false
          end
        | _ -> failwith "Impossible: Found non-IdentExpr in LHS of the AssignStmt"
      end
    | _ -> failwith "Expected AssignStmt"
  in
  let datums_with_same_ty =
    List.filter ctx.ctx_datum_stmts ~f:filter
  in
  if List.is_empty datums_with_same_ty then None
  else begin
    match Util.choose_one_exn datums_with_same_ty with
    | AssignStmt assign -> List.nth assign.assign_lhs 0
    | _ -> failwith "Expected AssignStmt"
  end

let get_datum_tables_i ctx =
  let ff i acc stmt =
    match stmt with
    | Ast.AssignStmt assign -> begin
        if not @@ phys_equal 1 @@ List.length assign.assign_lhs then acc
        else match List.nth_exn assign.assign_lhs 0 with
          | Ast.IdentExpr id -> begin
              match id.id_ty with
              | Ast.TyTable -> acc @ [(i, Ast.IdentExpr(id))]
              | _ -> acc
            end
          | _ -> acc
      end
    | _ -> acc
  in
  List.foldi
    ctx.ctx_datum_stmts
    ~init:[]
    ~f:ff

let get_free_idx  =
  let n = ref (-1) in
  fun () -> incr n; !n

open Core_kernel

(** Termination condition. *)
type cond_term =
  | CondRaising  (* need to raise the value to terminate *)
  | CondLowering (* need to lower the value to terminate *)
  | CondFalse    (* need to set to true to terminate *)
  | CondTrue     (* need to set to false to terminate *)

(** Generates condition variable used to terminate the loop. We intentionally
    don't add it to any environment, because we don't want to perform any
    random mutation with this variable. *)
let gen_cond_var () =
  let open Ast in
  let idx = Context.get_free_idx () in
  let id_name = Printf.sprintf "cond%d" idx in
  let (rhs, cond_term, id_ty) = match Random.int_incl 0 3 with
    | 0 -> (IntExpr(Random.int_incl 0 10), CondRaising, TyInt)
    | 1 -> (IntExpr(Random.int_incl 100 125), CondLowering, TyInt)
    | 2 -> (FalseExpr, CondTrue, TyBoolean)
    | _ -> (TrueExpr, CondFalse, TyBoolean)
  in
  let ident = IdentExpr{ id_id = idx;
                         id_name;
                         id_ty }
  in
  let assign = AssignStmt{ assign_local = true;
                           assign_lhs = [ident];
                           assign_rhs = [rhs] }
  in
  (ident, assign, cond_term)

(** Generates condition of the loop using the [cond] variable. *)
let gen_loop_cond cond_var_ident cond_term =
  let open Ast in
  match get_essential_ty cond_var_ident with
  | Some TyInt -> begin
      let (bin_rhs, bin_op) = match cond_term with
        | CondRaising  ->
          let v = IntExpr(Random.int_incl 20 50) in
          if Random.bool () then (v, OpLt) else (v, OpLte)
        | CondLowering ->
          let v = IntExpr(Random.int_incl 50 75) in
          if Random.bool () then (v, OpGt) else (v, OpGte)
        | _ -> assert false
      in
      BinExpr{ bin_lhs = cond_var_ident;
               bin_op;
               bin_rhs }
    end
  | Some TyBoolean -> begin
      let (bin_rhs, bin_op) = match cond_term with
        | CondTrue ->
          if Random.bool () then (TrueExpr, OpEq)
          else (FalseExpr, OpNeq)
        | CondFalse ->
          if Random.bool () then (FalseExpr, OpEq)
          else (TrueExpr, OpNeq)
        | _ -> assert false
      in
      BinExpr{ bin_lhs = cond_var_ident;
               bin_op;
               bin_rhs }
    end
  | _ -> assert false

let gen_term_stmt cond_var_ident cond_term =
  let open Ast in
  let rhs = match cond_term with
    | CondRaising  -> BinExpr{ bin_lhs = cond_var_ident;
                               bin_op = OpAdd;
                               bin_rhs = IntExpr(Random.int_incl 1 3)}
    | CondLowering -> BinExpr{ bin_lhs = cond_var_ident;
                               bin_op = OpSub;
                               bin_rhs = IntExpr(Random.int_incl 1 3)}
    | CondTrue     -> TrueExpr
    | CondFalse    -> FalseExpr
  in AssignStmt{ assign_local = false;
                 assign_lhs = [cond_var_ident];
                 assign_rhs = [rhs] }

let gen_loop_block ctx env cond_var_ident cond_term =
  let block = GenUtil.gen_empty_block env in
  let term_stmt = gen_term_stmt cond_var_ident cond_term in
  match block with
  | Ast.BlockStmt block ->
    let body = (Random.int_incl 1 3) |> GenerateLinear.generate_stmts ctx env in
    let body = block.block_stmts @ body @ [term_stmt] in
    Ast.BlockStmt{ block with block_stmts = body }
  | _ -> assert false

let generate ctx env =
  let ast_loop_ty = if Random.bool () then Ast.Repeat else Ast.While in
  let (cond_var_ident, cond_var_def, cond_term) = gen_cond_var () in
  let loop_cond = gen_loop_cond cond_var_ident cond_term in
  let loop_block = gen_loop_block ctx env cond_var_ident cond_term in
  let loop = Ast.LoopStmt{ loop_cond;
                           loop_block;
                           loop_ty = ast_loop_ty }
  in
  [cond_var_def; loop]

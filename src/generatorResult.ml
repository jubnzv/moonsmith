open Core_kernel

(** Gets [expr] that always evaluated to an integer value and
    extends it with some random computations. *)
let gen_numeric_assign_rhs expr =
  expr

let get_id_name = function
  | Ast.IdentExpr id -> Printf.sprintf "res_%s" id.id_name
  | _ -> failwith "Expected IdentExpr"

(* TODO: It doesn't handle multiple arguments returned from the functions. But
         we don't do this for now. *)
let gen_rhs ctx stmt =
  let open Ast in
  match stmt with
  | AssignStmt assign -> begin
      let lhs = List.nth_exn assign.assign_lhs 0 in
      match lhs with
      | IdentExpr id -> begin
          match id.id_ty with
          | TyNil -> IntExpr(1)
          | TyBoolean -> begin
              (* Generate alternative of ternary operation in Lua:
                 http://lua-users.org/wiki/TernaryOperator *)
              let bin_rhs = BinExpr{ bin_lhs = IntExpr(1);
                                     bin_op = OpOr;
                                     bin_rhs = IntExpr(0); }
              in
              BinExpr{ bin_lhs = IdentExpr(id);
                       bin_op = OpAnd;
                       bin_rhs }
              |> gen_numeric_assign_rhs
            end
          | TyInt -> gen_numeric_assign_rhs lhs
          | TyFloat   -> Transform.to_int ctx TyFloat lhs
                         |> gen_numeric_assign_rhs
          | TyString  -> Transform.to_int ctx TyString lhs
                         |> gen_numeric_assign_rhs
          | TyIntString -> Transform.to_int ctx TyIntString lhs
                           |> gen_numeric_assign_rhs
          | TyTable   -> Transform.to_int ctx TyTable lhs
                         |> gen_numeric_assign_rhs
          | TyUserdata | TyThread | TyAny | TyFunction -> IntExpr(1)
        end
      | _ -> failwith "Impossible: Found non-IdentExpr in LHS of the assignment"
    end
  | _ -> failwith "Expected AssignStmt"

(** Generates statements that assign new temporary variable to result of a
    function call. *)
let func_result_to_num ctx acc funccall_stmt =
  let open Ast in
  match funccall_stmt with
  | AssignStmt assign -> begin
      if not @@ phys_equal 1 @@ List.length assign.assign_rhs then acc
      else match List.nth_exn assign.assign_rhs 0 with
        | FuncCallExpr _ -> begin
            let func_result = (List.nth_exn assign.assign_lhs 0) in
            let id_name = get_id_name func_result in
            let lhs = IdentExpr{ id_id = Context.get_free_idx ();
                                 id_name;
                                 id_ty = TyInt}
            and rhs = gen_rhs ctx funccall_stmt
            in acc @ [AssignStmt{ assign_local = true;
                                  assign_lhs = [lhs];
                                  assign_rhs = [rhs] }]
          end
        | _ -> acc
    end
  | _ -> acc

(** Generates new statements that assign new temporary variables used to
    calculate the result. *)
let datum_to_num ctx acc datum_stmt =
  let open Ast in
  let id_name = match datum_stmt with
    | AssignStmt assign -> get_id_name (List.nth_exn assign.assign_lhs 0)
    | _ -> failwith "Expected AssignStmt"
  in
  let lhs = IdentExpr{ id_id = Context.get_free_idx ();
                       id_name;
                       id_ty = TyInt}
  and rhs = gen_rhs ctx datum_stmt
  in acc @ [AssignStmt{ assign_local = true;
                        assign_lhs = [lhs];
                        assign_rhs = [rhs] }]

(** Generates a random operator used to combine results. *)
let gen_random_combine_op () =
  let open Ast in
  match Random.bool () with
  | true  -> OpAdd  (* + *)
  | false -> OpSub  (* - *)

(** Generates an assign statements that combines generated intermediate
    results to a single number. *)
let gen_combine_stmt num_stmts =
  let open Ast in
  let combine acc stmt =
    match acc with
    | None -> begin
        match stmt with
        | AssignStmt assign -> Some(List.nth_exn assign.assign_lhs 0)
        | _ -> failwith "Expected AssignStmt"
      end
    | Some acc -> begin
        match stmt with
        | AssignStmt assign -> begin
            let bin_lhs = List.nth_exn assign.assign_lhs 0 in
            let bin = BinExpr{ bin_lhs;
                               bin_op = gen_random_combine_op ();
                               bin_rhs = acc }
            in
            Some(bin)
          end
        | _ -> failwith "Expected AssignStmt"
      end
  in
  let combined_expr = List.fold_left
      num_stmts
      ~init:None
      ~f:combine
  in
  let lhs = IdentExpr{ id_id = Context.get_free_idx ();
                       id_name = "RESULT";
                       id_ty = TyInt; }
  and rhs = Option.value combined_expr ~default:(IntExpr(42))
  in
  AssignStmt{ assign_local = false;
              assign_lhs = [lhs];
              assign_rhs = [rhs]; }

(** Generates a print statement for the combined result. *)
let gen_print_stmt ctx combine_stmt =
  let open Ast in
  let result_id = match combine_stmt with
    | AssignStmt assign -> begin
        match List.nth_exn assign.assign_lhs 0 with
        | IdentExpr id -> IdentExpr(id)
        | _ -> failwith "Impossible: Found non-IdentExpr in LHS of the assignment"
      end
    | _ -> failwith "Expected AssignStmt"
  in
  FuncCallStmt{ fc_expr = StdLib.mk_funccall "print" [ Transform.to_int ctx TyFloat result_id ] }

let generate (ctx : Context.t) =
  let open Context in
  let datum_to_num' = datum_to_num ctx in
  let func_result_to_num' = func_result_to_num ctx in
  let func_results_stmts = List.fold_left
      ctx.ctx_call_stmts
      ~init:[]
      ~f:func_result_to_num'
  and datum_stmts = List.fold_left
      ctx.ctx_datum_stmts
      ~init:[]
      ~f:datum_to_num'
  in
  let result_stmts = datum_stmts @ func_results_stmts in
  let combine_stmt = gen_combine_stmt result_stmts in
  let print_stmt = gen_print_stmt ctx combine_stmt in
  { ctx with ctx_result_stmts = result_stmts @ [combine_stmt; print_stmt] }

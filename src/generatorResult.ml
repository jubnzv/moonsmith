open Core_kernel

(** Gets [expr] that always evaluated to an integer value and
    extends it with some random computations. *)
let gen_numeric_assign_rhs expr =
  expr

(** Generates expression that converts the given [IdentExpr] from
    float to integer. *)
let float_to_int ctx expr =
  let can_convert_with_floor () =
    ctx.Context.ctx_config.Config.c_use_tostring &&
    ctx.Context.ctx_config.Config.c_use_string_sub &&
    ctx.Context.ctx_config.Config.c_use_length
  in
  let open Ast in
  if ctx.Context.ctx_config.Config.c_use_math_floor then begin
    (* Just use 'math.floor()'. *)
    let fcf_func = IdentExpr{ id_name = "math.floor";
                              id_ty = TyFunction }
    in
    let fc_ty = FCFunc{ fcf_func } in
    FuncCallExpr{ fc_id = -1;
                  fc_ty;
                  fc_args = [expr] }
  end
  else if can_convert_with_floor () then begin
    (* Floor with '// 1' and convert to string using 'tostring()'.
       Then remove .0 from the string and convert to integer. *)
    let floored = BinExpr{ bin_lhs = expr;
                           bin_op = OpFloor;
                           bin_rhs = IntExpr(1) }
    in
    let tostring = FuncCallExpr{
        fc_id = -1;
        fc_ty = FCFunc{
            fcf_func =
              IdentExpr{ id_name = "tostring";
                         id_ty = TyFunction } };
        fc_args = [floored] }
    in
    let len_expr = BinExpr{ bin_lhs = UnExpr{ un_op = OpLen;
                                              un_expr = tostring };
                            bin_op = OpSub;
                            bin_rhs = IntExpr(2) }
    in
    FuncCallExpr{
      fc_id = -1;
      fc_ty =
        FCFunc{
          fcf_func =
            IdentExpr{ id_name = "string.sub";
                       id_ty = TyFunction }};
      fc_args = [tostring; IntExpr(1); len_expr] }
  end
  else (* Well, we can't convert it in such restricted case. *)
    IntExpr(Random.int_incl 1 10)

(** Generates expression that converts the given [IdentExpr] from
    string to integer. *)
let string_to_int ctx expr =
  let open Ast in
  if ctx.Context.ctx_config.Config.c_use_length then
    (* Just take a length of the string. *)
    UnExpr{ un_op = OpLen;
            un_expr = expr }
  else (* Well, we can't convert it in such restricted case. *)
    IntExpr(Random.int_incl 1 10)

(** Generates expression that converts the given [IdentExpr] from
    table to integer. *)
let table_to_int ctx expr =
  let open Ast in
  if ctx.Context.ctx_config.Config.c_use_length then
    (* Just take a length of the table. *)
    UnExpr{ un_op = OpLen;
            un_expr = expr }
  else (* Well, we can't convert it in such restricted case. *)
    IntExpr(Random.int_incl 1 10)

(** Generates new statements that assigns new temporary variables used to
    calculate the result. *)
let gen_num_stmt ctx acc datum_stmt =
  let open Ast in
  let datum_name = ref "" in
  let gen_rhs () =
    match datum_stmt with
    | AssignStmt assign -> begin
        let lhs = List.nth_exn assign.assign_lhs 0 in
        match lhs with
        | IdentExpr id -> begin
            datum_name := id.id_name;
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
            | TyFloat -> float_to_int ctx lhs |> gen_numeric_assign_rhs
            | TyString -> string_to_int ctx lhs |> gen_numeric_assign_rhs
            | TyTable _ -> table_to_int ctx lhs |> gen_numeric_assign_rhs
            | TyUserdata | TyThread | TyFunction -> IntExpr(1)
          end
        | _ -> assert false
      end
    | _ -> assert false
  in
  let rhs = gen_rhs () in
  let id_name = Printf.sprintf "r_%s" !datum_name in
  let lhs = IdentExpr{ id_name;
                       id_ty = TyInt}
  in
  let assign = AssignStmt{ assign_local = false;
                           assign_lhs = [lhs];
                           assign_rhs = [rhs] }
  in
  acc @ [assign]

(** Generates a random operator used to combine results. *)
let gen_random_combine_op () =
  let open Ast in
  match Random.int_incl 0 7 with
  | 0 -> OpAdd  (* + *)
  | 1 -> OpSub  (* - *)
  | 2 -> OpMul  (* * *)
  | 3 -> OpBAnd (* & *)
  | 4 -> OpBOr  (* ~ *)
  | 5 -> OpBRs  (* >> *)
  | 6 -> OpBLs  (* << *)
  | _ -> OpBNot (* ~ *)

(** Generates an assign statements that combines generated intermediate
    results to a single number. *)
let gen_combine_stmt num_stmts =
  let open Ast in
  let combine acc stmt =
    match acc with
    | None -> begin
        match stmt with
        | AssignStmt assign -> Some(List.nth_exn assign.assign_lhs 0)
        | _ -> assert false
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
        | _ -> assert false
      end
  in
  let combined_expr = List.fold_left
      num_stmts
      ~init:None
      ~f:combine
  in
  let lhs = IdentExpr{ id_name = "RESULT";
                       id_ty = TyInt; }
  and rhs = match combined_expr with
    | Some rhs -> rhs
    | None -> IntExpr(42)
  in
  AssignStmt{ assign_local = false;
              assign_lhs = [lhs];
              assign_rhs = [rhs]; }

(** Generates a print statement for the combined result. *)
let gen_print_stmt combine_stmt =
  let open Ast in
  let result_id = match combine_stmt with
    | AssignStmt assign -> begin
        match List.nth_exn assign.assign_lhs 0 with
        | IdentExpr id -> IdentExpr(id)
        | _ -> assert false
      end
    | _ -> assert false
  in
  let fcf_func = IdentExpr{ id_name = "print";
                            id_ty = TyFunction }
  in
  let fc_ty = FCFunc{ fcf_func } in
  let fc_expr = FuncCallExpr{ fc_id = -1;
                              fc_ty;
                              fc_args = [result_id] }
  in
  FuncCallStmt{ fc_expr }

let generate (ctx : Context.t) =
  let open Context in
  let gen_num_stmt' = gen_num_stmt ctx in
  let num_stmts = List.fold_left
      ctx.ctx_datum_stmts
      ~init:[]
      ~f:gen_num_stmt'
  in
  let combine_stmt = gen_combine_stmt num_stmts in
  let print_stmt = gen_print_stmt combine_stmt in
  { ctx with ctx_result_stmts = num_stmts @ [combine_stmt; print_stmt] }

open Core_kernel

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
    let fcf_func = IdentExpr{ id_id = -1;
                              id_name = "math.floor";
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
              IdentExpr{ id_id = -1;
                         id_name = "tostring";
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
            IdentExpr{ id_id = -1;
                       id_name = "string.sub";
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

let to_nil ctx ty expr =
  let open Ast in
  (* TODO: This is not finished. *)
  NilExpr

let to_boolean ctx ty expr =
  let open Ast in
  match ty with
  | TyNil -> begin
      (* 'not nil' always returns true. *)
      UnExpr { un_op = OpNot;
               un_expr = expr }
    end
  (* TODO: This is not finished. *)
  | _ -> begin
      if Random.bool () then TrueExpr
      else                   FalseExpr
    end

let to_int ctx ty expr =
  let open Ast in
  match ty with
  | TyNil      -> IntExpr(Random.int_incl (-100) 100)
  | TyBoolean  -> IntExpr(Random.int_incl (-100) 100)
  | TyInt      -> expr
  | TyFloat    -> float_to_int ctx expr
  | TyString   -> string_to_int ctx expr
  | TyFunction -> IntExpr(Random.int_incl (-100) 100)
  | TyTable    -> IntExpr(Random.int_incl (-100) 100)
  | TyThread | TyUserdata | TyAny -> IntExpr(Random.int_incl (-100) 100)

let to_float ctx ty expr =
  let open Ast in
  (* TODO: This is not finished. *)
  FloatExpr(Random.float 100.0)

let to_string ctx ty expr =
  let open Ast in
  (* TODO: This is not finished. *)
  StringExpr(StringGen.gen_string ())

let to_function ctx ty expr =
  let open Ast in
  (* TODO: This is not finished. *)
  let lambda_body =
    ReturnStmt{ return_exprs = [IntExpr(Random.int_incl (-100) (100))] }
  in
  LambdaExpr{ lambda_args = [];
              lambda_body }

let to_table ctx ty expr =
  let open Ast in
  (* TODO: This is not finished. *)
  match ty with
  | TyTable -> expr
  | _ -> begin
      let dummy = IntExpr(42) in
      TableExpr(TArray{ table_elements = [dummy] })
    end

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
    StdLib.mk_funccall "math.floor" [expr]
  end
  else if can_convert_with_floor () then begin
    (* Floor with '// 1' and convert to string using 'tostring()'.
       Then remove .0 from the string and convert to integer. *)
    let floored = BinExpr{ bin_lhs = expr;
                           bin_op = OpFloor;
                           bin_rhs = IntExpr(1) }
    in
    let tostring = StdLib.mk_funccall "tostring" [floored] in
    let len_expr = BinExpr{ bin_lhs = UnExpr{ un_op = OpLen;
                                              un_expr = tostring };
                            bin_op = OpSub;
                            bin_rhs = IntExpr(2) }
    in
    StdLib.mk_funccall "string.sub" [tostring; IntExpr(1); len_expr]
  end
  else (* Well, we can't convert it in such restricted case. *)
    IntExpr(Random.int_incl 1 10)

(** Generates expression that converts the given [IdentExpr] from
    string to integer. *)
let string_to_int ctx expr =
  let open Ast in
  let open Context in
  let open Config in
  let fallback () = IntExpr(Random.int_incl 1 10) in
  Util.choose [(ctx.ctx_config.c_use_length,
                lazy (UnExpr{ un_op = OpLen; un_expr = expr }));
               (ctx.ctx_config.c_use_string_len,
                lazy (StdLib.mk_funccall "string.len" [expr]))]
  @@ lazy (fallback ())

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
  let fallback () = if Random.bool () then TrueExpr else FalseExpr in
  match ty with
  | TyNil -> begin
      (* 'not nil' always returns true. *)
      UnExpr{ un_op = OpNot;
              un_expr = expr }
    end
  | TyInt -> begin
      if ctx.Context.ctx_config.Config.c_use_math_ult then
        StdLib.mk_funccall "math.ult" [expr; IntExpr(Random.int_incl (-20) 20)]
      else
        fallback ()
    end
  | _ -> fallback ()

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
  let open Context in
  let open Config in
  let fallback () =
    if ctx.ctx_config.c_use_math_pi && Random.bool () then
      StdLib.mk_ident ~ty:TyFloat "math.pi"
    else
      FloatExpr(Random.float 100.0)
  in
  match ty with
  | TyInt | TyFloat -> begin
      Util.choose [(ctx.ctx_config.c_use_math_sin,
                    lazy (StdLib.mk_funccall "math.sin" [expr]));
                   (ctx.ctx_config.c_use_math_cos,
                    lazy (StdLib.mk_funccall "math.cos" [expr]))
                  ]
      @@ lazy (fallback ())
    end
  | _ -> fallback ()

let to_string ctx ty expr =
  let open Ast in
  let open Config in
  let open Context in
  let fallback () = StringExpr(StringGen.gen_string ()) in
  match ty with
  | TyInt -> begin
      if ctx.ctx_config.c_use_math_type then
        StdLib.mk_funccall "math.type" [expr]
      else
        fallback ()
    end
  | TyFloat -> begin
      if ctx.ctx_config.c_use_math_type then
        StdLib.mk_funccall "math.type" [expr]
      else
      if Random.bool () then fallback () else expr
    end
  | TyString -> begin
      (* string.reverse is horrible slow. Reduce it calls as much as possible. *)
      Util.choose [(ctx.ctx_config.c_use_string_reverse && phys_equal 0 @@ Random.int_incl 0 100,
                    lazy (StdLib.mk_funccall "string.reverse" [expr]));
                   (true,
                    lazy (expr))]
      @@ lazy (fallback ())
    end
  | _ -> fallback ()

let to_function ctx ty expr =
  let open Ast in
  let fallback () =
    let lambda_body =
      ReturnStmt{ return_exprs = [IntExpr(Random.int_incl (-100) (100))] }
    in
    LambdaExpr{ lambda_args = [];
                lambda_body }
  in
  fallback ()

let to_table ctx ty expr =
  let open Ast in
  let fallback () =
    let dummy = IntExpr(42) in
    TableExpr(TArray{ table_elements = [dummy] })
  in
  match ty with
  | TyTable -> expr
  | _ -> fallback ()

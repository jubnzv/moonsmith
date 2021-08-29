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
    string containing only integers to integer. *)
let int_string_to_int ctx expr =
  let open Ast in
  let open Context in
  let open Config in
  let fallback () = IntExpr(Random.int_incl 1 10) in
  Util.choose [(ctx.ctx_config.c_use_tonumber),
               lazy (StdLib.mk_funccall "tonumber" [expr]);
               (* Use string conversion: "123"+1*)
               (true,
                lazy (BinExpr{ bin_lhs = expr;
                               bin_op = OpAdd;
                               bin_rhs = IntExpr(Random.int_incl 0 3) }
                      |> float_to_int ctx))]
  @@ lazy (fallback ())

(** Generates expression that converts the given [IdentExpr] from
    table to integer. *)
let table_to_int ctx expr =
  let open Ast in
  let open Config in
  let open Context in
  let fallback () = IntExpr(Random.int_incl 1 10) in
  Util.choose [(ctx.ctx_config.c_use_length),
               lazy (UnExpr{ un_op = OpLen; un_expr = expr });
               (Option.is_some ctx.ctx_config.c_lib_path),
               lazy (StdLib.mk_funccall "ms.table_to_int" [expr])]
  @@ lazy (fallback ())

let to_nil ctx ty expr =
  let open Ast in
  let open Config in
  let open Context in
  let fallback () = NilExpr in
  match ty with
  | TyString -> begin
      (* Using tonumber() with non-numeric string results as nil. *)
      Util.choose [(ctx.ctx_config.c_use_tonumber),
                   lazy (StdLib.mk_funccall "tonumber" [expr])]
      @@ lazy (fallback ())
    end
  | TyInt | TyFloat -> begin
      (* Using 'nil and <numeric value>' always results as nil. *)
      Util.choose [(true),
                   lazy (BinExpr{ bin_lhs = NilExpr;
                                  bin_op = OpAnd;
                                  bin_rhs = expr })]
      @@ lazy (fallback ())
    end
  | TyNil -> expr
  | _ -> fallback ()

let to_boolean ctx ty expr =
  let open Ast in
  let open Context in
  let open Config in
  let fallback () = if Random.bool () then TrueExpr else FalseExpr in
  match ty with
  | TyBoolean -> begin
      Util.choose_lazy_exn [
        lazy (BinExpr{ bin_lhs = fallback ();
                       bin_op = Util.choose_one_exn [OpEq; OpNeq];
                       bin_rhs = expr });
        lazy (expr);]
    end
  | TyInt -> begin
      Util.choose [(ctx.ctx_config.c_use_math_ult),
                   lazy (StdLib.mk_funccall "math.ult" [expr; IntExpr(Random.int_incl (-20) 20)]) ]
      @@ lazy (fallback ())
    end
  | TyFloat -> fallback ()
  | TyString | TyIntString -> begin
      Util.choose [(true),
                   lazy (BinExpr{ bin_lhs = StringExpr(StringGen.gen_string ());
                                  bin_op = Util.choose_one_exn [OpEq; OpNeq];
                                  bin_rhs = expr })]
      @@ lazy (fallback ())
    end
  | TyTable | TyFunction -> begin
      Util.choose [(true),
                   lazy (BinExpr{ bin_lhs = expr;
                                  bin_op = Util.choose_one_exn [OpEq; OpNeq];
                                  bin_rhs = NilExpr })]
      @@ lazy (fallback ())
    end
  | TyNil -> begin
      (* Some logical operations between Booleans and nils returns Booleans. *)
      Util.choose_lazy_exn [
        lazy (BinExpr{ bin_lhs = TrueExpr;
                       bin_op = OpOr;
                       bin_rhs = expr });
        lazy (BinExpr{ bin_lhs = expr;
                       bin_op = OpAnd;
                       bin_rhs = FalseExpr });
        (* 'not nil' always returns true. *)
        lazy (UnExpr{ un_op = OpNot;
                      un_expr = expr }) ]
    end
  | TyUserdata | TyThread | TyAny -> fallback ()

let to_int ctx ty expr =
  let open Ast in
  let fallback () = IntExpr(Random.int_incl (-100) 100) in
  match ty with
  | TyNil       -> fallback ()
  | TyBoolean   -> fallback ()
  | TyInt       -> expr
  | TyFloat     -> float_to_int ctx expr
  | TyString    -> string_to_int ctx expr
  | TyIntString -> int_string_to_int ctx expr
  | TyFunction  -> fallback () (* TODO: Check return type and call, but prevent recursion. *)
  | TyTable     -> table_to_int ctx expr
  | TyThread | TyUserdata | TyAny -> fallback ()

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

let to_int_string ctx ty expr =
  let open Ast in
  let open Config in
  let open Context in
  let fallback () = StringExpr(StringGen.gen_int_string ()) in
  match ty with
  | TyInt -> begin
      if ctx.ctx_config.c_use_tostring then
        StdLib.mk_funccall "tostring" [expr]
      else
        fallback ()
    end
  | TyFloat -> begin
      Util.choose [(ctx.ctx_config.c_use_tostring),
                   lazy (StdLib.mk_funccall "tostring" [float_to_int ctx expr])]
      @@ lazy (fallback ())
    end
  | TyIntString -> expr
  | _ -> fallback ()

let to_function _ _ expr =
  let open Ast in
  let fallback () =
    let lambda_body =
      ReturnStmt{ return_exprs = [expr] }
    in
    LambdaExpr{ lambda_args = [];
                lambda_body }
  in
  fallback ()

let to_table _ ty expr =
  let open Ast in
  let fallback () =
    let dummy = IntExpr(Random.int_incl (-10) 10) in
    TableExpr(TArray{ table_elements = [dummy] })
  in
  match ty with
  | TyTable -> expr
  | TyFloat | TyInt -> begin
      (* Generate an array table. *)
      Util.choose_lazy_exn [lazy (TableExpr(TArray{ table_elements = [expr] }));
                            lazy (fallback ())]
    end
  | TyString | TyIntString | TyBoolean | TyNil -> begin
      (* Generate a hashmap table. *)
      Util.choose_lazy_exn [
        lazy (let idx = Context.get_free_idx () in
              TableExpr(THashMap{
                  table_fields = [{ tf_key = IdentExpr{ id_id = idx;
                                                        id_name = Printf.sprintf "t%d" idx;
                                                        id_ty = ty};
                                    tf_value = expr }] }));
        lazy (fallback ())]
    end
  | TyThread | TyUserdata | TyAny | TyFunction -> fallback ()

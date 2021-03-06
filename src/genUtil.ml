open Core_kernel

let gen_ty () =
  let open Ast in
  match Random.int_incl 0 7 with
  | 0 -> TyNil
  | 1 -> TyBoolean
  | 2 -> TyInt
  | 3 -> TyFloat
  | 4 -> TyString
  | 5 -> TyIntString
  | 6 -> TyFunction
  | _ -> TyTable

let gen_simple_ty () =
  let open Ast in
  Util.choose_one_exn [TyBoolean; TyInt; TyFloat; TyString; TyIntString]

let gen_array_table_init () =
  let open Ast in
  let gen_array args_num =
    let gen acc =
      if args_num <= List.length acc then
        [IntExpr(Random.int_incl (-20) 20)]
        |> List.append acc
      else acc
    in
    let table_elements = gen [] in
    TableExpr(TArray{ table_elements })
  in
  Random.int_incl 1 5 |> gen_array

let gen_simple_expr always_positive =
  let open Ast in
  match Random.int_incl 0 4 with
  | 0 -> TrueExpr
  | 1 -> FalseExpr
  | 2 ->
    let (l, r) =
      if always_positive then (0, 200)
      else (-100, 100)
    in
    IntExpr(Random.int_incl l r)
  | 3 ->
    let (l, r) =
      if always_positive then (-100.0), 100.0
      else 0.0, 200.0
    in
    FloatExpr(Random.float_range l r)
  | _ -> StringExpr(StringGen.gen_string ())

let gen_simple_expr ?(ty = Ast.TyAny) ?(always_positive = false) () =
  let open Ast in
  match ty with
  | TyAny -> gen_simple_expr always_positive
  | TyNil     -> NilExpr
  | TyBoolean -> begin
      match Random.bool () with
      | true -> TrueExpr
      | _ -> FalseExpr
    end
  | TyInt ->
    if always_positive then IntExpr(Random.int_incl 0 150)
    else IntExpr(Random.int_incl (-100) 100)
  | TyFloat ->
    if always_positive then FloatExpr(Random.float 150.0)
    else FloatExpr(Random.float_range (-100.0) 100.0)
  | TyString  -> StringExpr(StringGen.gen_string ())
  | TyIntString  -> StringExpr(StringGen.gen_int_string ())
  | TyTable -> gen_array_table_init ()
  | TyFunction -> begin
      let lambda_body =
        ReturnStmt{ return_exprs = [IntExpr(Random.int_incl (-100) (100))] }
      in
      LambdaExpr{ lambda_args = [];
                  lambda_body }
    end
  | TyThread | TyUserdata -> NilExpr

let gen_hash_table_init ?(ty = Ast.TyAny) () =
  let open Ast in
  let gen_hashmap args_num =
    let gen acc =
      if args_num > List.length acc then
        let idx = Context.get_free_idx () in
        let tf_key = IdentExpr{ id_id = idx;
                                id_name = Printf.sprintf "field%d" idx;
                                id_ty = ty }
        and tf_value = gen_simple_expr ~ty:ty () in
        acc @ [{ tf_key; tf_value }]
      else acc
    in
    let table_fields = gen [] in
    TableExpr(THashMap{ table_fields })
  in
  Random.int_incl 1 5 |> gen_hashmap

let gen_compare_binop ty =
  let open Ast in
  match ty with
  | TyInt | TyFloat -> begin
      match Random.int_incl 0 6 with
      | 0 -> OpEq
      | 2 -> OpNeq
      | 3 -> OpLt
      | 4 -> OpLte
      | 5 -> OpGt
      | _ -> OpGte
    end
  | TyNil | TyBoolean | TyString | TyIntString | TyUserdata
  | TyFunction | TyThread | TyTable | TyAny -> if Random.bool () then OpEq else OpNeq

let gen_ident ?(add_now=false) ?(name=None) env =
  let open Ast in
  let idx = Context.get_free_idx () in
  let name = Option.value name ~default:(Printf.sprintf "v%d" idx) in
  let i = IdentExpr{ id_id = idx;
                     id_name = name;
                     id_ty = (gen_simple_ty ()) } in
  let (_ : unit) =
    if add_now then begin env_add_binding env i;         end
    else            begin env_add_pending_binding env i; end
  in
  i

let gen_dummy_block () =
  let block_env = Ast.env_mk () in
  Ast.BlockStmt { block_stmts = [];
                  block_is_loop = false;
                  block_env }

let gen_rhs_to_assign_ident ctx env expr  =
  let open Ast in
  match expr with
  | IdentExpr id -> begin
      match Random.int_incl 0 2 with
      | 0 (* generate a simple expr *) -> begin
          gen_simple_expr ~ty:(id.id_ty) ()
        end
      | 1 (* try to peek a local expr *) -> begin
          if env_empty env then
            gen_simple_expr ~ty:(id.id_ty) ()
          else begin
            let binds_with_same_type = List.filter
                env.env_bindings
                ~f:(fun expr_ref -> begin
                      match !expr_ref with
                      | IdentExpr nid -> begin
                          if equal_ty nid.id_ty id.id_ty then true
                          else false
                        end
                      | _ -> false (* who knows, let's just skip *)
                    end)
            in
            if List.is_empty binds_with_same_type then
              gen_simple_expr ~ty:(id.id_ty) ()
            else begin
              ! (Util.choose_one_exn binds_with_same_type)
            end
          end
        end
      | _ (* try to peek a global datum *) -> begin
          Context.peek_typed_datum ctx id.id_ty
          |> Option.value ~default:(gen_simple_expr ~ty:(id.id_ty) ())
        end
    end
  | _ -> failwith "Impossible: Trying to generate RHS for not IdentExpr"

let gen_init_stmt_for_ident ?(assign_local = false) expr =
  let open Ast in
  let gen_init_stmt rhs =
    AssignStmt{ assign_local;
                assign_lhs = [expr];
                assign_rhs = [rhs] }
  in
  match expr with
  | IdentExpr id -> begin
      match id.id_ty with
      | TyNil     -> gen_init_stmt NilExpr
      | TyBoolean ->
        let rhs = match Random.bool () with
          | true -> TrueExpr
          | _ -> FalseExpr
        in
        gen_init_stmt rhs
      | TyInt -> IntExpr(Random.int_incl (-100) 100) |> gen_init_stmt
      | TyFloat -> FloatExpr(Random.float 100.0) |> gen_init_stmt
      | TyString -> StringExpr(StringGen.gen_string ()) |> gen_init_stmt
      | TyIntString -> StringExpr(StringGen.gen_int_string ()) |> gen_init_stmt
      | TyFunction  -> begin
          let lambda_body =
            ReturnStmt{ return_exprs = [IntExpr(Random.int_incl (-100) (100))] }
          in
          LambdaExpr{ lambda_args = [];
                      lambda_body }
          |> gen_init_stmt
        end
      | TyTable -> gen_array_table_init () |> gen_init_stmt
      | TyAny   -> gen_init_stmt NilExpr
      | TyUserdata | TyThread -> failwith "Userdata and Thread types are unsupported"

    end
  | _ -> failwith "Impossible: Got not IdentExpr as LHS of the assign statement"

let take_random_binding env =
  let open Ast in
  let rec aux ?(use_this_level = false) env level visited =
    (* Use this environment and visited ones. *)
    let stop_here () =
      if (env_empty env) then
        if List.is_empty visited then
          None
        else
          aux ~use_this_level:true !(List.hd_exn visited) 0 []
      else (* This env is not empty. Use it. *)
        aux ~use_this_level:true env level visited
    (* Walk to the parent environment. We want to reach the depth [level]. *)
    and walk_previous () =
      visited @ [ref env]
      |> aux (env_get_parent_exn env) (level - 1)
    in
    if use_this_level then
      if env_empty env then None
      else Some(env_peek_random_exn env)
    else if not (env_empty env) then
      if (env_has_parent env) && not (phys_equal level 0) then
        walk_previous ()
      else
        stop_here ()
    else (* env is empty *)
    if (env_has_parent env) && not (phys_equal level 0) then
      walk_previous () (* May be there are non-empty parents. *)
    else
      stop_here ()
  in
  let select_level () =
    (* We prefer the most near scopes:
         30% - 2 levels
         30% - 3 levels
         25% - 4 levels
         15% - 5 levels *)
    let r = Random.int_incl 0 100 in
    if      r < 30 then 2
    else if r < 60 then 3
    else if r < 85 then 4
    else                5
  in
  aux env (select_level ()) []

(** Generates unary operator which can take expression of type [ty] and returns
    an expression of the same [ty]. *)
let gen_combine_unop ty =
  let open Ast in
  match ty with
  | TyNil      -> None
  | TyBoolean  -> Some(OpNot)
  | TyInt      -> begin
      match Random.bool () with
      | true -> Some(OpSub)  (* - *)
      | _ ->    Some(OpBNot) (* ~ *)
    end
  | TyFloat     -> Some(OpSub)
  | TyString    -> None
  | TyIntString -> None
  | TyFunction  -> None
  | TyTable     -> None
  | TyAny       -> None
  | TyThread | TyUserdata -> None

(** Generates name of the function which takes a single argument of type [ty]
    and returns an expression of the same [ty]. *)
let gen_combine_un_funcall ctx ty =
  let open Ast in
  let open Context in
  let open Config in
  match ty with
  | TyNil      -> None
  | TyBoolean  -> None
  | TyInt ->
    Util.choose [(ctx.ctx_config.c_use_math_log,
                  lazy (Some("math.log")));
                 (true),
                 lazy (None)]
    @@ lazy (None)
  | TyFloat ->
    Util.choose [(ctx.ctx_config.c_use_math_log,
                  lazy (Some("math.log")));
                 (true),
                 lazy (None)]
    @@ lazy (None)
  | TyString ->
    Util.choose [(ctx.ctx_config.c_use_string_lower,
                  lazy (Some("string.upper")));
                 (ctx.ctx_config.c_use_string_upper,
                  lazy (Some("string.lower")));
                 (true),
                 lazy (None)]
    @@ lazy (None)
  | TyIntString -> None
  | TyFunction  -> None
  | TyTable     -> None
  | TyAny       -> None
  | TyThread | TyUserdata -> None

(** Generates binary operator which can take two expressions of type [ty] and
    returns an expression of the same [ty]. The type of the generated operator
    depends of values of [lhs] and [rhs], because we must avoid result
    expresssions which exceeds Lua's number limits. *)
let gen_combine_binop ty lhs rhs =
  let open Ast in
  match ty with
  | TyNil      -> None
  | TyBoolean  -> begin
      match Random.bool () with
      | true -> Some(OpEq)  (* == *)
      | _    -> Some(OpNeq) (* ~= *)
    end
  | TyInt      -> begin
      (* Chooses "safe" operand that won't cause number overflow. *)
      let select_safe () =
        match Random.int_incl 0 4 with
        | 0 -> Some(OpAdd)  (* + *)
        | 1 -> Some(OpSub)  (* - *)
        | 3 -> Some(OpBAnd) (* & *)
        | _ -> Some(OpBOr)  (* ~ *)
      in
      match lhs, rhs with
      | IntExpr(l), IntExpr(r) -> begin
          (* If we have both even integers, we could safely divide them. *)
          if (phys_equal (l % 2) 0) && (phys_equal (r % 2) 0) &&
             (not @@ phys_equal r 0) then
            if Random.bool() then
              Some(OpDiv) (* / *)
            else
              Some(OpMod) (* % *)
          else if (l < 10) && (r < 3) then
            (* We could safely perform some operations on small integers. *)
            match Random.int_incl 0 4 with
            | 0 -> Some(OpPow)  (* ^ *)
            | 1 -> Some(OpBRs)  (* >> *)
            | 2 -> Some(OpMul)  (* * *)
            | _ -> Some(OpBLs)  (* << *)
          else
            select_safe ()
        end
      | _ -> select_safe ()
    end
  | TyFloat    -> begin
      (* Select operator that can't cause number overflow or zero division. *)
      let select_safe () =
        if Random.bool () then
          Some(OpAdd)  (* + *)
        else
          Some(OpSub)  (* - *)
      in
      match lhs, rhs with
      | FloatExpr(l), FloatExpr(r) -> begin
          if ((int_of_float l) < 10) &&
             ((int_of_float r) < 3) &&
             (not @@ phys_equal r 0.0) then
            match Random.int_incl 0 3 with
            | 0 -> Some(OpMul)  (* * *)
            | 1 -> Some(OpDiv)  (* / *)
            | 2 -> Some(OpPow)  (* ^ *)
            | _ -> Some(OpMod)  (* % *)
          else select_safe ()
        end
      | _ -> select_safe ()
    end
  | TyString    -> None (* skip, because concat ('..') operators are horrible slow *)
  | TyIntString -> None (* skip, because concat ('..') operators are horrible slow *)
  | TyFunction  -> None
  | TyTable     -> None
  | TyAny       -> None
  | TyThread | TyUserdata -> None

(** Combines [exprs] of the same type [ty] to a single expression. *)
let rec combine ctx ty exprs =
  let open Ast in
  match exprs with
  | [] -> begin
      None
    end
  | [expr] -> begin
      match gen_combine_unop ty with
      | Some un_op -> begin
          (* Create an unary expression. *)
          Some(UnExpr{ un_op;
                       un_expr = expr })
        end
      | None -> begin
          (* Create a function call with a single argument. *)
          match gen_combine_un_funcall ctx ty with
          | Some func_name -> begin
              let fcf_func = IdentExpr{ id_id = Context.get_free_idx ();
                                        id_name = func_name;
                                        id_ty = TyFunction } in
              let fc_ty = FCFunc{ fcf_func } in
              Some(FuncCallExpr{ fc_id = -1;
                                 fc_ty;
                                 fc_args = [expr] })
            end
          | None -> begin
              (* Just use this value "as is". *)
              Some(expr)
            end
        end
    end
  | lhs::head -> begin
      let rhs_opt = combine ctx ty head in
      match rhs_opt with
      | Some rhs -> begin
          match gen_combine_binop ty lhs rhs with
          | Some bin_op -> begin
              (* Create a binary operator. *)
              Some(BinExpr{ bin_lhs = lhs;
                            bin_op;
                            bin_rhs = rhs; })
            end
          (* TODO: Add standard functions with two arguments. *)
          | None -> begin
              (* Well, we can't create such expressions yet. *)
              Some(lhs)
            end
        end
      | None -> None
    end

let combine_to_typed_expr ctx ty exprs =
  let open Ast in
  let type_conv = match ty with
    | TyNil       -> Transform.to_nil
    | TyBoolean   -> Transform.to_boolean
    | TyInt       -> Transform.to_int
    | TyFloat     -> Transform.to_float
    | TyString    -> Transform.to_string
    | TyIntString -> Transform.to_int_string
    | TyFunction  -> Transform.to_function
    | TyTable     -> Transform.to_table
    | TyThread | TyUserdata | TyAny -> Transform.to_nil
  in
  List.fold_left
    exprs
    ~init:[]
    ~f:(fun acc expr -> begin
          let e_ty = Option.value (get_essential_ty expr) ~default:TyNil in
          acc @ [type_conv ctx e_ty expr]
        end)
  |> combine ctx ty

(** Extends the BlockStmt [block] adding given [stmt] to the end of the block. *)
let extend_block_stmt block stmts =
  match block with
  | Ast.BlockStmt block -> begin
      let block_stmts = block.block_stmts @ stmts in
      Ast.BlockStmt { block with block_stmts }
    end
  | _ -> block

let gen_empty_block ?(is_loop = false) parent_env =
  let block_env = Ast.env_mk () in
  let block_env = { block_env with env_parent = Some(ref parent_env) } in
  let block_stmts = [] in
  Ast.env_add_child parent_env block_env;
  Ast.BlockStmt{ block_stmts;
                 block_is_loop = is_loop;
                 block_env }

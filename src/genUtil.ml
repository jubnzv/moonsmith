open Core_kernel

let gen_ty () =
  let open Ast in
  match Random.int_incl 0 6 with
  | 0 -> TyNil
  | 1 -> TyBoolean
  | 2 -> TyInt
  | 3 -> TyFloat
  | 4 -> TyString
  | 5 -> TyFunction
  | _ -> table_mk_empty ()

let gen_simple_ty () =
  let open Ast in
  Util.choose_one_exn [TyBoolean; TyInt; TyFloat; TyString]

let gen_simple_expr () =
  let open Ast in
  match Random.int_incl 0 5 with
  | 0 -> TrueExpr
  | 1 -> FalseExpr
  | 2 -> NilExpr
  | 3 -> IntExpr(Random.int_incl (-100) 100)
  | 4 -> FloatExpr(Random.float 100.00)
  | _ -> StringExpr(StringGen.gen_string ())

let gen_random_table_init () =
  let open Ast in
  let gen_array args_num =
    let gen acc =
      if args_num <= List.length acc then
        [gen_simple_expr ()] |> List.append acc
      else
        acc
    in
    let table_elements = gen [] in
    TableExpr(TArray{table_elements})
    (* and gen_hashmap args_num =              *)
    (*   let rec gen acc =                     *)
    (*     if args_num <= List.length acc then *)
    (*     else                                *)
    (*       acc                               *)
    (*     in                                  *)
    (*     acc @ []                            *)
    (*       gen []                            *)
  in
  let args_num = Random.int_incl 0 5 in
  match Random.bool () with
  | _ -> gen_array args_num
(* TODO: | _ -> gen_hashmap args_num *)

let gen_simple_typed_expr ty =
  let open Ast in
  match ty with
  | TyNil     -> NilExpr
  | TyBoolean -> begin
      match Random.bool () with
      | true -> TrueExpr
      | _ -> FalseExpr
    end
  | TyInt     -> IntExpr(Random.int_incl (-100) 100)
  | TyFloat   -> FloatExpr(Random.float 100.0)
  | TyString  -> StringExpr(StringGen.gen_string ())
  | TyTable _ -> gen_random_table_init ()
  | TyFunction -> begin
      let lambda_body =
        ReturnStmt{ return_exprs = [IntExpr(Random.int_incl (-100) (100))] }
      in
      LambdaExpr{ lambda_args = [];
                  lambda_body }
    end
  | TyThread | TyUserdata -> NilExpr

let gen_ident ?(add_now=false) ?(name=None) env =
  let open Ast in
  let name = match name with
    | Some(n) -> n
    | None -> Printf.sprintf "v%d" @@ Context.get_free_idx ()
  in
  let i = IdentExpr{ id_name = name;
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
      | 0 (* generate simple expr *) -> begin
          gen_simple_typed_expr id.id_ty
        end
      | 1 (* try to peek local expr *) -> begin
          if env_empty env then
            gen_simple_typed_expr id.id_ty
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
            if phys_equal 0 @@ List.length binds_with_same_type then
              gen_simple_typed_expr id.id_ty
            else begin
              ! (Util.choose_one_exn binds_with_same_type)
            end
          end
        end
      | _ (* try to peek global datum *) -> begin
          match Context.peek_typed_datum ctx id.id_ty with
          | None -> gen_simple_typed_expr id.id_ty
          | Some datum_lhs_expr -> datum_lhs_expr
        end
    end
  | _ -> assert false

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
      | TyFunction  -> begin
          let lambda_body =
            ReturnStmt{ return_exprs = [IntExpr(Random.int_incl (-100) (100))] }
          in
          LambdaExpr{ lambda_args = [];
                      lambda_body }
          |> gen_init_stmt
        end
      | TyTable _ -> gen_random_table_init () |> gen_init_stmt
      | TyUserdata | TyThread -> assert false
    end
  | _ -> assert false

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

(** Generates combination operator for integer typed expressions. *)
let gen_int_combine_binop () =
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
  | TyFloat    -> Some(OpSub)
  | TyString   -> None
  | TyFunction -> None
  | TyTable _  -> None
  | TyThread | TyUserdata -> None

(** Generates name of the function which takes a single argument of type [ty]
    and returns an expression of the same [ty]. *)
let gen_combine_un_funcall ctx ty =
  (* TODO: This is not finished. Need more functions from standard library. *)
  let open Ast in
  match ty with
  | TyNil      -> None
  | TyBoolean  -> None
  | TyInt      -> begin
      if ctx.Context.ctx_config.Config.c_use_math_log then
        Some("math.log")
      else
        None
    end
  | TyFloat    -> begin
      if ctx.Context.ctx_config.Config.c_use_math_log then
        Some("math.log")
      else
        None
    end
  | TyString   -> begin
      if Random.bool () && ctx.Context.ctx_config.Config.c_use_string_lower then
        Some("string.lower")
      else if ctx.Context.ctx_config.Config.c_use_string_upper then
        Some("string.upper")
      else
        None
    end
  | TyFunction -> None
  | TyTable _  -> None
  | TyThread | TyUserdata -> None

(** Generates binary operator which can take two expressions of type [ty] and
    returns an expression of the same [ty]. *)
let gen_combine_binop ty =
  let open Ast in
  match ty with
  | TyNil      -> None
  | TyBoolean  -> begin
      match Random.bool () with
      | true -> Some(OpEq)  (* == *)
      | _    -> Some(OpNeq) (* ~= *)
    end
  | TyInt      -> begin
      match Random.int_incl 0 9 with
      | 0 -> Some(OpAdd)  (* + *)
      | 1 -> Some(OpSub)  (* - *)
      | 2 -> Some(OpMul)  (* * *)
      | 4 -> Some(OpPow)  (* ^ *)
      | 5 -> Some(OpMod)  (* % *)
      | 6 -> Some(OpBAnd) (* & *)
      | 7 -> Some(OpBOr)  (* ~ *)
      | 8 -> Some(OpBRs)  (* >> *)
      | _ -> Some(OpBLs)  (* << *)
    end
  | TyFloat    -> begin
      match Random.int_incl 0 5 with
      | 0 -> Some(OpAdd)  (* + *)
      | 1 -> Some(OpSub)  (* - *)
      | 2 -> Some(OpMul)  (* * *)
      | 4 -> Some(OpPow)  (* ^ *)
      | _ -> Some(OpMod)  (* % *)
    end
  | TyString   -> begin
      Some(OpConcat)
    end
  | TyFunction -> None
  | TyTable _  -> None
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
          (* Create unary expression. *)
          Some(UnExpr{ un_op;
                       un_expr = expr })
        end
      | None -> begin
          (* Create a function call with a single argument. *)
          match gen_combine_un_funcall ctx ty with
          | Some func_name -> begin
              let fcf_func = IdentExpr{ id_name = func_name;
                                        id_ty = TyFunction } in
              let fc_ty = FCFunc{ fcf_func } in
              Some(FuncCallExpr{ fc_id = -1;
                                 fc_ty;
                                 fc_args = [expr] })
            end
          | None -> begin
              (* Just use value "as is". *)
              Some(expr)
            end
        end
    end
  | lhs::head -> begin
      let rhs_opt = combine ctx ty head in
      match rhs_opt with
      | Some rhs -> begin
          match gen_combine_binop ty with
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
    | TyNil      -> Transform.to_nil
    | TyBoolean  -> Transform.to_boolean
    | TyInt      -> Transform.to_int
    | TyFloat    -> Transform.to_float
    | TyString   -> Transform.to_string
    | TyFunction -> Transform.to_function
    | TyTable _  -> Transform.to_table
    | TyThread | TyUserdata -> Transform.to_nil
  in
  List.fold_left
    exprs
    ~init:[]
    ~f:(fun acc expr -> begin
          let e_ty = match get_essential_ty expr with
            | None -> TyNil
            | Some ty -> ty
          in
          acc @ [type_conv ctx e_ty expr]
        end)
  |> combine ctx ty

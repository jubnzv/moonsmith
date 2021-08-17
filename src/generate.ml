open Core_kernel

open Ast

(** Layout contains randomly generated options for the current generation. *)
type layout =
  { toplevel_stmts: int; }

(** Generation context. *)
type context =
  { ctx_stmts: stmt list;
    ctx_global_env: env;
    mutable ctx_free_idx: int; }

let get_free_idx ctx =
  ctx.ctx_free_idx <- ctx.ctx_free_idx + 1;
  ctx.ctx_free_idx

(** Generates a random layout. *)
let gen_layout c =
  { toplevel_stmts = Random.int_incl
        c.Config.c_min_toplevel_stmts
        c.Config.c_max_toplevel_stmts; }

(** Randomly generates an essential type. *)
let gen_ty () =
  match Random.int_incl 0 7 with
  | 0 -> TyNil
  | 1 -> TyBoolean
  | 2 -> TyNumber
  | 3 -> TyString
  (* TODO: | 4 -> TyUserdata *)
  | 5 -> TyFunction
  (* TODO: | 6 -> TyThread *)
  | _ -> TyTable

(** Creates a new identifier in the [env].
    The created identifier won't implicitly added to the environment, because
    in some cases we should delay this. For example, we want to avoid the
    following situation:
      local a1, a2 = "foo", a1
    a1 is used before it was defined, so this causes a problem.
*)
let mk_ident ?(add_now=false) ?(name=None) env ctx =
  let name = match name with
    | Some(n) -> n
    | None -> Printf.sprintf "v%d" @@ get_free_idx ctx
  in
  let i = IdentExpr{ id_name = name;
                     id_ty = (gen_ty ()) } in
  let _ =
    if add_now then begin env_add_binding env i;         end
    else            begin env_add_pending_binding env i; end
  in
  i

(** Generates a simple random expression. *)
let gen_simple_expr () =
  match Random.int_incl 0 4 with
  | 0 -> TrueExpr
  | 1 -> FalseExpr
  | 2 -> NilExpr
  | 3 -> NumberExpr(Random.float 100.00)
  | _ -> StringExpr(StringGen.gen_string ())

(** Generates expression with the given type.*)
let gen_simple_typed_expr ty =
  match ty with
  | TyNil     -> NilExpr
  | TyBoolean -> begin
      match Random.int_incl 0 1 with
      | 0 -> TrueExpr
      | _ -> FalseExpr
    end
  | TyNumber -> NumberExpr(Random.float 100.0)
  | TyString -> StringExpr(StringGen.gen_string ())
  (* TODO: | TyUserdata  -> "userdata" *)
  (* TODO: | TyFunction  -> "function" *)
  (* TODO: | TyThread    -> "thread"   *)
  (* TODO: Provide some initialization for the fields: *)
  (* TODO: | TyTable -> TableExpr{table_fields = []} *)
  | _ -> NilExpr

(** Generates unary expression that return result with the given type.
    If it is not possible to apply one of unary operators to this type,
    generates a simple expression. *)
let gen_typed_un_expr ty =
  match ty with
  | TyNumber -> begin
      let un_expr = gen_simple_typed_expr TyNumber in
      UnExpr { un_op = OpSub; un_expr }
    end
  | TyBoolean -> begin
      let un_expr = gen_simple_typed_expr TyNumber in
      UnExpr { un_op = OpNot; un_expr }
    end
  | _ -> gen_simple_typed_expr ty

(** Generates binary expression that return result with the given type.
    If it is not possible to create binary expression results with this type,
    generates a simple expression. *)
let gen_typed_bin_expr ty =
  match ty with
  | TyNumber -> begin
      (* TODO: It will be a good idea to extend this with # (length) operators
               applied to the existing tables. *)
      let bin_lhs = gen_simple_typed_expr TyNumber
      and bin_rhs = gen_simple_typed_expr TyNumber
      and bin_op = match Random.int_incl 0 5 with
        | 0 -> OpAdd
        | 1 -> OpSub
        | 2 -> OpMul
        | 3 -> OpDiv
        | 4 -> OpPow
        | _ -> OpMod
      in
      BinExpr { bin_lhs; bin_op; bin_rhs }
    end
  | TyBoolean -> begin
      match Random.int_incl 0 1 with
      | 0 -> begin (* compare random booleans *)
          let bin_lhs = gen_simple_typed_expr TyBoolean
          and bin_rhs = gen_simple_typed_expr TyBoolean
          and bin_op = match Random.int_incl 0 1 with
            | 0 -> OpEq
            | _ -> OpNeq
          in
          BinExpr { bin_lhs; bin_op; bin_rhs }
        end
      | _ -> begin (* compare random numbers *)
          let bin_lhs = gen_simple_typed_expr TyNumber
          and bin_rhs = gen_simple_typed_expr TyNumber
          and bin_op = match Random.int_incl 0 4 with
            | 0 -> OpLt
            | 1 -> OpLte
            | 2 -> OpGt
            | _ -> OpGte
          in
          BinExpr { bin_lhs; bin_op; bin_rhs }
        end
    end
  | TyString -> begin
      let bin_lhs = gen_simple_typed_expr TyString
      and bin_rhs = gen_simple_typed_expr TyString
      and bin_op = OpConcat
      in
      BinExpr { bin_lhs; bin_op; bin_rhs }
    end
  | _ (*TyTable*) -> gen_simple_typed_expr ty

(** Takes a random binding from the [env] or one of its parents.
    Returns None if environment and parents are empty. *)
let take_random_binding env =
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
      else Some(env_take_rand_exn env)
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

(** Generates conditional expression used in the loops and conditional
    statements. *)
let gen_cond_expr env =
  (* Generates rhs with type that is appropriate to lhs is possible. *)
  let gen_rhs lhs =
    match lhs with
    | NumberExpr _ -> gen_simple_typed_expr TyNumber
    | TrueExpr | FalseExpr -> gen_simple_typed_expr TyBoolean
    | StringExpr _ -> gen_simple_typed_expr TyString
    | IdentExpr id -> begin
        gen_simple_typed_expr id.id_ty
      end
    | _ -> gen_simple_typed_expr TyNil
  in
  let gen_logical_op lhs rhs =
    (* We cannot use some comparison operators between various types. *)
    let are_comparable lhs rhs =
      match ((get_essential_ty lhs), (get_essential_ty rhs)) with
      | (Some(ty_lhs), Some(ty_rhs)) -> types_are_comparable ty_lhs ty_rhs
      | _ -> false
    in
    let m = if not @@ are_comparable lhs rhs then 1 else 5 in
    match Random.int_incl 0 m with
    | 0 -> OpEq  (* == *)
    | 1 -> OpNeq (* ~= *)
    | 2 -> OpLt  (* < *)
    | 3 -> OpLte (* <= *)
    | 4 -> OpGt  (* > *)
    | _ -> OpGte (* >= *)
  in
  (* If we have some bindings in given environment, take one.
     Otherwise, generate a random number, string, or Boolean. *)
  let bin_lhs =
    match (take_random_binding env) with
    | Some b -> !b
    | None -> gen_simple_expr ()
  in
  let bin_rhs = gen_rhs bin_lhs in
  BinExpr { bin_lhs;
            bin_op = gen_logical_op bin_lhs bin_rhs;
            bin_rhs }

(** Generates a random init for the table expression.
    It could be both: array or hashmap table init. *)
let gen_random_table_init () =
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
  match Random.int_incl 0 1 with
  | _ -> gen_array args_num
(* TODO: | _ -> gen_hashmap args_num *)

(** Generates an initializer statement for the identifier [expr] with known type. *)
let gen_init_stmt_for_ident ?(assign_local = false) expr =
  let gen_stmt rhs =
    AssignStmt{ assign_local;
                assign_lhs = [expr];
                assign_rhs = [rhs] }
  in
  match expr with
  | IdentExpr id -> begin
      match id.id_ty with
      | TyNil     -> gen_stmt NilExpr
      | TyBoolean ->
        let rhs = match Random.int_incl 0 1 with
          | 0 -> TrueExpr
          | _ -> FalseExpr
        in
        gen_stmt rhs
      | TyNumber -> NumberExpr(Random.float 100.0) |> gen_stmt
      | TyString -> StringExpr(StringGen.gen_string ()) |> gen_stmt
      | TyFunction  -> begin
          let lambda_body = AssignStmt{ assign_local = true;
                                        assign_lhs = [IdentExpr{ id_name = "a";
                                                                 id_ty = TyNumber }];
                                        assign_rhs = [NumberExpr(42.0)]}
          in
          LambdaExpr{ lambda_args = [];
                      lambda_body }
          |> gen_stmt
        end
      (* TODO: | TyUserdata  -> "userdata" *)
      (* TODO: | TyThread    -> "thread"   *)
      | TyTable -> gen_random_table_init () |> gen_stmt
      | _ -> assert false
    end
  | _ -> assert false

(** Generates free function call statement with definition of its parameters,
    using information about types of the arguments. *)
let gen_fcall_from_fdef stmt =
  match stmt with
  | FuncDefStmt fd -> begin
      let (fcf_args, fcf_init_stmts) =
        List.fold_left
          fd.fd_args
          ~init:[]
          ~f:(fun acc expr -> begin
                match expr with
                | IdentExpr id -> begin
                    let new_name = "fc" ^ id.id_name in
                    let new_ident = IdentExpr{ id_name = new_name;
                                               id_ty = id.id_ty } in
                    let init = gen_init_stmt_for_ident new_ident in
                    acc @ [(new_ident, init)]
                  end
                | _ -> assert false
              end)
        |> Caml.List.split
      in
      let fcf_func = IdentExpr{ id_name = fd.fd_name;
                                id_ty = TyFunction }
      in
      let fc = FCFunc{ fcf_func; fcf_args }
      in
      fcf_init_stmts @ [FuncCallStmt{fc_expr = FuncCallExpr(fc)}]
    end
  | _ -> assert false

(** Randomly returns an IdentExpr with a binding from the given [env] or
    creates a new Ident and binds it to the [env].
    User should flush pending bindings in the [env] after calling this
    function. *)
let get_or_create_ident env ctx =
  match Random.int_incl 0 1 with
  | 0 -> mk_ident env ctx
  | _ -> begin
      match (take_random_binding env) with
      | Some b -> !b
      | None -> mk_ident env ctx
    end

(** Generates random conditional statement. *)
let gen_if_stmt env gen_block =
  let if_else = match Random.int_incl 0 1 with
    | 0 -> Some(gen_block env (Random.int_incl 1 5))
    | _ -> None
  in
  IfStmt{ if_cond = gen_cond_expr env;
          if_body = gen_block env (Random.int_incl 1 5) ;
          if_else }

(** Generates random loop statement.
    It randomly choose between while and repeat statements. *)
let gen_loop_stmt env gen_block =
  let loop_ty = match Random.int_incl 0 1 with
    | 0 -> While
    | _ -> Repeat
  in
  LoopStmt{ loop_cond = gen_cond_expr env;
            loop_block = gen_block env (Random.int_incl 1 5) ;
            loop_ty }

(** Generates do-end block with a random nested statements. *)
let gen_do_end_stmt env gen_block =
  DoBlockStmt{ do_block = gen_block env (Random.int_incl 1 5) ; }

(** Generates a random assignment statement.
    It could be both binding a new variable or changing the value/type of
    one that exists in the current environment [env]. *)
let gen_assign_stmt env ctx =
  (* Generates a random expression that is type compatible with lhs of the
     assignment operation. *)
  let gen_rhs_expr_from_lhs ty lhs =
    match lhs with
    | IdentExpr _ -> begin
        match Random.int_incl 0 3 with
        | 0 -> gen_typed_un_expr ty
        | _ -> gen_typed_bin_expr ty
      end
    | _ -> gen_simple_typed_expr ty
  in
  let rec do_gen acc n f =
    if List.length acc >= n then
      acc
    else
      let acc = acc @ [f ()] in
      do_gen acc (n - 1) f
  in
  let exprs_num = Random.int_incl 1 4 in
  let assign_lhs =
    do_gen [] exprs_num (fun _ -> get_or_create_ident env ctx)
  in
  let assign_rhs =
    List.fold_left
      assign_lhs
      ~init:[]
      ~f:(fun acc lhs -> begin
            let ty = match get_essential_ty lhs with
              | Some(ty) -> ty
              | None -> assert false (* always IdentExpr *)
            in
            (* Choose a random rhs. *)
            match Random.int_incl 0 5 with
            | 0 -> acc @ [ gen_simple_typed_expr ty ]
            | _ -> acc @ [ gen_rhs_expr_from_lhs ty lhs ]
          end)
  in
  let assign_local = match Random.int_incl 0 4 with
    | 0 -> true
    | _ -> false
  in
  env_flush_pending_bindings env;
  AssignStmt{ assign_local;
              assign_lhs;
              assign_rhs }

(** Generates random statement in the given [env]. *)
let gen_stmt ?(no_nested = false) env ctx gen_block =
  if no_nested then
    gen_assign_stmt env ctx
  else
    match Random.int_incl 0 6 with
    | 0             -> gen_if_stmt env gen_block
    | 1             -> gen_loop_stmt env gen_block
    | 2 | 3 | 4 | 5 -> gen_assign_stmt env ctx
    | _             -> gen_do_end_stmt env gen_block

(** Generates BlockStmt with randomly generated nested blocks. *)
let rec gen_block depth ctx env_parent max_stmts =
  let rec gen_nested_stmts acc env max_num =
    if List.length acc >= max_num then
      acc
    else
      let gen_block' = (gen_block (depth + 1) ctx)
      and nn = if phys_equal max_stmts 1 then true else false in
      let acc = acc @ [gen_stmt ~no_nested:nn env ctx gen_block'] in
      gen_nested_stmts acc env max_num
  in
  (* Prevent endless recursion. *)
  let stmts_num = if depth < 5 then Random.int_incl 1 max_stmts else 1
  and block_env = env_mk () in
  let block_env = { block_env  with env_parent = Some(ref env_parent) } in
  let block_stmts = gen_nested_stmts [] block_env stmts_num in
  env_add_child env_parent block_env;
  BlockStmt{ block_stmts; block_env }

(** Generates a random function defined on the top-level. *)
let gen_toplevel_funcdef ctx =
  let gen_arg n env =
    let name = Printf.sprintf "a%d" n in
    mk_ident env ctx ~add_now:true ~name:(Some(name))
  in
  let gen_args max_args env =
    let rec aux acc num_args =
      let n = List.length acc in
      if n >= num_args then
        acc
      else
        aux (acc @ [gen_arg n env]) num_args
    in
    let num_args = Random.int_incl 0 max_args in
    aux [] num_args
  in
  let fd_name = Printf.sprintf "func%d" @@ get_free_idx ctx
  and fd_body = gen_block 0 ctx ctx.ctx_global_env 10 in
  let fd_args = gen_args 5 (get_block_env_exn fd_body) in
  FuncDefStmt{ fd_name; fd_args; fd_body }

(** Generates a random table created using the assignment expression. *)
let gen_table ctx =
  let assign_local = if phys_equal (Random.int_incl 0 1) 0 then true else false
  and name = IdentExpr{ id_name = Printf.sprintf "t%d" @@ get_free_idx ctx;
                        id_ty = TyTable }
  and init = TableExpr(THashMap{table_fields = []})
  in
  AssignStmt{ assign_local;
              assign_lhs = [name];
              assign_rhs = [init] }

(** Generates top-level statements for the given [ctx]. *)
let gen_top_stmts ctx l =
  let gen_top_stmt ctx =
    match (Random.int_incl 0 1) with
    | 0 -> gen_toplevel_funcdef ctx
    | _ -> gen_table ctx
  in
  let rec aux ctx acc =
    if List.length acc >= l.toplevel_stmts then
      acc
    else
      aux ctx (acc @ [gen_top_stmt ctx])
  in
  { ctx with ctx_stmts = (aux ctx []) }

(** Converts all statement in the [ctx] to Lua code. *)
let ctx_to_string ctx c =
  let header = {|--------------------------------------------------------
-- This code was automatically generated by moonsmith --
-- https://github.com/jubnzv/moonsmith                --
--------------------------------------------------------
|}
  and toplevel = List.fold_left
      ctx.ctx_stmts
      ~init:[]
      ~f:(fun acc stmt -> acc @ [stmt_to_s c stmt ~cr:true])
                 |> String.concat ~sep:"\n"
  and exec_toplevel = if c.Config.c_execute_toplevel then
      List.fold_left
        ctx.ctx_stmts
        ~init:[]
        ~f:(fun acc stmt -> begin
              match stmt with
              | FuncDefStmt _ -> acc @ gen_fcall_from_fdef stmt
              | _ -> acc
            end)
      |> List.map ~f:(fun s -> stmt_to_s c s)
      |> String.concat ~sep:"\n"
    else ""
  in
  let exec_toplevel =
    if not (String.is_empty exec_toplevel) then
      {|-----------------------
-- Calling top-level --
-----------------------
|} ^ exec_toplevel
    else
      ""
  in
  String.concat [header; toplevel; exec_toplevel] ~sep:"\n"

let generate c =
  let l = gen_layout c in
  let ctx_global_env = env_mk () in
  let ctx = { ctx_stmts = []; ctx_free_idx = 0; ctx_global_env } in
  let ctx = gen_top_stmts ctx l in
  ctx_to_string ctx c

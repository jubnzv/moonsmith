open Core_kernel

open Ast

(** Layout contains randomly generated options for the current generation. *)
type layout =
  { toplevel_stmts: int; }

(** Generation context. *)
type context =
  {
    (** Randomly generated AST. *)
    ctx_stmts: stmt list;

    (** Global environment for the top-level. *)
    ctx_global_env: env;

    (** Definitions of standard functions. *)
    ctx_standard_functions: stmt list;

    (** User-defined configuration. *)
    ctx_config : Config.t;

    (** Map that associates ids of OOP tables with definitions of their methods. *)
    mutable ctx_oop_table_methods_map: (int, stmt ref list, Int.comparator_witness) Base.Map.t;

    (** Map that accociates ids of FuncDefStmts with pointer to their AST nodes. *)
    mutable ctx_func_def_map: (int, stmt ref, Int.comparator_witness) Base.Map.t;

    (** Next free index used to generate unique identifiers. *)
    mutable ctx_free_idx: int;

    (** Seed used to initialize PRG. *)
    ctx_seed: int;
  }

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
  match Random.int_incl 0 5 with
  | 0 -> TyNil
  | 1 -> TyBoolean
  | 2 -> TyNumber
  | 3 -> TyString
  | 4 -> TyFunction
  | _ -> table_mk_empty ()

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
  let (_ : unit) =
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
  match Random.bool () with
  | _ -> gen_array args_num
(* TODO: | _ -> gen_hashmap args_num *)

(** Generates expression with the given type.*)
let gen_simple_typed_expr ty =
  match ty with
  | TyNil     -> NilExpr
  | TyBoolean -> begin
      match Random.bool () with
      | true -> TrueExpr
      | _ -> FalseExpr
    end
  | TyNumber  -> NumberExpr(Random.float 100.0)
  | TyString  -> StringExpr(StringGen.gen_string ())
  | TyTable _ -> gen_random_table_init ()
  | TyThread | TyUserdata | TyFunction -> NilExpr

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
      match Random.bool () with
      | true -> begin (* compare random booleans *)
          let bin_lhs = gen_simple_typed_expr TyBoolean
          and bin_rhs = gen_simple_typed_expr TyBoolean
          and bin_op = match Random.bool () with
            | true -> OpEq
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
        let rhs = match Random.bool () with
          | true -> TrueExpr
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
      | TyTable _ -> gen_random_table_init () |> gen_stmt
      | TyUserdata | TyThread -> assert false
    end
  | _ -> assert false

(** Generates a call statement for a free function with definition of its
    parameters, using information about types of the arguments. *)
let gen_fcall_from_fdef stmt =
  match stmt with
  | FuncDefStmt fd -> begin
      let (fc_args, fcf_init_stmts) =
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
      let fc_ty = match fd.fd_receiver with
        | Some r -> FCMethod { fcm_receiver = r;
                               fcm_method = fd.fd_name }
        | None -> begin
            let fcf_func = IdentExpr{ id_name = fd.fd_name;
                                      id_ty = TyFunction }
            in
            FCFunc{ fcf_func }
          end
      in
      let fc_expr = FuncCallExpr{ fc_id = fd.fd_id;
                                  fc_ty;
                                  fc_args }
      in
      fcf_init_stmts @ [ FuncCallStmt{ fc_expr } ]
    end
  | _ -> assert false

(** Randomly returns an IdentExpr with a binding from the given [env] or
    creates a new Ident and binds it to the [env].
    User should flush pending bindings in the [env] after calling this
    function. *)
let get_or_create_ident env ctx =
  match Random.bool () with
  | true -> mk_ident env ctx
  | _ -> begin
      match (take_random_binding env) with
      | Some b -> !b
      | None -> mk_ident env ctx
    end

(** Returns ident with given type [ty] from the current environment
    [env]. If there no identifiers with such [ty], returns None.
    environments. *)
let get_typed_ident ty env =
  let rec search env =
    List.find
      env.env_bindings
      ~f:(fun expr_ref -> begin
            match get_essential_ty !expr_ref with
            | Some e_ty -> if equal_ty e_ty ty then true else false
            | None -> false
          end)
  in
  match search env with
  | Some expr_ref -> Some !expr_ref
  | None -> None

(** Generates random conditional statement. *)
let gen_if_stmt env gen_block =
  let if_else = match Random.bool () with
    | true -> Some(gen_block false env (Random.int_incl 1 5))
    | _ -> None
  in
  IfStmt{ if_cond = gen_cond_expr env;
          if_body = gen_block false env (Random.int_incl 1 5) ;
          if_else }

(** Generates random loop statement.
    It randomly choose between while and repeat statements. *)
let gen_loop_stmt env gen_block =
  let loop_ty = match Random.bool () with
    | true -> While
    | _ -> Repeat
  in
  LoopStmt{ loop_cond = gen_cond_expr env;
            loop_block = gen_block true env (Random.int_incl 1 5) ;
            loop_ty }

(** Generates a random function call statement. *)
let gen_func_call_stmt env ctx =
  let peek_func_def () =
    (* We always have at least some standard functions on the top-level, so it
       will work anyway. *)
    let n = Random.int_incl 0 ((Map.length ctx.ctx_func_def_map) - 1) in
    match Map.nth ctx.ctx_func_def_map n with
    | Some(_, stmt_ref) -> !stmt_ref
    | None -> assert false
  in
  let gen_args fd =
    match fd with
    | FuncDefStmt fd -> begin
        let len = List.length fd.fd_args in
        let rec aux acc n =
          if n >= len then acc
          else begin
            let arg = List.nth_exn fd.fd_args n in
            let ty = match get_essential_ty arg with
              | Some ty -> ty
              | None -> TyNil
            in
            let arg = match get_typed_ident ty env with
              | Some ident -> ident
              | None -> gen_simple_typed_expr ty
            in
            aux (acc @ [arg]) (n + 1)
          end
        in
        aux [] 0
      end
    | _ -> assert false
  in
  match peek_func_def () with
  | FuncDefStmt fdd -> begin
      let fcf_func = IdentExpr{ id_name = fdd.fd_name;
                                id_ty = TyFunction }
      in
      let fc_ty = match fdd.fd_receiver with
        | Some r -> FCMethod { fcm_receiver = r;
                               fcm_method = fdd.fd_name }
        | None -> FCFunc{ fcf_func }
      in
      let fd = FuncDefStmt{ fdd with fd_id = fdd.fd_id } in
      let fc_args = gen_args fd in
      let fc_expr = FuncCallExpr{ fc_id = fdd.fd_id;
                                  fc_ty;
                                  fc_args }
      in
      FuncCallStmt{ fc_expr }
    end
  | _ -> assert false

(** Generates a break statement wrapped inside conditional statement, i.e.:
    if a == 2 then
      break
    end
    We always need conditional statements, because Lua forbids standalone break
    statements.
*)
let gen_cond_break_stmt env =
  let block_env = env_mk () in
  let block_env = { block_env with env_parent = Some(ref env) } in
  let if_cond = gen_cond_expr env
  and if_body = BlockStmt { block_stmts = [BreakStmt];
                            block_is_loop = false;
                            block_env }
  in
  env_add_child env block_env;
  IfStmt { if_cond; if_body; if_else = None }

(** Generates do-end block with a random nested statements. *)
let gen_do_end_stmt env gen_block =
  DoBlockStmt{ do_block = gen_block false env (Random.int_incl 1 5) ; }

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
let gen_stmt
    ?(no_nested = false)
    ?(gen_loop = None)
    ?(in_loop = false)
    env ctx gen_block =
  if no_nested then
    gen_assign_stmt env ctx
  else if in_loop && phys_equal (Random.int_incl 0 8) 0 then
    gen_cond_break_stmt env
  else
    match gen_loop with
    | Some true -> begin
        match Random.int_incl 0 6 with
        | 0 | 1 | 2  -> gen_assign_stmt env ctx
        | 3 | 4 | 5  -> gen_func_call_stmt env ctx
        | _  -> gen_if_stmt env gen_block
      end
    | Some false -> begin
        gen_loop_stmt env gen_block
      end
    | None -> begin
        match Random.int_incl 0 12 with
        | 0                     -> gen_if_stmt env gen_block
        | 1 | 2 | 3 | 4 | 5 | 6 -> gen_assign_stmt env ctx
        | 7 | 8 | 9 | 10        -> gen_func_call_stmt env ctx
        | 11                    -> gen_loop_stmt env gen_block
        | _                     -> gen_do_end_stmt env gen_block
      end

(** Generates BlockStmt with randomly generated nested blocks. *)
let rec gen_block depth ctx in_loop  env_parent max_stmts =
  let rec gen_nested_stmts acc env max_num is_loop =
    if List.length acc >= max_num then
      acc
    else
      let gen_block' = (gen_block (depth + 1) ctx)
      and nn = if phys_equal max_stmts 1 then true else false in
      let acc = acc @ [gen_stmt
                         ~in_loop:in_loop
                         ~no_nested:nn
                         ~gen_loop:(Some(is_loop))
                         env ctx gen_block'] in
      gen_nested_stmts acc env max_num is_loop
  in
  (* Prevent endless recursion. *)
  let stmts_num = if depth < 5 then Random.int_incl 1 max_stmts else 1
  and block_env = env_mk () in
  let block_env = { block_env  with env_parent = Some(ref env_parent) }
  and block_is_loop = in_loop || match Random.int_incl 0 7 with
    | 0 -> true
    | _ -> false
  in
  let block_stmts = gen_nested_stmts [] block_env stmts_num block_is_loop in
  env_add_child env_parent block_env;
  BlockStmt{ block_stmts; block_is_loop; block_env }

(** Extends the BlockStmt [block] adding given [stmt] to the end of the block. *)
let extend_block_stmt block stmt =
  match block with
  | BlockStmt block -> begin
      let block_stmts = block.block_stmts @ [stmt] in
      BlockStmt { block with block_stmts }
    end
  | _ -> block

(** Generates a random function defined on the top-level.
    [fd_receiver] is an optional name of the object which the generated function
                  definition belongs to. If it set to Some value, the function
                  is a method.
    Returns generated FuncDefStmt and unique identifier of generated function. *)
let gen_toplevel_funcdef ?(fd_receiver = None) ctx =
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
  let gen_return_types () =
    let rec aux acc num =
      if List.length acc >= num then acc
      else aux (acc @ [gen_ty ()]) (num - 1)
    in
    aux [] (Random.int_incl 0 5)
  in
  let gen_return_exprs return_types =
    let get_ty = function
      | TyNil     -> gen_simple_typed_expr TyBoolean
      | TyBoolean -> gen_simple_typed_expr TyBoolean
      | TyNumber  -> gen_simple_typed_expr TyNumber
      | TyString  -> gen_simple_typed_expr TyString
      | TyTable _ -> gen_simple_typed_expr @@ table_mk_empty ()
      | TyThread | TyUserdata | TyFunction -> NilExpr
    in
    (* Function that returns doesn't have expr types is a routine. We don't
       need return statement at all then. *)
    if phys_equal 0 @@ List.length return_types then
      None
    else
      Some(List.fold_left return_types ~init:[] ~f:(fun acc e -> acc @ [get_ty e]))
  in
  let fd_name = match fd_receiver with
    | Some _ -> Printf.sprintf "m%d"    @@ get_free_idx ctx
    | None   -> Printf.sprintf "func%d" @@ get_free_idx ctx
  and fd_body = gen_block 0 ctx false ctx.ctx_global_env 10 in
  let fd_args = gen_args 5 (get_block_env_exn fd_body)
  and fd_ty  = gen_return_types () in
  let fd_body = match gen_return_exprs fd_ty with
    | None -> fd_body
    | Some return_exprs -> begin
        ReturnStmt{ return_exprs } |> extend_block_stmt fd_body
      end
  in
  let fd_id = mki () in
  let fd = FuncDefStmt{ fd_id;
                        fd_receiver;
                        fd_name;
                        fd_args;
                        fd_has_varags = false;
                        fd_body;
                        fd_ty }
  in
  ctx.ctx_func_def_map <- Map.set ctx.ctx_func_def_map ~key:fd_id ~data:(ref fd);
  (fd, fd_id)

(** Generates a random OOP table defined on the top-level.

    The generated table will be an object in OOP sense. It can contain some
    methods, it can be inherited using `setmetatable` function, it can have
    some special __-methods.

    For more details, see: https://www.lua.org/pil/16.html *)
let gen_toplevel_oop_table ctx =
  let generate_methods table_name num =
    let rec aux acc num =
      if List.length acc >= num then acc
      else begin
        let (fd, fd_id) = gen_toplevel_funcdef ~fd_receiver:(Some(table_name)) ctx in
        let acc = acc @ [(fd, fd_id)] in
        aux acc num
      end
    in
    aux [] num
  in
  let generate_assign table_name id method_ids =
    let id_ty = TyTable{ tyt_id = id;
                         tyt_method_ids = method_ids } in
    let assign_local = if Random.bool () then true else false
    and name = IdentExpr{ id_name = table_name;
                          id_ty }
    (* TODO: Save names and types of the fields somewhere. *)
    and init = TableExpr(THashMap{table_fields = []}) in
    AssignStmt{ assign_local;
                assign_lhs = [name];
                assign_rhs = [init] }
  in
  let table_id = get_free_idx ctx in
  let table_name = Printf.sprintf "tobj%d" table_id  in
  let methods = generate_methods table_name @@ Random.int_incl 0 4 in
  let (method_fds, method_ids) = Caml.List.split methods in
  [generate_assign table_name table_id method_ids] @ method_fds

(** Generates top-level statements for the given [ctx]. *)
let gen_top_stmts ctx l =
  let gen_toplevel_funcdef' ctx =
    let (fd, _) = gen_toplevel_funcdef ctx in [fd]
  in
  let rec aux ctx acc =
    let len = List.length acc in
    if len >= l.toplevel_stmts then
      acc
    else if ctx.ctx_config.c_generate_oop_tables &&
            (len < (l.toplevel_stmts / 2)) then
      aux ctx (acc @ gen_toplevel_oop_table ctx)
    else
      aux ctx (acc @ gen_toplevel_funcdef' ctx)
  in
  { ctx with ctx_stmts = (aux ctx []) }

(** Converts all statement in the [ctx] to Lua code. *)
let ctx_to_string ctx c =
  let header = Printf.sprintf({|--------------------------------------------------------
-- This code was automatically generated by moonsmith --
-- https://github.com/jubnzv/moonsmith                --
--
-- Seed: %20d                         --
--------------------------------------------------------
|}) ctx.ctx_seed
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

(** Generates a list of standard functions and saves them in the current
    context. *)
let gen_standard_functions ctx =
  let gen_dummy_block () =
    let block_env = env_mk () in
    BlockStmt { block_stmts = [];
                block_is_loop = false;
                block_env }
  in
  let fd_id = mki () in
  let gen_print ctx =
    let fd = FuncDefStmt { fd_id;
                           fd_receiver = None;
                           fd_name = "print";
                           fd_args = [];
                           fd_has_varags = true;
                           fd_body = gen_dummy_block ();
                           fd_ty = [TyNil] }
    in
    let ctx = { ctx with ctx_standard_functions = ctx.ctx_standard_functions @ [fd] } in
    ctx.ctx_func_def_map <- Map.set ctx.ctx_func_def_map ~key:fd_id ~data:(ref fd);
    ctx
  in
  gen_print ctx

let generate c =
  let l = gen_layout c in
  let ctx_global_env = env_mk ()
  and ctx_seed =
    match c.c_seed with Some(v) -> v | _ -> Random.bits ()
  and ctx_func_def_map = Map.empty (module Int)
  and ctx_oop_table_methods_map = Map.empty (module Int) in
  let ctx = { ctx_stmts = [];
              ctx_standard_functions = [];
              ctx_config = c;
              ctx_func_def_map;
              ctx_oop_table_methods_map;
              ctx_free_idx = 0;
              ctx_global_env;
              ctx_seed; }
  in
  let ctx = gen_standard_functions ctx in
  Random.init ctx_seed;
  let ctx = gen_top_stmts ctx l in
  ctx_to_string ctx c

open Core_kernel

(** Generate expressions used in ReturnStmt of the function. *)
let gen_return_exprs ctx env return_types =
  let open Ast in
  let aux ty =
    match env_shuffle_local_bindings env with
    | [x] -> x
    | l -> GenUtil.combine_to_typed_expr ctx ty l
           |> Option.value ~default:(GenUtil.gen_simple_expr ~ty:ty ())
  in
  let get_ty = function
    | TyNil     -> aux TyBoolean
    | TyTable   -> aux TyTable
    | TyBoolean -> aux TyBoolean
    | TyInt     -> aux TyInt
    | TyFloat   -> aux TyFloat
    | TyString  -> aux TyString
    | TyThread | TyUserdata | TyFunction | TyAny -> NilExpr
  in
  (* Function that returns doesn't have expr types is a routine. We don't
     need a return statement at all then. *)
  if List.is_empty return_types then None
  else Some(List.fold_left return_types
              ~init:[]
              ~f:(fun acc e -> acc @ [get_ty e]))

(** Randomly peeks a table name and index in [ctx_datum_stmts].
    It will become a receiver (object) for a new method.
    If returns None, this means, that it will be a free function. *)
let peek_receiver ctx =
  if not ctx.Context.ctx_config.Config.c_gen_oop_methods then None
  else match Random.bool () with
    | false -> None
    | true -> begin
        let tables_idxes = Context.get_datum_tables_i ctx in
        if List.is_empty tables_idxes then None
        else begin
          let (idx, table) = Util.choose_one_exn tables_idxes in
          match table with
          | Ast.IdentExpr id -> Some(idx, id.id_name)
          | _ -> None
        end
      end

(** Randomly generates some types that this function returns. *)
let gen_return_types () =
  let rec aux acc num =
    if List.length acc >= num then acc
    else aux (acc @ [GenUtil.gen_ty ()]) (num - 1)
  in
  aux [] (Random.int_incl 1 2)

(** Generates function arguments and places them in the function's
    environment. *)
let gen_args max_args env =
  let gen_arg n env =
    let name = Printf.sprintf "a%d" n in
    GenUtil.gen_ident ~add_now:true ~name:(Some(name)) env
  in
  let rec aux acc num_args =
    let n = List.length acc in
    if n >= num_args then
      acc
    else
      aux (acc @ [gen_arg n env]) num_args
  in
  let num_args = Random.int_incl 0 max_args in
  aux [] num_args

(** Adds a [method_id] to the methods of the datum table that has index [idx]
    in the [ctx.ctx_datum_stmts].  *)
let add_method_to_datum_table ctx method_id idx =
  let open Context in
  let open Ast in
  match List.nth_exn ctx.ctx_datum_stmts idx with
  | AssignStmt s -> begin
      match List.nth_exn s.assign_lhs 0 with
      | IdentExpr id -> begin
          match id.id_ty with
          | TyTable -> begin
              let method_ids = Map.find_exn ctx.ctx_oop_table_methods_map id.id_id in
              let method_ids = method_ids @ [method_id] in
              ctx.Context.ctx_oop_table_methods_map <- Map.set
                  ~key:idx
                  ~data:(method_ids)
                  ctx.Context.ctx_oop_table_methods_map;
            end
          | _ -> assert false
        end
      | _ -> assert false
    end
  | _ -> assert false

(** Generates a random statement that defines some local variables in the
    given [env] *)
let gen_local_def_stmt ctx env =
  let stmts_in_assign = Random.int_incl 1 3 in
  let rec aux acc =
    if (List.length acc) > stmts_in_assign then
      acc
    else begin
      let lhs = GenUtil.gen_ident env
          ~name:(Some(Printf.sprintf "v%d" @@ Context.get_free_idx ()))
      in
      let rhs = GenUtil.gen_rhs_to_assign_ident ctx env lhs in
      aux (acc @ [(lhs, rhs)])
    end
  in
  let (assign_lhs, assign_rhs) = aux [] |> Caml.List.split in
  Ast.env_flush_pending_bindings env;
  Ast.AssignStmt{ assign_local = Random.bool ();
                  assign_lhs;
                  assign_rhs }

(** Generates a random statements that changes some data in the in the given
    [env] or changes some global datum in [ctx.ctx_datum_stmts]. *)
let gen_mutation_stmts ctx env =
  match Random.int_incl 0 4 with
  | 0 | 1 -> [GenerateLinear.generate ctx env]
  | 2     -> [GenerateCond.generate ctx env]
  | 3     -> GenerateLoop.generate ctx env
  | _     -> [GenerateFor.generate ctx env]

(** Generates a block statement that will be a body of the generated
    function. *)
let gen_body ctx block =
  let open Ast in
  let rec gen_nested_stmt gen acc env num =
    if List.length acc >= num then
      acc
    else
      let stmt = gen ctx env in
      let acc = acc @ [stmt] in
      gen_nested_stmt gen acc env num
  in
  let rec gen_nested_stmts gen acc env num =
    if List.length acc >= num then
      acc
    else
      let stmts = gen ctx env in
      let acc = acc @ stmts in
      gen_nested_stmts gen acc env num
  in
  match block with
  | BlockStmt block -> begin
      let num_local_defs = Random.int_incl 2 3 in
      let local_def_stmts =
        gen_nested_stmt gen_local_def_stmt
          []
          block.block_env
          num_local_defs
      in
      let num_mutations = Random.int_incl 4 6 in
      let mutation_stmts =
        gen_nested_stmts gen_mutation_stmts
          []
          block.block_env
          num_mutations
      in
      BlockStmt{ block with block_stmts = local_def_stmts @ mutation_stmts }
    end
  | _ -> assert false

(** Fills body of the given function. *)
let fill_funcdef ctx fd =
  let open Ast in
  match fd with
  | FuncDefStmt fd -> begin
      let fd_body = gen_body ctx fd.fd_body in
      let fd_body =
        match gen_return_exprs ctx (get_block_env_exn fd.fd_body) fd.fd_ty with
        | Some return_exprs -> begin
            [ReturnStmt{ return_exprs }] |> GenUtil.extend_block_stmt fd_body
          end
        | None -> fd_body
      in
      FuncDefStmt{ fd with fd_body = fd_body}
    end
  | _ -> assert false

(** Generates a random function defined on the top-level.
    Generated function contains an empty body. *)
let gen_funcdef ctx =
  let open Ast in
  let fd_id = mki () in
  let (fd_name, fd_receiver) = match peek_receiver ctx with
    | None   -> (Printf.sprintf "func%d" @@ Context.get_free_idx (), None)
    | Some (datums_idx, receiver_name) -> begin
        let name = Printf.sprintf "m%d" @@ Context.get_free_idx () in
        add_method_to_datum_table ctx fd_id datums_idx;
        (name, Some(receiver_name))
      end
  and fd_body = GenUtil.gen_empty_block ctx.ctx_global_env in
  let fd_args = gen_args 5 (get_block_env_exn fd_body)
  and fd_has_varags = if phys_equal 0 @@ Random.int_incl 0 10 then true else false
  and fd_ty  = gen_return_types () in
  let fd = FuncDefStmt{ fd_id;
                        fd_receiver;
                        fd_name;
                        fd_args;
                        fd_has_varags;
                        fd_body;
                        fd_ty }
  in
  ctx.Context.ctx_func_def_map <- Map.set
      ~key:fd_id
      ~data:(ref fd)
      ctx.Context.ctx_func_def_map;
  fd

let generate (ctx : Context.t) =
  let open Context in
  let open Config in
  let rec aux acc num =
    if List.length acc >= num then acc
    else aux (acc @ [gen_funcdef ctx]) num
  in
  let ctx_funcdef_stmts = aux [] @@ Random.int_incl
      ctx.ctx_config.c_min_toplevel_funcdefs
      ctx.ctx_config.c_max_toplevel_funcdefs
  in
  let ctx_funcdef_stmts = List.fold_left
      ctx_funcdef_stmts
      ~init:[]
      ~f:(fun acc fd -> acc @ [fill_funcdef ctx fd])
  in
  { ctx with ctx_funcdef_stmts = ctx_funcdef_stmts }

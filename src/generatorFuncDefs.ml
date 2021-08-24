open Core_kernel

exception ConfigError of string

(** Generate expressions used in ReturnStmt of the function. *)
let gen_return_exprs return_types =
  let open Ast in
  let get_ty = function
    (* TODO: Take variables from the local environment. *)
    | TyNil     -> GenUtil.gen_simple_typed_expr TyBoolean
    | TyBoolean -> GenUtil.gen_simple_typed_expr TyBoolean
    | TyInt     -> GenUtil.gen_simple_typed_expr TyInt
    | TyFloat   -> GenUtil.gen_simple_typed_expr TyFloat
    | TyString  -> GenUtil.gen_simple_typed_expr TyString
    | TyTable _ -> GenUtil.gen_simple_typed_expr @@ Ast.table_mk_empty ()
    | TyThread | TyUserdata | TyFunction -> NilExpr
  in
  (* Function that returns doesn't have expr types is a routine. We don't
     need a return statement at all then. *)
  if phys_equal 0 @@ List.length return_types then
    None
  else
    Some(List.fold_left return_types
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
        if phys_equal 0 @@ List.length tables_idxes then None
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
  let updated_table = match List.nth_exn ctx.ctx_datum_stmts idx with
    | AssignStmt s -> begin
        if (List.length s.assign_lhs) < 1 then assert false
        else begin
          let lhs = List.nth_exn s.assign_lhs 0 in
          match lhs with
          | IdentExpr id -> begin
              match id.id_ty with
              | TyTable t -> begin
                  let new_tyt_method_ids = t.tyt_method_ids @ [method_id] in
                  let new_id_ty = TyTable{ t with tyt_method_ids = new_tyt_method_ids } in
                  let new_lhs = IdentExpr{ id with id_ty = new_id_ty } in
                  AssignStmt{ s with assign_lhs = [new_lhs] }
                end
              | _ -> assert false
            end
          | _ -> assert false
        end
      end
    | _ -> assert false
  in
  let new_datums = Util.replace ctx.ctx_datum_stmts idx updated_table in
  ctx.ctx_datum_stmts <- new_datums

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

(** Generates a random mutation statement that never changes control flow
    (i.e. no loops, no conditions). *)
let gen_linear_mutation_stmt ctx env =
  (* Peek lhs for mutation. Prefer local variables to global datums. *)
  let peek_lhs () =
    match Random.int_incl 0 3 with
    (* TODO: Sometimes we could unintentionally assign a new value
             to datum. This will override previous method definitions.
             So this case needs some special logic. *)
    (* | 0 -> Context.peek_random_datum_exn ctx *)
    | _ -> !(Ast.env_peek_random_exn env)
  in
  (* Randomly peek some idents for the rhs.
     Or just generate some simple expressions. *)
  let peek_rhs_parts () =
    let rhs_num = Random.int_incl 2 8 in
    let peek () =
      match Random.int_incl 0 8 with
      (* TODO: Need figure out with types of the lhs. *)
      (* | 0         -> Context.peek_random_datum_exn ctx *)
      | 1 | 2 | 3 -> !(Ast.env_peek_random_exn env)
      | _         -> GenUtil.gen_simple_expr ()
    in
    let rec gen acc =
      if (List.length acc) > rhs_num then acc
      else begin
        gen (acc @ [peek ()])
      end
    in
    gen []
  in
  let lhs = peek_lhs () in
  let ty = match lhs with
    | IdentExpr id -> id.id_ty
    | _ -> assert false
  in
  let rhs_opt = peek_rhs_parts () |> GenUtil.combine_to_typed_expr ctx ty in
  match rhs_opt with
  | Some rhs -> begin
      Ast.AssignStmt{ assign_local = false;
                      assign_lhs = [lhs];
                      assign_rhs = [rhs]; }
    end
  | None -> raise @@ ConfigError "Can't combine expressions using this config. Try to enable more functions."

(** Generates a random statement that changes some data in the in the given
    [env] or changes some global datum in [ctx.ctx_datum_stmts]. *)
let gen_mutation_stmt ctx env =
  gen_linear_mutation_stmt ctx env

(** Generates a block statement that will be a body of the generated
    function. *)
let gen_body ctx block =
  let open Ast in
  let rec gen_nested_stmts gen acc env num =
    if List.length acc >= num then
      acc
    else
      let stmt = gen ctx env in
      let acc = acc @ [stmt] in
      gen_nested_stmts gen acc env num
  in
  match block with
  | BlockStmt block -> begin
      let num_local_defs = Random.int_incl 2 3 in
      let local_def_stmts =
        gen_nested_stmts gen_local_def_stmt
          []
          block.block_env
          num_local_defs
      in
      let num_mutations = Random.int_incl 4 6 in
      let mutation_stmts =
        gen_nested_stmts gen_mutation_stmt
          []
          block.block_env
          num_mutations
      in
      BlockStmt{ block with block_stmts = local_def_stmts @ mutation_stmts }
    end
  | _ -> assert false

(** Generates an empty block statement that will be a body of the generated
    function. *)
let gen_empty_body parent_env =
  let block_env = Ast.env_mk () in
  let block_env = { block_env with env_parent = Some(ref parent_env) } in
  let block_stmts = [] in
  Ast.env_add_child parent_env block_env;
  Ast.BlockStmt{ block_stmts;
                 block_is_loop = false;
                 block_env }

(** Fills body of the given function. *)
let fill_funcdef ctx fd =
  let open Ast in
  match fd with
  | FuncDefStmt fd -> begin
      let fd_body = gen_body ctx fd.fd_body in
      (* let fd_body = match gen_return_exprs fd_ty with *)
      (*   | _ -> fd_body                                *)
      (* | Some return_exprs -> begin                                *)
      (*     ReturnStmt{ return_exprs } |> extend_block_stmt fd_body *)
      (*   end                                                       *)
      (* in *)
      (* () *)
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
  and fd_body = gen_empty_body ctx.ctx_global_env in
  let fd_args = gen_args 5 (get_block_env_exn fd_body)
  and fd_ty  = gen_return_types () in
  let fd = FuncDefStmt{ fd_id;
                        fd_receiver;
                        fd_name;
                        fd_args;
                        fd_has_varags = false;
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

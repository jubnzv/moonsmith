open Core_kernel

let gen_loop_block env =
  GenUtil.gen_empty_block env

let gen_number_for ctx env =
  let open Ast in
  let (nfor_init, nfor_limit, nfor_step) =
    if Random.bool () then
      IntExpr(Random.int_incl 0 5),
      IntExpr(Random.int_incl 10 20),
      IntExpr(Random.int_incl 1 3)
    else
      IntExpr(Random.int_incl 10 20),
      IntExpr(Random.int_incl 0 5),
      IntExpr(Random.int_incl 1 3)
  in
  let block = gen_loop_block env in
  let block =
    (Random.int_incl 1 3)
    |> GenerateLinear.generate_stmts ctx env
    |> GenUtil.extend_block_stmt block
  in
  NumForStmt { nfor_name = "i";
               nfor_init;
               nfor_limit;
               nfor_step;
               nfor_body = block; }

let can_generate_generic_loop ctx =
  if ctx.Context.ctx_config.Config.c_use_pairs ||
     ctx.Context.ctx_config.Config.c_use_ipairs then
    true else false

(** Generates function call expression for iterator for a generic for loop. *)
let gen_iterator_exn ctx arg =
  let open Ast in
  let names = [] in
  let names =
    if ctx.Context.ctx_config.Config.c_use_pairs
    then names @ ["pairs"] else names
  in
  let names =
    if ctx.Context.ctx_config.Config.c_use_ipairs
    then names @ ["ipairs"] else names
  in
  let name = Util.choose_one_exn names in
  FuncCallExpr{ fc_id = -1;
                fc_ty = FCFunc{ fcf_func = IdentExpr{ id_name = name;
                                                      id_ty = TyFunction } };
                fc_args = [arg] }

(** Generates variables used as lhs in the genertic for loop.
    It also includes them in the environment of [BlockStmt] which is body of
    the loop. *)
let gen_for_names for_body =
  let open Ast in
  let gen_var name ty env =
    let var = IdentExpr{ id_name = name;
                         id_ty = ty }
    in
    env_add_binding env var;
    var
  in
  let gen_wildcard () =
    IdentExpr{ id_name = "_";
               id_ty = TyNil }
  in
  match for_body with
  | BlockStmt block -> begin
      let env = block.block_env in
      match Random.int_incl 0 2 with
      | 0 -> [(gen_var "i" TyInt env); gen_wildcard ()]
      | 1 -> [gen_wildcard (); (gen_var "v" TyAny env)]
      | _ -> [(gen_var "i" TyInt env); (gen_var "v" TyAny env)]
    end
  | _ -> assert false

let gen_generic_for ctx env =
  let open Ast in
  let iterator_arg =
    env_find_binding_with_ty env @@ table_mk_empty ()
    |> Option.value ~default:(GenUtil.gen_array_table_init ())
  in
  let iterator_funccall = gen_iterator_exn ctx iterator_arg in
  let block = gen_loop_block env in
  let for_names = gen_for_names block in
  let block =
    (Random.int_incl 1 3)
    |> GenerateLinear.generate_stmts ctx env
    |> GenUtil.extend_block_stmt block
  in
  ForStmt{ for_names;
           for_exprs = [iterator_funccall];
           for_body = block }

let generate ctx env =
  let gen =
    if not @@ can_generate_generic_loop ctx then gen_number_for
    else if Random.bool () then gen_number_for
    else gen_generic_for
  in
  gen ctx env

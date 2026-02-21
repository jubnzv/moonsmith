open Core

let generate ctx env =
  let open Ast in
  let gen_cond () =
    let cond_lhs =
      match Random.int_incl 0 3 with
      | 0 -> Context.peek_random_datum_exn ctx
      | _ -> !(env_peek_random_exn env)
    in
    let cond_rhs, cond_op =
      match get_essential_ty cond_lhs with
      | Some ty ->
        (GenUtil.gen_simple_expr ~ty:ty (), GenUtil.gen_compare_binop ty)
      | None ->
        (NilExpr, OpEq)
    in
    BinExpr{ bin_lhs = cond_lhs;
             bin_op = cond_op;
             bin_rhs = cond_rhs }
  in
  let gen_body () =
    let block = GenUtil.gen_empty_block env in
    match block with
    | BlockStmt block ->
      (* NOTE: We are working in the parent environment. No new variables
         inside "if" blocks. *)
      let body =
        (Random.int_incl 1 3) |> GenerateLinear.generate_stmts ctx env
      in BlockStmt{ block with block_stmts = body }
    | _ -> failwith "Impossible: Body of the function is not a BlockStmt"
  in
  let gen_else () =
    if Random.bool () then None
    else Some(gen_body ())
  in
  IfStmt{ if_cond = gen_cond ();
          if_body = gen_body ();
          if_else = gen_else () }


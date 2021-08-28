open Core_kernel

let generate ctx env =
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
    let rhs_num = Random.int_incl 2 3 in
    let peek () =
      match Random.int_incl 0 8 with
      (* TODO: Need to figure out with types of the lhs. *)
      (* | 0         -> Context.peek_random_datum_exn ctx *)
      | 1 | 2 | 3 -> !(Ast.env_peek_random_exn env)
      | _         -> GenUtil.gen_simple_expr ()
    in
    let rec gen acc =
      if (List.length acc) >= rhs_num then acc
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
  | None ->
    Errors.ConfigError "Can't combine expressions using this config. Try to enable more functions." |> raise

let generate_stmts ctx env num =
  let rec aux acc =
    if num <= List.length acc then acc
    else aux (acc @ [generate ctx env])
  in aux []

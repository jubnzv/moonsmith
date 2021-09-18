open Core_kernel

(** Generates a call statement for a free function with definition of its
    parameters, using information about types of the arguments. *)
let gen_fcall_from_fdef stmt =
  let open Ast in
  match stmt with
  | FuncDefStmt fd -> begin
      let gen_varags () =
        let rec aux acc num =
          let len = List.length acc in
          if num > len then
            let idx = Context.get_free_idx ()
            and id_ty = Util.choose_one_exn [TyInt; TyFloat; TyBoolean; TyString]
            in
            let varg = IdentExpr{ id_id = idx;
                                  id_name = Printf.sprintf "vararg%d" len;
                                  id_ty }
            in
            aux (acc @ [varg]) num
          else acc
        in
        aux [] (Random.int_incl 0 3)
      in
      (* Generate arguments for function call with their initialization
         statements. *)
      let (fc_args, fcf_init_stmts) =
        (* If function supports varargs, add some. *)
        let varags = if fd.fd_has_varags then gen_varags () else [] in
        List.fold_left
          (fd.fd_args @ varags)
          ~init:[]
          ~f:(fun acc expr -> begin
                match expr with
                | IdentExpr id -> begin
                    let idx = (Context.get_free_idx ()) in
                    let new_name = Printf.sprintf "%s_%s" fd.fd_name id.id_name in
                    let new_ident = IdentExpr{ id_id = idx;
                                               id_name = new_name;
                                               id_ty = id.id_ty } in
                    let init = GenUtil.gen_init_stmt_for_ident ~assign_local:true new_ident in
                    acc @ [(new_ident, init)]
                  end
                | _ -> failwith "gen_varags works incorrectly"
              end)
        |> Caml.List.split
      in
      (* Set type of the function call. *)
      let fc_ty = match fd.fd_receiver with
        | Some r -> FCMethod { fcm_receiver = r;
                               fcm_method = fd.fd_name }
        | None -> begin
            let fcf_func = IdentExpr{ id_id = Context.get_free_idx ();
                                      id_name = fd.fd_name;
                                      id_ty = TyFunction }
            in
            FCFunc{ fcf_func }
          end
      in
      (* Assign result to variables. *)
      let results = List.fold_left
          fd.fd_ty
          ~init:[]
          ~f:(fun acc ty -> begin
                let idx = Context.get_free_idx ()
                and len = List.length acc in
                acc @ [IdentExpr{ id_id = idx;
                                  id_name = Printf.sprintf "r_%s_%d" fd.fd_name len;
                                  id_ty = ty }]
              end)
      in
      (* Generate call expression. *)
      fcf_init_stmts @ [AssignStmt{ assign_local = true;
                                    assign_lhs = results;
                                    assign_rhs = [FuncCallExpr{ fc_id = fd.fd_id;
                                                                fc_ty;
                                                                fc_args }]}]
    end
  | _ -> failwith "Expected FuncDefStmt"

let generate (ctx : Context.t) =
  let open Context in
  let call_stmts = List.fold_left
      ctx.ctx_funcdef_stmts
      ~init:[]
      ~f:(fun acc stmt -> begin
            match stmt with
            | Ast.FuncDefStmt _ -> acc @ gen_fcall_from_fdef stmt
            | _ -> acc
          end)
  in { ctx with ctx_call_stmts = call_stmts }

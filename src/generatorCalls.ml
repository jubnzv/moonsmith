open Core_kernel

(** Generates a call statement for a free function with definition of its
    parameters, using information about types of the arguments. *)
let gen_fcall_from_fdef stmt =
  let open Ast in
  match stmt with
  | FuncDefStmt fd -> begin
      (* Generate arguments for function call with their initialization
         statements. *)
      let (fc_args, fcf_init_stmts) =
        List.fold_left
          fd.fd_args
          ~init:[]
          ~f:(fun acc expr -> begin
                match expr with
                | IdentExpr id -> begin
                    let new_name = Printf.sprintf "arg%d_%s"
                        (Context.get_free_idx ())
                        id.id_name
                    in
                    let new_ident = IdentExpr{ id_name = new_name;
                                               id_ty = id.id_ty } in
                    let init = GenUtil.gen_init_stmt_for_ident new_ident in
                    acc @ [(new_ident, init)]
                  end
                | _ -> assert false
              end)
        |> Caml.List.split
      in
      (* Set type of the function call. *)
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
      (* Generate final expression. *)
      let fc_expr = FuncCallExpr{ fc_id = fd.fd_id;
                                  fc_ty;
                                  fc_args }
      in
      fcf_init_stmts @ [ FuncCallStmt{ fc_expr } ]
    end
  | _ -> assert false

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

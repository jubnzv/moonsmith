let mk_funccall name args =
  let open Ast in
  let fcf_func = IdentExpr{ id_id = -1;
                            id_name = name;
                            id_ty = TyFunction }
  in
  let fc_ty = FCFunc{ fcf_func } in
  FuncCallExpr{ fc_id = -1;
                fc_ty;
                fc_args = args }

let mk_ident ?(ty = Ast.TyAny) name =
  Ast.IdentExpr{ id_id = -1;
                 id_name = name;
                 id_ty = ty}

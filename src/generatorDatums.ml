open Core_kernel

(** Generates a datum table that may be an object in OOP sense. It may
    have methods and could be inherited. The methods can be randomly generated
    later by the [GeneratorFuncDefs] generator.
    For more details, see: https://www.lua.org/pil/16.html *)
let gen_datum_table tyt_id  =
  Ast.TyTable{ tyt_id;
               tyt_method_ids = [] }

let gen_datum_ident () =
  let idx = Context.get_free_idx () in
  let id_name = Printf.sprintf "datum%d" idx
  and id_ty =
    if Random.bool () then GenUtil.gen_simple_ty ()
    else gen_datum_table idx
  in
  Ast.IdentExpr{ id_name;
                 id_ty }

(** Generates a random initializer for the given [lhs]. *)
let gen_datum_value lhs =
  let open Ast in
  match lhs with
  | IdentExpr id -> begin
      match id.id_ty with
      | TyBoolean -> if Random.bool() then TrueExpr else FalseExpr
      | TyInt -> IntExpr(Random.int_incl (-100) 100)
      | TyFloat -> FloatExpr(Random.float 100.0)
      | TyString -> let v = StringGen.gen_string () in StringExpr(v)
      | TyTable _ -> begin
          (* We are generating just empty tables here. They could be extended later. *)
          let table_ty = THashMap{ table_fields = [] } in
          TableExpr(table_ty)
        end
      | TyFunction | TyThread | TyUserdata | TyNil -> assert false
    end
  | _ -> assert false

(** Generates a random datum block in the given [ctx]. *)
let gen_random_datum ctx =
  let lhs = gen_datum_ident ctx in
  let rhs = gen_datum_value lhs in
  Ast.AssignStmt{ assign_local = Random.bool ();
                  assign_lhs = [lhs];
                  assign_rhs = [rhs]; }

let generate (ctx : Context.t) =
  let open Context in
  let open Config in
  let rec aux acc num =
    if List.length acc >= num then acc
    else aux (acc @ [gen_random_datum ()]) num
  in
  let ctx_datum_stmts = aux [] @@ Random.int_incl
      ctx.ctx_config.c_min_toplevel_datums
      ctx.ctx_config.c_max_toplevel_datums
  in
  { ctx with ctx_datum_stmts = ctx_datum_stmts }

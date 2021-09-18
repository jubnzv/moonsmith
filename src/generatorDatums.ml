open Core_kernel

(** Generates a datum table that may be an object in OOP sense. It may
    have methods and could be inherited. The methods can be randomly generated
    later by the [GeneratorFuncDefs] generator.
    For more details, see: https://www.lua.org/pil/16.html *)
let gen_datum_table ctx idx id_name =
  let id = Ast.IdentExpr{ id_id = idx;
                          id_name;
                          id_ty = TyTable }
  in
  ctx.Context.ctx_table_fields_map <- Map.set
      ~key:idx
      ~data:([])
      ctx.Context.ctx_table_fields_map;
  id

let gen_datum_ident ctx =
  let idx = Context.get_free_idx () in
  let id_name = Printf.sprintf "datum%d" idx in
  if Random.bool () then
    Ast.IdentExpr{ id_id = idx;
                   id_name;
                   id_ty = GenUtil.gen_simple_ty () }
  else gen_datum_table ctx idx id_name

(** Generates a random initializer for the given [lhs]. *)
let gen_datum_value ctx lhs =
  let open Ast in
  match lhs with
  | IdentExpr lhs_id -> begin
      match lhs_id.id_ty with
      | TyBoolean -> if Random.bool() then TrueExpr else FalseExpr
      | TyInt -> IntExpr(Random.int_incl (-100) 100)
      | TyFloat -> FloatExpr(Random.float 100.0)
      | TyString -> let v = StringGen.gen_string () in StringExpr(v)
      | TyIntString -> let v = StringGen.gen_int_string () in StringExpr(v)
      | TyTable   -> begin
          let table_expr = GenUtil.gen_hash_table_init () in
          let added_ids = get_table_key_ids table_expr
          and prev_data =
            Map.find_exn ctx.Context.ctx_table_fields_map lhs_id.id_id
          in
          ctx.Context.ctx_table_fields_map <- Map.set
              ~key:lhs_id.id_id
              ~data:(prev_data @ added_ids)
              ctx.Context.ctx_table_fields_map;
          table_expr
        end
      | TyFunction | TyThread | TyUserdata | TyNil | TyAny -> failwith "Unsupported type for gen_datum_value"
    end
  | _ -> failwith "Expected IdentExpr"

(** Generates a random datum block in the given [ctx]. *)
let gen_random_datum ctx =
  let lhs = gen_datum_ident ctx in
  let rhs = gen_datum_value ctx lhs in
  Context.add_to_global_env ctx lhs;
  Ast.AssignStmt{ assign_local = Random.bool ();
                  assign_lhs = [lhs];
                  assign_rhs = [rhs]; }

let generate (ctx : Context.t) =
  let open Context in
  let open Config in
  let rec aux acc num =
    if List.length acc >= num then acc
    else aux (acc @ [gen_random_datum ctx]) num
  in
  let ctx_datum_stmts = aux [] @@ Random.int_incl
      ctx.ctx_config.c_min_toplevel_datums
      ctx.ctx_config.c_max_toplevel_datums
  in
  { ctx with ctx_datum_stmts = ctx_datum_stmts }

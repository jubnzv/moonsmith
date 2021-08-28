open Core_kernel

(** Essential types present in Lua. *)
type ty =
  | TyNil
  | TyBoolean
  | TyInt
  | TyFloat
  | TyString
  | TyUserdata
  | TyFunction
  | TyThread
  | TyTable
  | TyAny
[@@deriving eq, show]

(** See: https://www.lua.org/manual/5.3/manual.html#3.4.1 *)
type operator =
  | OpAdd     (* + *)
  | OpSub     (* - *)
  | OpMul     (* * *)
  | OpDiv     (* / *)
  | OpFloor   (* // *)
  | OpPow     (* ^ *)
  | OpMod     (* % *)
  | OpConcat  (* .. *)
  | OpLt      (* < *)
  | OpLte     (* <= *)
  | OpGt      (* > *)
  | OpGte     (* >= *)
  | OpEq      (* == *)
  | OpNeq     (* ~= *)
  | OpBAnd    (* & *)
  | OpBOr     (* ~ *)
  | OpBRs     (* >> *)
  | OpBLs     (* << *)
  | OpBNot    (* ~ *)
  | OpAnd     (* and *)
  | OpOr      (* or *)
  | OpNot     (* not *)
  | OpLen     (* # *)

type env = { mutable env_bindings: expr ref list;
             mutable env_pending_bindings: expr ref list;
             mutable env_parent: env ref option;
             mutable env_children: env ref list; }

and expr =
  | TrueExpr
  | FalseExpr
  | NilExpr
  | IntExpr of int
  | FloatExpr of float
  | StringExpr of string
  | IdentExpr of { id_id: int;
                   id_name: string;
                   id_ty: ty }
  | AttrGetExpr of { ag_obj: expr;
                     ag_key: expr }
  | TableExpr of table_ty
  | LambdaExpr of { lambda_args: expr list;
                    lambda_body: stmt; }
  | FuncCallExpr of { fc_id: int;
                      fc_ty: func_call;
                      fc_args: expr list }
  | UnExpr of { un_op: operator;
                un_expr: expr }
  | BinExpr of { bin_lhs: expr;
                 bin_op: operator;
                 bin_rhs: expr }
and table_ty =
  | TArray of { table_elements: expr list }
  | THashMap of { table_fields: table_field list }
and table_field =
  { tf_key: expr;
    tf_value: expr }
and func_call =
  | FCMethod of { fcm_receiver: string;
                  fcm_method: string; }
  | FCFunc of { fcf_func: expr }

and stmt =
  | AssignStmt of { assign_local: bool;
                    assign_lhs: expr list;
                    assign_rhs: expr list }
  | FuncCallStmt of { fc_expr: expr }
  | DoBlockStmt of { do_block: stmt }
  | LoopStmt of { loop_cond: expr;
                  loop_block: stmt;
                  loop_ty: loop_type }
  | IfStmt of { if_cond: expr;
                if_body: stmt;
                if_else: stmt option }
  | NumForStmt of { nfor_name: string;
                    nfor_init: expr;
                    nfor_limit: expr;
                    nfor_step: expr;
                    nfor_body: stmt }
  | ForStmt of { for_names: expr list;
                 for_exprs: expr list;
                 for_body: stmt }
  | FuncDefStmt of { fd_id: int;
                     (** Receiver (object) which this function belongs to.
                         If not None, the function is a method. *)
                     fd_receiver: string option;
                     fd_name: string;
                     fd_args: expr list;
                     fd_has_varags: bool;
                     fd_body: stmt;
                     fd_ty: ty list }
  | ReturnStmt of { return_exprs: expr list }
  | BreakStmt
  | BlockStmt of { block_stmts: stmt list;
                   block_is_loop: bool;
                   block_env: env }
and loop_type =
  | While
  | Repeat

let mki =
  let r = ref 0 in
  fun () -> incr r;
    !r

let ty_to_s = function
  | TyNil      -> "nil"
  | TyBoolean  -> "boolean"
  | TyInt      -> "int"
  | TyFloat    -> "float"
  | TyString   -> "string"
  | TyUserdata -> "userdata"
  | TyFunction -> "function"
  | TyThread   -> "thread"
  | TyTable    -> "table"
  | TyAny      -> "any"

let get_essential_ty expr =
  match expr with
  | TrueExpr | FalseExpr -> Some(TyBoolean)
  | NilExpr              -> Some(TyNil)
  | IntExpr _            -> Some(TyInt)
  | FloatExpr _          -> Some(TyFloat)
  | StringExpr _         -> Some(TyString)
  | IdentExpr id         -> Some(id.id_ty)
  | TableExpr _          -> Some(TyTable)
  | LambdaExpr _         -> Some(TyFunction)
  | _ -> None

let types_are_comparable ty_lhs ty_rhs =
  match (ty_lhs, ty_rhs) with
  | (TyNil, TyNil)           -> false
  | (TyBoolean, TyBoolean)   -> false
  | (TyInt, TyInt)           -> true
  | (TyInt, TyFloat)         -> true
  | (TyFloat, TyInt)         -> true
  | (TyFloat, TyFloat)       -> true
  | (TyString, TyString)     -> true
  | (TyUserdata, TyUserdata) -> false
  | (TyFunction, TyFunction) -> false
  | (TyThread, TyThread)     -> false
  | (TyTable, TyTable)       -> false
  | _ -> false

let get_block_env_exn = function
  | BlockStmt s -> s.block_env
  | _ -> Errors.InternalError "Expected BlockStmt" |> raise

let op_to_s = function
  | OpAdd     ->  "+"
  | OpSub     ->  "-"
  | OpMul     ->  "*"
  | OpDiv     ->  "/"
  | OpFloor   ->  "//"
  | OpPow     ->  "^"
  | OpMod     ->  "%"
  | OpConcat  ->  ".."
  | OpLt      ->  "<"
  | OpLte     ->  "<="
  | OpGt      ->  ">"
  | OpGte     ->  ">="
  | OpEq      ->  "=="
  | OpNeq     ->  "~="
  | OpNot     ->  "not"
  | OpBAnd    ->  "&"
  | OpBOr     ->  "~"
  | OpBRs     ->  ">>"
  | OpBLs     ->  "<<"
  | OpBNot    ->  "~"
  | OpAnd     ->  "and"
  | OpOr      ->  "or"
  | OpLen     ->  "#"

let env_mk () =
  { env_bindings = [];
    env_pending_bindings = [];
    env_parent = None;
    env_children = [] }

let env_empty env =
  List.is_empty env.env_bindings

let env_has_parent env =
  Option.is_some env.env_parent

let env_get_parent_exn env =
  match env.env_parent with
  | Some parent -> !parent
  | None -> Errors.InternalError "Env has no parent!" |> raise

(* TODO: Add nested level *)
let env_peek_random_exn env =
  Random.int_incl 0 @@ (List.length env.env_bindings) - 1
  |> List.nth_exn env.env_bindings

let env_shuffle_local_bindings env =
  let open Util in
  if env_empty env then []
  else begin
    let bindings_len = List.length env.env_bindings in
    let get_shuffled_idxes () =
      let nums = ( -- ) 0 @@ bindings_len in
      let ns = List.map nums ~f:(fun num -> (Random.bits (), num)) in
      let sorted = Caml.List.sort Caml.compare ns in
      List.map sorted ~f:snd
    in
    let rec aux acc n idxes =
      if bindings_len <= List.length acc then
        acc
      else
        aux (acc @ [! (List.nth_exn env.env_bindings n)]) (n + 1) idxes
    in
    get_shuffled_idxes () |> aux [] 0
  end

let env_find_binding_with_ty ?(depth = 1000) env ty =
  let find env =
    let filter e =
      match !e with
      | IdentExpr id -> if equal_ty id.id_ty ty then true else false
      | _ -> false
    in
    match List.filter env.env_bindings ~f:filter with
    | [x] -> Some(!x)
    | [] -> None
    | l -> Some(!(Util.choose_one_exn l))
  in
  let rec aux env current_depth =
    if current_depth < depth then
      match find env with
      | Some v -> Some v
      | None ->
        if env_has_parent env then begin
          aux (env_get_parent_exn env) (current_depth + 1)
        end
        else
          None
    else
      None
  in
  aux env 0

let env_add_binding env ident =
  env.env_bindings <- env.env_bindings @ [ref ident];
  ()

let env_add_pending_binding env ident =
  env.env_pending_bindings <- env.env_pending_bindings @ [ref ident];
  ()

let env_flush_pending_bindings env =
  let aux () =
    match env.env_pending_bindings with
    | [x] -> begin
        env.env_pending_bindings <- [];
        env.env_bindings <- env.env_bindings @ [x];
        ()
      end
    | x :: xs -> begin
        env.env_pending_bindings <- xs;
        env.env_bindings <- env.env_bindings @ [x];
        ()
      end
    | _ -> ()
  in
  aux ()

let env_add_child env_parent block_env =
  env_parent.env_children <- env_parent.env_children @ [ref block_env]

let get_ident_name = function
  | IdentExpr id -> id.id_name
  | _ -> assert false

(** Generates indentation string of depth [n]. *)
let rec mk_indent is n =
  if phys_equal n 0 then ""
  else is ^ mk_indent is (n - 1)

let rec expr_to_s stmt_to_s c expr =
  let expr_to_s' = expr_to_s stmt_to_s c in
  match expr with
  | TrueExpr -> "true"
  | FalseExpr -> "false"
  | NilExpr -> "nil"
  | StringExpr s -> Printf.sprintf "\"%s\"" s
  | IdentExpr id -> id.id_name
  | TableExpr t -> begin
      match t with
      | TArray t -> begin
          List.fold_left
            t.table_elements
            ~init:[]
            ~f:(fun acc el -> begin
                  acc @ [Printf.sprintf "%s"
                           (expr_to_s' el)]
                end)
          |> String.concat ~sep:", "
          |> Printf.sprintf "{%s}"
        end
      | THashMap t -> begin
          List.fold_left
            t.table_fields
            ~init:[]
            ~f:(fun acc kv -> begin
                  acc @ [Printf.sprintf "%s = %s"
                           (expr_to_s' kv.tf_key)
                           (expr_to_s' kv.tf_value)]
                end)
          |> String.concat ~sep:", "
          |> Printf.sprintf "{%s}"
        end
    end
  | LambdaExpr e -> begin
      let args_s = exprs_to_cs stmt_to_s c e.lambda_args
      and body_s = stmt_to_s c e.lambda_body
      in
      Printf.sprintf "function (%s) %s end" args_s body_s
    end
  | IntExpr n ->
    Util.choose [ (phys_equal 0 @@ Random.int_incl 0 10),
                  lazy (Printf.sprintf "0x%x" n);
                  (phys_equal 0 @@ Random.int_incl 0 10),
                  lazy (Printf.sprintf "0x%X" n) ]
    @@ lazy (Printf.sprintf "%d" n)
  | FloatExpr n ->
    Util.choose [ (phys_equal 0 @@ Random.int_incl 0 10),
                  lazy (Printf.sprintf "%e" n);
                  (c.Config.c_use_hex_floats &&
                   phys_equal 0 @@ Random.int_incl 0 10),
                  lazy (Printf.sprintf "%h" n) ]
    @@ lazy (Printf.sprintf "%f" n)
  | UnExpr e -> begin
      let (sl, sr) = match e.un_op with
        | OpSub | OpLen -> "(", ")"
        | _ -> " ", ""
      in
      Printf.sprintf "%s%s%s%s"
        (op_to_s e.un_op)
        sl
        (expr_to_s' e.un_expr)
        sr
    end
  | BinExpr e -> begin
      Printf.sprintf "%s %s %s"
        (expr_to_s' e.bin_lhs)
        (op_to_s e.bin_op)
        (expr_to_s' e.bin_rhs)
    end
  | AttrGetExpr e -> begin
      Printf.sprintf "%s[\"%s\"]"
        (expr_to_s' e.ag_obj)
        (expr_to_s' e.ag_key)
    end
  | FuncCallExpr e -> begin
      match e.fc_ty with
      | FCFunc f -> begin
          Printf.sprintf "%s(%s)"
            (expr_to_s' f.fcf_func)
            (exprs_to_cs stmt_to_s c e.fc_args)
        end
      | FCMethod m -> begin
          Printf.sprintf "%s:%s(%s)"
            (m.fcm_receiver)
            m.fcm_method
            (exprs_to_cs stmt_to_s c e.fc_args)
        end
    end

(** Helper function that converts list of arguments to a string. *)
and exprs_to_s ?(sep = " ") stmt_to_s c exprs =
  List.fold_left
    exprs
    ~init:[]
    ~f:(fun acc expr -> acc @ [expr_to_s stmt_to_s c expr])
  |> String.concat ~sep:sep

(** Converts list of arguments to comma-separated string. *)
and exprs_to_cs stmt_to_s c exprs =
  exprs_to_s stmt_to_s c exprs ~sep:", "

let to_block_stmts_exn = function
  | BlockStmt bs -> bs.block_stmts
  | _ -> Errors.InternalError "Expected BlockStmt" |> raise

let rec stmt_to_s ?(cr = false) ?(depth = 0) c stmt =
  let mk_i () = mk_indent c.Config.c_indent depth in
  let cr_s = if cr then "\n" else "" in
  match stmt with
  | FuncDefStmt fd -> begin
      let docstring =
        [ let s = List.fold_left
              fd.fd_ty
              ~init:[]
              ~f:(fun acc ty -> acc @ [ty_to_s ty])
                  |> String.concat ~sep:", "
          in let s = if String.is_empty s then "nil" else s in
          Printf.sprintf "%s-- @return %s" (mk_i ()) s]
        |> List.append @@
        List.fold_left
          fd.fd_args
          ~init:[]
          ~f:(fun acc expr -> begin
                match expr with
                | IdentExpr id ->
                  acc @ [Printf.sprintf "%s-- @param %s %s"
                           (mk_i ()) id.id_name (ty_to_s id.id_ty)]
                | _ -> assert false
              end)
        |> String.concat ~sep:"\n"
      and args_code = exprs_to_cs stmt_to_s c fd.fd_args
      and varargs_s = if fd.fd_has_varags then
          Printf.sprintf "%s..." (if List.is_empty fd.fd_args then "" else ", ")
        else ""
      and body_code = List.fold_left
          (to_block_stmts_exn fd.fd_body)
          ~init:[]
          ~f:(fun acc stmt ->
              acc @ [stmt_to_s c stmt ~depth:(depth + 1)])
                      |> String.concat ~sep:"\n"
      and name = match fd.fd_receiver with
        | Some r -> Printf.sprintf "%s:%s" r fd.fd_name
        | None -> fd.fd_name
      in
      Printf.sprintf "%s\n%sfunction %s(%s%s)\n%s\n%send%s"
        docstring
        (mk_i ())
        name
        args_code
        varargs_s
        body_code
        (mk_i ())
        cr_s
    end
  | FuncCallStmt s -> begin
      let (name, args_s) = match s.fc_expr with
        | FuncCallExpr fce -> begin
            match fce.fc_ty with
            | FCFunc f -> (get_ident_name f.fcf_func,
                           exprs_to_cs stmt_to_s c fce.fc_args)
            | FCMethod m -> begin
                let name = Printf.sprintf "%s:%s" m.fcm_receiver m.fcm_method
                in
                (name, exprs_to_cs stmt_to_s c fce.fc_args)
              end
          end
        | _ -> assert false
      in
      Printf.sprintf "%s%s(%s)" (mk_i ()) name args_s
    end
  | AssignStmt s -> begin
      let l_s = if s.assign_local then "local " else ""
      and lhs_s = exprs_to_cs stmt_to_s c s.assign_lhs
      and rhs_s = exprs_to_cs stmt_to_s c s.assign_rhs in
      Printf.sprintf "%s%s%s = %s%s" (mk_i ()) l_s lhs_s rhs_s cr_s
    end
  | IfStmt s -> begin
      let cond_s = expr_to_s stmt_to_s c s.if_cond
      and body_s = stmt_to_s c s.if_body ~depth:(depth + 1)
      and else_s = match s.if_else with
        | Some es -> begin
            let es_s = stmt_to_s c es ~depth:(depth + 1) in
            Printf.sprintf "\n%selse\n%s" (mk_i ()) es_s
          end
        | None -> ""
      in
      Printf.sprintf "%sif %s then\n%s%s\n%send"
        (mk_i ()) cond_s body_s else_s (mk_i ())
    end
  | BlockStmt s -> begin
      List.fold_left
        s.block_stmts
        ~init:[]
        ~f:(fun acc s -> acc @ [stmt_to_s c s ~depth:(depth + 1)])
      |> String.concat ~sep:"\n"
    end
  | LoopStmt s -> begin
      let cond_s = expr_to_s stmt_to_s c s.loop_cond
      and body_s = stmt_to_s c s.loop_block ~depth:(depth + 1)
      in
      match s.loop_ty with
      | While ->
        Printf.sprintf "%swhile %s do\n%s\n%send"
          (mk_i ()) cond_s body_s (mk_i ())
      | Repeat ->
        Printf.sprintf "%srepeat\n%s\n%suntil %s"
          (mk_i ()) body_s (mk_i ()) cond_s
    end
  | DoBlockStmt s -> begin
      let body_s = stmt_to_s c s.do_block ~depth:(depth + 1) in
      Printf.sprintf "%sdo\n%s\n%send" (mk_i ()) body_s (mk_i ())
    end
  | BreakStmt -> Printf.sprintf "%sbreak" (mk_i ())
  | ReturnStmt s -> begin
      let exprs_s = exprs_to_cs stmt_to_s c s.return_exprs in
      Printf.sprintf "%sreturn %s" (mk_i ()) exprs_s
    end
  | NumForStmt s -> begin
      let for_init = match s.nfor_step with
        | IntExpr 1 ->
          Printf.sprintf "%s=%s,%s"
            (s.nfor_name)
            (expr_to_s stmt_to_s c s.nfor_init)
            (expr_to_s stmt_to_s c s.nfor_limit)
        | _ ->
          Printf.sprintf "%s=%s,%s,%s"
            (s.nfor_name)
            (expr_to_s stmt_to_s c s.nfor_init)
            (expr_to_s stmt_to_s c s.nfor_limit)
            (expr_to_s stmt_to_s c s.nfor_step)
      in
      let for_body = stmt_to_s c s.nfor_body ~depth:(depth + 1) in
      Printf.sprintf "%sfor %s do\n%s\n%send" (mk_i ()) for_init for_body (mk_i ())
    end
  | ForStmt s -> begin
      let for_names = exprs_to_cs stmt_to_s c s.for_names
      and for_exprs = exprs_to_cs stmt_to_s c s.for_exprs
      and for_body = stmt_to_s c s.for_body ~depth:(depth + 1) in
      Printf.sprintf "%sfor %s in %s do\n%s\n%send"
        (mk_i ()) for_names for_exprs for_body (mk_i ())
    end

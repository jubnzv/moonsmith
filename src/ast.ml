open Core_kernel

exception InternalError of string

(** Essential types present in Lua. *)
type ty =
  | TyNil
  | TyBoolean
  | TyNumber
  | TyString
  | TyUserdata
  | TyFunction
  | TyThread
  | TyTable

type operator =
  | OpAdd     (* + *)
  | OpSub     (* - *)
  | OpMul     (* * *)
  | OpDiv     (* / *)
  | OpPow     (* ^ *)
  | OpMod     (* % *)
  | OpConcat  (* .. *)
  | OpLt      (* < *)
  | OpLte     (* <= *)
  | OpGt      (* > *)
  | OpGte     (* >= *)
  | OpEq      (* == *)
  | OpNeq     (* ~= *)
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
  | NumberExpr of float
  | StringExpr of string
  | IdentExpr of { id_name: string;
                   id_ty: ty }
  | AttrGetExpr of { ag_obj: expr;
                     ag_key: expr }
  | TableExpr of table_ty
  | LambdaExpr of { lambda_args: expr list;
                    lambda_body: stmt; }
  | FuncCallExpr of func_call
  | UnExpr of { un_op: operator;
                un_expr: expr }
  | BinExpr of { bin_lhs: expr;
                 bin_op: operator;
                 bin_rhs: expr }
and table_ty =
  | TArray of { table_elements: expr list }
  | THashMap of { table_fields: table_field list }
and table_field =
  { tf_key: expr; tf_value: expr }
and func_call =
  | FCMethod of { fcm_receiver: expr;
                  fcm_method: string;
                  fcm_args: expr list }
  | FCFunc of { fcf_func: expr;
                fcf_args: expr list }

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
  | FuncDefStmt of { fd_name: string;
                     fd_args: expr list;
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

let ty_to_s = function
  | TyNil       -> "nil"
  | TyBoolean   -> "boolean"
  | TyNumber    -> "number"
  | TyString    -> "string"
  | TyUserdata  -> "userdata"
  | TyFunction  -> "function"
  | TyThread    -> "thread"
  | TyTable     -> "table"

let get_essential_ty expr =
  match expr with
  | TrueExpr | FalseExpr -> Some(TyBoolean)
  | NilExpr              -> Some(TyNil)
  | NumberExpr _         -> Some(TyNumber)
  | StringExpr _         -> Some(TyString)
  | IdentExpr id         -> Some(id.id_ty)
  | TableExpr _          -> Some(TyTable)
  | LambdaExpr _         -> Some(TyFunction)
  | _ -> None

let types_are_comparable ty_lhs ty_rhs =
  match (ty_lhs, ty_rhs) with
  | (TyNil, TyNil)           -> false
  | (TyBoolean, TyBoolean)   -> false
  | (TyNumber, TyNumber)     -> true
  | (TyString, TyString)     -> true
  | (TyUserdata, TyUserdata) -> false
  | (TyFunction, TyFunction) -> false
  | (TyThread, TyThread)     -> false
  | (TyTable, TyTable)       -> false
  | _ -> false

let get_block_env_exn = function
  | BlockStmt s -> s.block_env
  | _ -> raise @@ InternalError "Expected BlockStmt"

let op_to_s = function
  | OpAdd     ->  "+"
  | OpSub     ->  "-"
  | OpMul     ->  "*"
  | OpDiv     ->  "/"
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
  | OpLen     ->  "#"

let env_mk () =
  { env_bindings = [];
    env_pending_bindings = [];
    env_parent = None;
    env_children = [] }

let env_empty env =
  phys_equal 0 @@ List.length env.env_bindings

let env_has_parent env =
  match env.env_parent with Some _ -> true | None -> false

let env_get_parent_exn env =
  match env.env_parent with
  | Some parent -> !parent
  | None -> raise @@ InternalError "Env has no parent!"

let env_take_rand_exn env =
  let idx = Random.int_incl 0 @@ (List.length env.env_bindings) - 1 in
  List.nth_exn env.env_bindings idx

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
  | NumberExpr v -> Printf.sprintf "%f" v
  | UnExpr e -> begin
      let s = match e.un_op with
        | OpSub -> ""
        | _ -> " "
      in
      Printf.sprintf "%s%s%s"
        (op_to_s e.un_op)
        s
        (expr_to_s' e.un_expr)
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
      match e with
      | FCFunc f -> begin
          Printf.sprintf "%s(%s)"
            (expr_to_s' f.fcf_func)
            (exprs_to_cs stmt_to_s c f.fcf_args)
        end
      | FCMethod m -> begin
          Printf.sprintf "%s:%s(%s)"
            (expr_to_s' m.fcm_receiver)
            m.fcm_method
            (exprs_to_cs stmt_to_s c m.fcm_args)
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
  | _ -> raise @@ InternalError "Expected BlockStmt"

let rec stmt_to_s ?(cr = false) ?(depth = 0) c stmt =
  let mk_i () = mk_indent c.Config.c_indent depth in
  let cr_s = if cr then "\n" else "" in
  match stmt with
  | FuncDefStmt fd -> begin
      let docstring =
        [ List.fold_left
            fd.fd_ty
            ~init:[]
            ~f:(fun acc ty -> acc @ [ty_to_s ty])
          |> String.concat ~sep:", "
          |> Printf.sprintf "%s-- @return %s" (mk_i ())]
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
      and body_code = List.fold_left
          (to_block_stmts_exn fd.fd_body)
          ~init:[]
          ~f:(fun acc stmt ->
              acc @ [stmt_to_s c stmt ~depth:(depth + 1)])
                      |> String.concat ~sep:"\n"
      in
      Printf.sprintf "%s\n%sfunction %s(%s)\n%s\n%send%s"
        docstring
        (mk_i ())
        fd.fd_name
        args_code
        body_code
        (mk_i ())
        cr_s
    end
  | FuncCallStmt s -> begin
      let (name, args_s) = match s.fc_expr with
        | FuncCallExpr fce -> begin
            match fce with
            | FCFunc f -> (get_ident_name f.fcf_func,
                           exprs_to_cs stmt_to_s c f.fcf_args)
            | FCMethod m -> begin
                let name = Printf.sprintf "%s:%s"
                    (get_ident_name m.fcm_receiver)
                    m.fcm_method
                in
                (name, exprs_to_cs stmt_to_s c m.fcm_args)
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
  | NumForStmt _ | ForStmt _ -> assert false

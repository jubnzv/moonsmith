type t =
  { c_indent: string;
    c_debug: bool;
    c_min_toplevel_stmts: int;
    c_max_toplevel_stmts: int;

    (** Seed used to initialize random generator. If None, seed will be choosen randomly. *)
    c_seed: int option;

    (** Forces to execute each function occurred on the top-level. *)
    c_execute_toplevel: bool }

let mk_default () =
  { c_indent = "  ";
    c_debug = true;
    c_min_toplevel_stmts = 5;
    c_max_toplevel_stmts = 10;
    c_seed = None;
    c_execute_toplevel = true; }

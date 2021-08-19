type t =
  { c_indent: string;
    c_min_toplevel_stmts: int;
    c_max_toplevel_stmts: int;

    (** Prints generated program to stdout. *)
    c_stdout: bool;

    (** Seed used to initialize random generator. If None, seed will be choosen randomly. *)
    c_seed: int option;

    (** Forces to execute each function occurred on the top-level. *)
    c_execute_toplevel: bool }

let mk_default () =
  { c_indent = "  ";
    c_min_toplevel_stmts = 5;
    c_max_toplevel_stmts = 10;
    c_stdout = false;
    c_seed = None;
    c_execute_toplevel = true; }

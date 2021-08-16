type t =
  { c_indent: string;
    c_debug: bool;
    c_min_toplevel_stmts: int;
    c_max_toplevel_stmts: int;
    (** Forces to execute each function occurred on the top-level. *)
    c_execute_toplevel: bool }

let mk_default () =
  { c_indent = "  ";
    c_debug = true;
    c_min_toplevel_stmts = 5;
    c_max_toplevel_stmts = 10;
    c_execute_toplevel = true; }

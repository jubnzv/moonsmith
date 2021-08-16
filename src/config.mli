(** Config module describes user-defined options for program generation. *)

type t =
  { c_indent: string;
    c_debug: bool;
    c_min_toplevel_stmts: int;
    c_max_toplevel_stmts: int;
    (** Forces to execute each function occurred on the top-level. *)
    c_execute_toplevel: bool }

val mk_default : unit -> t

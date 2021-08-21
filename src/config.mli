(** Config module describes user-defined options for program generation. *)

type t =
  { c_indent: string;
    c_min_toplevel_stmts: int;
    c_max_toplevel_stmts: int;

    (** Prints generated program to stdout. *)
    c_stdout: bool;

    (** Seed used to initialize random generator. If None, seed will be choosen randomly. *)
    c_seed: int option;

    (** If true, generates random OOP tables with methods, inheritance, etc. *)
    c_generate_oop_tables: bool;

    (** Add additional statements at the end of top-level that execute each
        function and method on the top-level. *)
    c_execute_toplevel: bool }

val mk_default : unit -> t

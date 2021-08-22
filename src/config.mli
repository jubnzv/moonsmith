(** User-defined options for generating random programs. *)
type t = {
  c_indent: string;
  (** Minimum number of datums defined on the top-level. *)
  c_min_toplevel_datums: int;
  (** Maximum number of datums defined on the top-level. *)
  c_max_toplevel_datums: int;
  (** Minimum number of functions and methods defined on the top-level. *)
  c_min_toplevel_funcdefs: int;
  (** Maximum number of functions and methods defined on the top-level. *)
  c_max_toplevel_funcdefs: int;
  (** Prints generated program to stdout. *)
  c_stdout: bool;
  (** Seed used to initialize random generator. If None, seed will be choosen randomly. *)
  c_seed: int option;
  (** If true, generate some random OOP methods for the datum tables. *)
  c_gen_oop_methods: bool;
  (* *** Language features *** *)
  (** Use length ('#') operators on strings and tables. *)
  c_use_length: bool;
  (** Use tostring(). *)
  c_use_tostring: bool;
  (** Use string.sub(). *)
  c_use_string_sub: bool;
  (** Use math.floor(). *)
  c_use_math_floor: bool;
}

val mk_default : unit -> t

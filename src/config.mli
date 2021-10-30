(** User-defined options for generating random programs. *)
type t = {
  c_indent: string;
  (** Indentation symbols used in generated Lua code. *)

  c_lib_path: string option;
  (** Path to external Lua module used in runtime. *)

  c_min_toplevel_datums: int;
  (** Minimum number of datums defined on the top-level. *)

  c_max_toplevel_datums: int;
  (** Maximum number of datums defined on the top-level. *)

  c_min_toplevel_funcdefs: int;
  (** Minimum number of functions and methods defined on the top-level. *)

  c_max_toplevel_funcdefs: int;
  (** Maximum number of functions and methods defined on the top-level. *)

  c_stdout: bool;
  (** Prints generated program to stdout. *)

  c_seed: int option;
  (** Seed used to initialize random generator. If None, seed will be choosen randomly. *)

  c_gen_oop_methods: bool;
  (** If true, generate some random OOP methods for the datum tables. *)

  c_use_hex_floats: bool;
  (** Use hexademical floats introduced in Lua 5.3. *)

  (* *** Language features *** *)

  c_use_length: bool;
  (** Use length ('#') operators on strings and tables. *)

  c_use_pairs: bool;
  (** Use 'pairs' function. *)

  c_use_ipairs: bool;
  (** Use 'ipairs' function. *)

  c_use_tostring: bool;
  (** Use tostring(). *)

  c_use_tonumber: bool;
  (** Use tonumber(). *)

  c_use_string_upper: bool;
  (** Use string.upper(). *)

  c_use_string_lower: bool;
  (** Use string.lower(). *)

  c_use_string_sub: bool;
  (** Use string.sub(). *)

  c_use_string_reverse: bool;
  (** Use string.reverse(). *)

  c_use_string_gsub: bool;
  (** Use string.gsub(). *)

  c_use_string_len: bool;
  (** Use string.len(). *)

  c_use_math_type: bool;
  (** Use math.type() *)

  c_use_math_ult: bool;
  (** Use math.ult() *)

  c_use_math_sin: bool;
  (** Use math.sin() *)

  c_use_math_cos: bool;
  (** Use math.cos() *)

  c_use_math_tan: bool;
  (** Use math.tan() *)

  c_use_math_abs: bool;
  (** Use math.abs() *)

  c_use_math_pi: bool;
  (** Use math.pi *)

  c_use_math_log: bool;
  (** Use math.log() *)

  c_use_math_floor: bool;
  (** Use math.floor(). *)
}

val from_yaml : string -> t option

val mk_default : unit -> t

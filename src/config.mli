(** User-defined options for generating random programs. *)
type t = {
  c_indent: string [@key "indent"];
  (** Minimum number of datums defined on the top-level. *)

  c_lib_path: string option [@key "lib_path"];
  (** Path to external Lua module used in runtime. *)

  c_min_toplevel_datums: int [@key "min_toplevel_datums"];
  (** Minimum number of datums defined on the top-level. *)

  c_max_toplevel_datums: int [@key "max_toplevel_datums"];
  (** Maximum number of datums defined on the top-level. *)

  c_min_toplevel_funcdefs: int [@key "min_toplevel_funcdefs"];
  (** Minimum number of functions and methods defined on the top-level. *)

  c_max_toplevel_funcdefs: int [@key "max_toplevel_funcdefs"];
  (** Maximum number of functions and methods defined on the top-level. *)

  c_stdout: bool [@key "stdout"];
  (** Prints generated program to stdout. *)

  c_seed: int option [@key "seed"];
  (** Seed used to initialize random generator. If None, seed will be choosen randomly. *)

  c_gen_oop_methods: bool [@key "gen_oop_methods"];
  (** If true, generate some random OOP methods for the datum tables. *)

  c_use_hex_floats: bool [@key "use_hex_floats"];
  (** Use hexademical floats introduced in Lua 5.3. *)

  (* *** Language features *** *)

  c_use_length: bool [@key "use_length"];
  (** Use length ('#') operators on strings and tables. *)

  c_use_pairs: bool [@key "use_pairs"];
  (** Use 'pairs' function. *)

  c_use_ipairs: bool [@key "use_ipairs"];
  (** Use 'ipairs' function. *)

  c_use_tostring: bool [@key "use_tostring"];
  (** Use tostring(). *)

  c_use_tonumber: bool [@key "use_tonumber"];
  (** Use tonumber(). *)

  c_use_string_upper: bool [@key "use_string_upper"];
  (** Use string.upper(). *)

  c_use_string_lower: bool [@key "use_string_lower"];
  (** Use string.lower(). *)

  c_use_string_sub: bool [@key "use_string_sub"];
  (** Use string.sub(). *)

  c_use_string_reverse: bool [@key "use_string_reverse"];
  (** Use string.reverse(). *)

  c_use_string_len: bool [@key "use_string_len"];
  (** Use string.len(). *)

  c_use_math_type: bool [@key "use_math_type"];
  (** Use math.type() *)

  c_use_math_ult: bool [@key "use_math_ult"];
  (** Use math.ult() *)

  c_use_math_sin: bool [@key "use_math_sin"];
  (** Use math.sin() *)

  c_use_math_cos: bool [@key "use_math_cos"];
  (** Use math.cos() *)

  c_use_math_tan: bool [@key "use_math_tan"];
  (** Use math.tan() *)

  c_use_math_abs: bool [@key "use_math_abs"];
  (** Use math.abs() *)

  c_use_math_pi: bool [@key "use_math_pi"];
  (** Use math.pi *)

  c_use_math_log: bool [@key "use_math_log"];
  (** Use math.log() *)

  c_use_math_floor: bool [@key "use_math_floor"];
  (** Use math.floor(). *)
} [@@deriving yojson]

val mk_default : unit -> t

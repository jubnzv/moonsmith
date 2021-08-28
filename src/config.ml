(** User-defined options for generating random programs. *)
type t = {
  c_indent: string;
  (** Minimum number of datums defined on the top-level. *)
  c_lib_path: string option;
  (** Path to external Lua module used in runtime. *)
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
  (** Use hexademical floats introduced in Lua 5.3. *)
  c_use_hex_floats: bool;
  (* *** Language features *** *)
  (** Use length ('#') operators on strings and tables. *)
  c_use_length: bool;
  (** Use 'pairs' function. *)
  c_use_pairs: bool;
  (** Use 'ipairs' function. *)
  c_use_ipairs: bool;
  (** Use tostring(). *)
  c_use_tostring: bool;
  (** Use string.upper(). *)
  c_use_string_upper: bool;
  (** Use string.lower(). *)
  c_use_string_lower: bool;
  (** Use string.sub(). *)
  c_use_string_sub: bool;
  (** Use string.reverse(). *)
  c_use_string_reverse: bool;
  (** Use string.len(). *)
  c_use_string_len: bool;
  (** Use math.type() *)
  c_use_math_type: bool;
  (** Use math.ult() *)
  c_use_math_ult: bool;
  (** Use math.sin() *)
  c_use_math_sin: bool;
  (** Use math.cos() *)
  c_use_math_cos: bool;
  (** Use math.pi *)
  c_use_math_pi: bool;
  (** Use math.log() *)
  c_use_math_log: bool;
  (** Use math.floor(). *)
  c_use_math_floor: bool;
}

let mk_default () =
  { c_indent = "  ";
    c_lib_path = Some("lua/lib.lua");
    c_min_toplevel_datums = 5;
    c_max_toplevel_datums = 10;
    c_min_toplevel_funcdefs = 4;
    c_max_toplevel_funcdefs = 8;
    c_stdout = false;
    c_seed = None;
    c_gen_oop_methods = true;
    c_use_hex_floats = true;
    c_use_length = true;
    c_use_pairs = true;
    c_use_ipairs = true;
    c_use_tostring = true;
    c_use_string_upper = true;
    c_use_string_lower = true;
    c_use_string_sub = true;
    c_use_string_reverse = true;
    c_use_string_len = true;
    c_use_math_type = true;
    c_use_math_ult = true;
    c_use_math_sin = true;
    c_use_math_cos = true;
    c_use_math_pi = true;
    c_use_math_log = true;
    c_use_math_floor = true; }

open Core_kernel

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
    c_use_tonumber = true;
    c_use_string_upper = true;
    c_use_string_lower = true;
    c_use_string_sub = true;
    c_use_string_reverse = true;
    c_use_string_gsub = true;
    c_use_string_len = true;
    c_use_math_type = true;
    c_use_math_ult = true;
    c_use_math_sin = true;
    c_use_math_cos = true;
    c_use_math_tan = true;
    c_use_math_abs = true;
    c_use_math_pi = true;
    c_use_math_log = true;
    c_use_math_floor = true; }

let get_yaml_value_exn (v : Yaml.yaml) =
  match v with
  | `Scalar scalar -> scalar.value
  | _ -> failwith "Configuration file is broken."

let set_config_value acc fn =
  Caml.Option.fold
    ~none:None
    ~some:fn
    acc

(** [set_general_settings] parses the [general] option from the configuration
    file. *)
let set_general_settings acc (mapping : Yaml.mapping) =
  let f = (fun acc ((k : Yaml.yaml),v) -> begin
        match k with
        | `Scalar scalar -> begin
            match scalar.value with
            | "indent" ->
              set_config_value acc
                (fun c -> Some({ c with c_indent = get_yaml_value_exn v }))
            | "lib_path" ->
              set_config_value acc
                (fun c -> Some({ c with c_lib_path = Some(get_yaml_value_exn v) }))
            | "min_toplevel_datums" ->
              set_config_value acc
                (fun c -> Some({ c with c_min_toplevel_datums =
                                          int_of_string @@ get_yaml_value_exn v }))
            | "max_toplevel_datums" ->
              set_config_value acc
                (fun c -> Some({ c with c_max_toplevel_datums =
                                          int_of_string @@ get_yaml_value_exn v }))
            | "min_toplevel_funcdefs" ->
              set_config_value acc
                (fun c -> Some({ c with c_min_toplevel_funcdefs =
                                          int_of_string @@ get_yaml_value_exn v }))
            | "max_toplevel_funcdefs" ->
              set_config_value acc
                (fun c -> Some({ c with c_max_toplevel_funcdefs =
                                          int_of_string @@ get_yaml_value_exn v }))
            | "stdout" ->
              set_config_value acc
                (fun c -> Some({ c with c_stdout =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "seed" ->
              set_config_value acc
                (fun c -> Some({ c with c_seed =
                                          match get_yaml_value_exn v with
                                          | "null" -> None
                                          | num -> Some(int_of_string num) }))
            | _ -> begin
                Printf.printf "Unexpected configuration option: %s\n" scalar.value;
                acc
              end
          end
        | _ -> begin
            Printf.printf "Configuration file is broken\n";
            acc
          end
      end)
  in
  List.fold_left
    mapping.m_members
    ~init:acc
    ~f:f

(** [set_language_settings] parses [language] option from the configuration
    file. *)
let set_language_settings acc (mapping : Yaml.mapping) =
  let f = (fun acc ((k : Yaml.yaml),v) -> begin
        match k with
        | `Scalar scalar -> begin
            match scalar.value with
            | "gen_oop_methods" ->
              set_config_value acc
                (fun c -> Some({ c with c_gen_oop_methods =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_hex_floats" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_hex_floats =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_length" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_length =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_pairs" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_pairs =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_ipairs" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_ipairs =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_tostring" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_tostring =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_tonumber" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_tonumber =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_string_upper" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_string_upper =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_string_lower" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_string_lower =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_string_sub" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_string_sub =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_string_reverse" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_string_reverse =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_string_gsub" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_string_gsub =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_string_len" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_string_len =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_type" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_type =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_ult" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_ult =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_sin" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_sin =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_cos" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_cos =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_tan" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_tan =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_abs" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_abs =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_pi" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_pi =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_log" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_log =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | "use_math_floor" ->
              set_config_value acc
                (fun c -> Some({ c with c_use_math_floor =
                                          bool_of_string @@ get_yaml_value_exn v }))
            | _ -> begin
                Printf.printf "Unexpected configuration option: %s\n" scalar.value;
                acc
              end
          end
        | _ -> begin
            Printf.printf "Configuration file is broken\n";
            acc
          end
      end)
  in
  List.fold_left
    mapping.m_members
    ~init:acc
    ~f:f

let from_yaml filepath =
  let yaml = In_channel.read_all filepath
             |> Yaml.yaml_of_string
             |> Caml.Result.get_ok
  in
  match yaml with
  | `O mapping -> begin
      List.fold_left
        mapping.m_members
        ~init:(Some(mk_default ()))
        ~f:(fun acc (k, v) -> begin
              match k with
              | `Scalar scalar -> begin
                  match scalar.value with
                  | "general" -> begin
                      match v with
                      | `O mapping -> set_general_settings acc mapping
                      | _ -> begin
                          Printf.printf "Configuration file is broken\n";
                          acc
                        end
                    end
                  | "language" -> begin
                      match v with
                      | `O mapping -> set_language_settings acc mapping
                      | _ -> begin
                          Printf.printf "Configuration file is broken\n";
                          acc
                        end
                    end
                  | _ -> begin
                      Printf.printf "Unexpected configuration option: %s\n" scalar.value;
                      acc
                    end
                end
              | _ -> None
            end)
    end
  | _ -> None

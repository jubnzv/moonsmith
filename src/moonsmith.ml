open Core_kernel

let set_seed c s =
  match int_of_string_opt s with
  | Some i -> begin
      if i > 0 then { c with Config.c_seed = Some(i) }
      else          { c with Config.c_seed = Some(-i) }
    end
  | None -> c

let () =
  Clap.description "Random generator of Lua programs";

  let out =
    Clap.default_string
      ~short:'o'
      ~description:
        "Location of the generated Lua file"
      ~placeholder:"OUTPUT"
      "out.lua"
  and libpath =
    Clap.default_string
      ~short:'I'
      ~description:
        "Path to extra Lua module used in runtime. If not exists, it won't be used."
      ~placeholder:"LIBPATH"
      "lua/lib.lua"
  and nolib =
    Clap.default_int
      ~short:'n'
      ~description:
        "Don't use extra Lua module."
      ~placeholder:"NOLIB"
      0
  and seed =
    Clap.default_string
      ~short:'s'
      ~description:
        "Seed used to initialize random generator. If not set, seed will be choosen randomly."
      ~placeholder:"SEED"
      ""
  and stdout =
    Clap.default_int
      ~short:'S'
      ~description:
        "Print generated program to stdout"
      ~placeholder:"STDOUT"
      0
  in

  Clap.close ();

  let c = Config.mk_default () in
  let c = set_seed c seed in
  let c = { c with c_stdout = phys_equal stdout 1 } in
  let c =
    if phys_equal nolib 1 then { c with c_lib_path = None }
    else { c with c_lib_path = Some(libpath) }
  in
  let program = Generate.generate c in
  let oc = Out_channel.create out in
  Out_channel.output_string oc program;
  Out_channel.flush oc;
  Out_channel.close oc;
  if c.Config.c_stdout then Printf.printf "%s\n" program else ();

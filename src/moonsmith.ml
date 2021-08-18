open Core_kernel

let () =
  Clap.description "Random generator of Lua programs";

  let out =
    Clap.default_string
      ~short: 'o'
      ~description:
        "Location of the generated Lua file"
      ~placeholder: "OUTPUT"
      "out.lua"
  in

  Clap.close ();

  let c = Config.mk_default () in
  let program = Generate.generate c in
  let oc = Out_channel.create out in
  Out_channel.output_string oc program;
  Out_channel.flush oc;
  Out_channel.close oc;
  if c.Config.c_debug then Printf.printf "%s\n" program else ();

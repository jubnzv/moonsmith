open Core_kernel

let () =
  let c = Config.mk_default () in
  let program = Generate.generate c in
  let oc = Out_channel.create "test.lua" in
  Out_channel.output_string oc program;
  Out_channel.flush oc;
  Out_channel.close oc;
  if c.Config.c_debug then Printf.printf "%s\n" program else ();

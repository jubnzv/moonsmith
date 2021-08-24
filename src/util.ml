open Core_kernel

let choose_one_exn l =
  List.nth_exn l @@ Random.int @@ List.length l

let replace l idx v =
  List.mapi l
    ~f:(fun i x -> if i = idx then v else x)

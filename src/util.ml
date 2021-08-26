open Core_kernel

let choose_one_exn l =
  List.nth_exn l @@ Random.int @@ List.length l

let replace l idx v =
  List.mapi l
    ~f:(fun i x -> if i = idx then v else x)

let ( -- ) i j =
  let rec aux n acc =
    if n < i then acc
    else aux (n - 1) (n :: acc)
  in aux (j - 1) []

open Core_kernel

let choose_one_exn l =
  List.nth_exn l @@ Random.int @@ List.length l

let choose kv fallback =
  let l = List.fold_left
      kv
      ~init:[]
      ~f:(fun acc (k, v) -> if k then acc @ [v] else acc)
  in if List.is_empty l then Lazy.force fallback
  else Lazy.force (choose_one_exn l)

let choose_lazy_exn vs =
  let l = List.fold_left
      vs
      ~init:[]
      ~f:(fun acc v -> acc @ [v])
  in
  Lazy.force (choose_one_exn l)

let replace l idx v =
  List.mapi l
    ~f:(fun i x -> if i = idx then v else x)

let ( -- ) i j =
  let rec aux n acc =
    if n < i then acc
    else aux (n - 1) (n :: acc)
  in aux (j - 1) []

let endswith s1 s2 =
  let len1 = String.length s1 and len2 = String.length s2 in
  if len1 < len2 then false
  else
    let sub = String.sub s1 ~pos:(len1 - len2) ~len:(len2) in
    String.equal sub s2

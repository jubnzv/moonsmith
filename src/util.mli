(** Randomly chooses one element from the list. *)
val choose_one_exn : 'a list -> 'a

val choose_lazy_exn : ('a lazy_t) list -> 'a

val choose : (bool * 'a lazy_t) list -> 'a lazy_t -> 'a

(** Replaces element in the given list. *)
val replace : 'a list -> int -> 'a -> 'a list

val ( -- ) : int -> int -> int list

val endswith : string -> string -> bool

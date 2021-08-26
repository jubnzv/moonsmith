(** Randomly chooses one element from the list. *)
val choose_one_exn : 'a list -> 'a

(** Replaces element in the given list. *)
val replace : 'a list -> int -> 'a -> 'a list

(** Range operator: generates range of integers [i, j). *)
val ( -- ) : int -> int -> int list

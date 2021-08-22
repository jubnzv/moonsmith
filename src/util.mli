open Core_kernel

(** Randomly chooses one element from the list. *)
val choose_one : 'a list -> 'a

(** Replaces element in the given list. *)
val replace : 'a list -> int -> 'a -> 'a list

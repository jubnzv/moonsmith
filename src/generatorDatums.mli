(** Generates global datums for the context [ctx].
    These variables mutate in the functions and methods and later combined to
    the result string. *)
val generate : Context.t -> Context.t

(** This module provides interfaces for generation random Lua AST from the
    user-defined config.. *)

(** Entry point of code generation. *)
val generate : Config.t -> string

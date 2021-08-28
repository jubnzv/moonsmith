(** This module handles code generation for calls of functions from the Lua's
    standard library as well as for functions from the moonsmith's helper
    library. *)

val mk_funccall : string -> Ast.expr list -> Ast.expr

val mk_ident : ?ty:Ast.ty -> string -> Ast.expr

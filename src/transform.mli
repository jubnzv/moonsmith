(** This module contains functions that transforms Lua expressions between
    various types. *)

(** Converts given expression with its type to a new expression that results as [TyNil].  *)
val to_nil : Context.t -> Ast.ty -> Ast.expr -> Ast.expr

(** Converts given expression with its type to a new expression that results as [TyBoolean].  *)
val to_boolean : Context.t -> Ast.ty -> Ast.expr -> Ast.expr

(** Converts given expression with its type to a new expression that results as [TyInt].  *)
val to_int : Context.t -> Ast.ty -> Ast.expr -> Ast.expr

(** Converts given expression with its type to a new expression that results as [TyFloat].  *)
val to_float : Context.t -> Ast.ty -> Ast.expr -> Ast.expr

(** Converts given expression with its type to a new expression that results as [TyString].  *)
val to_string : Context.t -> Ast.ty -> Ast.expr -> Ast.expr

(** Converts given expression with its type to a new expression that results as [TyFunction].  *)
val to_function : Context.t -> Ast.ty -> Ast.expr -> Ast.expr

(** Converts given expression with its type to a new expression that results as [TyTable].  *)
val to_table : Context.t -> Ast.ty -> Ast.expr -> Ast.expr

(* state.mli *)

type t = Exp.t * Subst.t
(** computational state of schema matching *)

val to_string : t -> string

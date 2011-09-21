(* subst.mli *)

type t = (Symbol.t * Term.t) list
(** type substitution *)

val is_consistent : t -> bool
(** checks if list of substitution is well defined *)

val apply_to_term : Term.t -> t -> Term.t
(** [apply_to_term t s] applies substitution s to term t *)

val apply_to_exp : Exp.t -> t -> Exp.t
(** [apply_to_exp e s] applies substitution s
    to expression e *)

val apply_to_subst : t -> t -> t

val simplify : t -> t

val to_string : t -> string

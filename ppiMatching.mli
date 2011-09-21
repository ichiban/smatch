(* ppiMatching.mli *)

exception Not_matchable

val ppi_match : Strategy.t -> Exp.t -> Subst.t list
(** [ppi_match st ep] is matchers of a cluster ep under a strategy st *)

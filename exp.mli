(* exp.mli *)

type t = (Term.t * Term.t) list
(** type matching expression *)

val cluster : Symbol.t -> t -> t * t
(** cluster p E split E to 2 expressions Ep and Er.
    Ep is an expression which schemata-side headers are p and
    Er is E - Ep *)

val to_string : t -> string

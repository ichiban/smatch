(* strategy.mli *)

type t =
    ITerm.t list -> ITerm.t list

val none : t
val imit_preference : t
val strict : t
val nonredundant : t

val compose : t -> t -> t
val ( *.. ) : t -> t -> t

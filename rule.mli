
(* rule.mli *)

exception Imit_failed

val simp : State.t -> State.t
val imit : State.t -> State.t
val proj : State.t -> State.t list

val ppi : Strategy.t -> State.t -> State.t list

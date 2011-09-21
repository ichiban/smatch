(* matching.mli *)

val preprocess : Term.t * Term.t -> State.t
  (** preprocesses schema matching pair *)
     
val derive : bool -> Strategy.t -> int option -> State.t -> Subst.t list
  (** takes states of matching process,
      returns list of matchers. *)

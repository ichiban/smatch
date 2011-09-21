(* iTerm.mli *)

type index =
    (int * Subst.t) list

type t =
    | Star of index
    | Atom of Symbol.t * index
    | Appl of t * t * index

val ihd : t -> t
(** head of indexed term. *) 

val ibody : t -> t list
(** arguments of indexed term *)

val symbol : t -> Symbol.t
(** symbol of atomic indexed term *)

val label : t -> index

val arity : t -> int
(** arity of indexed term *)

val construct : Term.t -> Term.t -> t

val commonize : t list -> t

val reduce : t -> t

val split : t -> t list
(** *)

val to_string : t -> string


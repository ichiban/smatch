(* misc.mli *)

val id : 'a -> 'a
(** identifier. same as fun x -> x. *)

val n2m : int -> int -> int list
(** n2m n m is [n; n+1; ...; m-1; m] *)

val append : 'a list -> 'a list -> 'a list
val (@) : 'a list -> 'a list -> 'a list
(** tail-recursive List.append *)

val no_overlaps : 'a list -> 'a list
(** makes list with no overlaps *)

val map : ('a -> 'b) -> 'a list -> 'b list
(** tail-recursive List.map *)

val comb : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** comb f [a1; ...; an] [b1; ...; bm] is [f a1 b1; ...; f a1 bm; f a2 b1; ...; f an bm]. *)

(* symbol.mli *)

type ident = string * int option

type t =
    | FC of ident
	(* f, g, h, ... *)
    | FV of ident
	(* F, G, H, ... *)
    | FSV of ident
	(* ?f, ?g, ?h, ... *)
    | PC of ident
	(* p, q, r, ... *)
    | PV of ident
	(* P, Q, R, ... *)
    | AQ of t
	(* AX., AY., AZ., ... *)
    | EQ of t
	(* EX., EY., EZ., ... *)
    | Not
	(* ~p(a), ~q(b), ... *)
    | And
	(* p(a)/\q(b), q(c)/\r(d), ... *)
    | Or
	(* p(a)\/q(b), q(c)\/r(d), ... *)
    | Cond
	(* p(a)->q(b), q(c)->r(d), ... *)
    | BV of int
	(* w1, w2, ... *)
(** symbols *)

val to_string : t -> string

val gen_fv : int -> t
(** [gen_fv n] generates new FV "Vn". *)

val gen_ffv : unit -> t
(** [gen_ffv ()] generates fresh new FV "Hn" where n is an unique integer. *)

val gen_bv : unit -> t
(** [gen_bv ()] generates new BV "Wn" where n is an unique integer. *)

val is_fc : t -> bool
val is_fv : t -> bool
val is_fsv : t -> bool
val is_pc : t -> bool
val is_pv : t -> bool
val is_aq : t -> bool
val is_eq : t -> bool
val is_not : t -> bool
val is_and : t -> bool
val is_or : t -> bool
val is_cond : t -> bool
val is_bv : t -> bool

val is_constant : t -> bool
val is_variable : t -> bool
val is_syntax_variable : t -> bool
val is_quontifier : t -> bool
val is_logical_connective : t -> bool

val is_function : t -> bool
val is_predicate : t -> bool

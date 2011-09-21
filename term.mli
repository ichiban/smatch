(* term.mli *)

type t =
    | Atom of Symbol.t
    | Appl of t * t
    | Abst of Symbol.t * t
(** type term that dipicts schemata, formulae and other terms *)

val to_string : t -> string

val hd : t -> t
(** returns @ of @(t1, t2, ...) *)

val body : t -> t list
(** returns t1, t2, ... of @(t1, t2, ...) *)

val arity : t -> int
(** returns arity of term *)

val gen_appl : int -> t -> t
(** [gen_appl n f] generates f(v1, v2, ..., vn) *)

val gen_abst : int -> t -> t
(** [gen_abst n t] generates \v1.\v2..\vn.t *)

val symbol : t -> Symbol.t

val contains_bv : t -> bool
(** returns true if term has BVs. *)

val substitute : Symbol.t -> t -> t -> t
(** [substitute v t tm] returns tm[v:=t]*)

val alpha : Symbol.t -> Symbol.t -> t -> t
(** [alpha x y tm] returns tm[x:=y] if x is variable. *)

val beta : t -> t
(** returns tm[x:=a] if input was \x.tm a *)

val beta_all : t -> t
(** applies beta-conversion to all subterms *)

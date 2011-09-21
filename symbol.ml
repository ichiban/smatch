(* symbol.ml *)

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

let rec to_string =
  function
    | (FC (st, Some i)
      |FV (st, Some i)
      |FSV (st, Some i)
      |PC (st, Some i)
      |PV (st, Some i)) ->
       st ^ string_of_int i
    | (FC (st, None)
      |FV (st, None)
      |FSV (st, None)
      |PC (st, None)
      |PV (st, None)) ->
	st
    | BV n ->
       "w" ^ string_of_int n
    | EQ v ->
       "E" ^ to_string v ^ "."
    | AQ v ->
	"A" ^ to_string v ^ "."
    | Cond ->
	"->"
    | Or ->
	"\\/"
    | And ->
	"/\\"
    | Not ->
	"~"

let gen_fv n =
      FV ("V", Some n)

let gen_ffv =
  let c = ref 0 in
    fun _ ->
      c := !c + 1;
      FV ("H", Some !c)

let gen_bv =
  let c = ref 0 in
    fun _ ->
      c := !c + 1;
      BV !c

let is_fc = function
  | FC _ -> true
  | _ -> false

let is_fv = function
  | FV _ -> true
  | _ -> false

let is_fsv = function
  | FSV _ -> true
  | _ -> false

let is_pc = function
  | PC _ -> true
  | _ -> false

let is_pv = function
  | PV _ -> true
  | _ -> false

let is_aq = function
  | AQ _ -> true
  | _ -> false

let is_eq = function
  | EQ _ -> true
  | _ -> false

let is_not = function
  | Not -> true
  | _ -> false

let is_and = function
  | And -> true
  | _ -> false

let is_or = function
  | Or -> true
  | _ -> false

let is_cond = function
  | Cond -> true
  | _ -> false

let is_bv = function
  | BV _ -> true
  | _ -> false


let is_constant = function
  | FC _ | PC _ | And | Or | Cond | Not -> true
  | _ -> false

let is_variable = function
  | FV _ | PV _ -> true
  | _ -> false

let is_syntax_variable = function
  | FSV _ -> true
  | _ -> false

let is_quontifier = function
  | AQ _ | EQ _ -> true
  | _ -> false

let is_logical_connective = function
  | And | Or | Cond | Not -> true
  | _ -> false


let is_function = function
  | FC _ | FV _ | FSV _ -> true
  | _ -> false

let is_predicate = function
  | PC _ | PV _ | And | Or | Cond | Not -> true
  | _ -> false


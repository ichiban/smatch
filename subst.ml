(* subst.ml *)

type t = (Symbol.t * Term.t) list

exception Invalid_subst

let is_consistent x =
  let p (v, t) (v', t') =
    v <> v' ||
    (v = v' && t = t') in
  let rec iter = function
    | [] ->
	true
    | (v, t) :: tl ->
	List.for_all (p (v, t)) tl && iter tl in
    iter x

let apply_to_term t s =
  let rec iter t = function
    | [] -> t
    | (v, w) :: tl -> iter (Term.substitute v w t) tl in
    Term.beta_all (iter t s)
      
let rec apply_to_exp e m =
  let rec iter_cps e m cont = 
    match e with
      | [] -> cont []
      | (s, t) :: tl ->
	  iter_cps tl m (fun x ->
	    cont ((apply_to_term s m, apply_to_term t m) :: x)) in
    iter_cps e m (fun x -> x)

let apply_to_subst s s' =
  let rec iter s = function
    | [] -> s
    | (v, t) :: tl when Symbol.is_pv v || Symbol.is_syntax_variable v -> iter (s @ [v, t]) tl 
    | (v, t) :: tl ->
	let rec subiter_cps cont = function
	  | [] -> cont []
	  | (v', t') :: tl -> subiter_cps (fun x -> cont ((v', Term.beta_all(Term.substitute v t t')) :: x)) tl in
	  subiter_cps (fun x -> x) s in
    iter s s'

let simplify = function
  | [] -> []
  | hd :: tl ->
      let rec iter s = function
	| [] -> s
	| hd :: tl ->
	    iter (apply_to_subst s [hd]) tl in
	iter [hd] tl

let to_string x =
  let rec iter_cps cont = function
    | [] -> cont ""
    | (v, t) :: [] ->
	cont ((Symbol.to_string v) ^ ":=" ^ (Term.to_string t))
    | (v, t) :: tl ->
	iter_cps (fun x -> cont ((Symbol.to_string v) ^ ":=" ^ (Term.to_string t) ^ ", " ^ x)) tl in
    iter_cps (fun y -> "[" ^ y ^ "]") x

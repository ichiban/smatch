(* ppiMatching.ml *)

open Misc
open Symbol
open ITerm

exception Not_matchable

let ppi (s, t) = ITerm.construct s t

let derive strategy p r it =
  let next_its it =
    strategy (split it) in
  let rec ppi_derive_cps cont = function
    | [] ->
	cont []
    | (Star [i, theta]) :: tl ->
	ppi_derive_cps (fun x -> cont ((Term.Atom (gen_fv i), theta) :: x)) tl
    | (Atom (c, [])) :: tl ->
	ppi_derive_cps (fun x -> cont ((Term.Atom c, []) :: x)) tl
    | (Appl (f, a, [])) :: tl ->
	ppi_derive_cps (fun x ->
	  ppi_derive_cps (fun y ->
	    ppi_derive_cps (fun z ->
	      cont (List.filter (fun x ->
		Subst.is_consistent (snd x)) (comb (fun (f', s) (a', s') ->
		  (Term.Appl (f', a'), no_overlaps (s @ s'))) y z) @ x)) (next_its a)) (next_its f)) tl
    | it :: tl ->
	failwith ("PpiMatching.derive " ^ to_string it) in
    map (fun (x, y) -> (p, Term.gen_abst r x) :: y) (ppi_derive_cps id (next_its it))

let ppi_match strategy = function
  | [] ->
      []
  | (s, t) :: e' as e ->
      let p = Term.symbol (Term.hd s)
      and r = Term.arity s in
	assert (List.for_all (fun (x, y) -> Term.symbol (Term.hd x) = p) e');
	let cit = commonize (map ppi e) in
	let rcit = reduce cit in
	if rcit = Star [] then
	  raise Not_matchable
	else
	  derive strategy p r rcit

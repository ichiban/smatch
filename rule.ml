(* rule.ml *)

open Misc
open Symbol
open Term
open PpiMatching

exception Imit_failed

let simp (e, sub) =
  let rec iter_cps cont = function
  | [] ->
      cont []
  | ((Atom (BV _) as s), (Atom (BV _)as t)) :: e when s = t -> (* simp (1) *)
      iter_cps cont e
  | (s, t) :: e
      when let h = symbol (hd s) in
	     h = symbol (hd t)
	  && arity s = arity t
	  && is_constant h -> (* simp (2) *)
      iter_cps cont ((List.combine (body s) (body t)) @ e)
  | ((Appl ((Atom (AQ x)), s'), Appl ((Atom (AQ y)), t')) :: e
    |(Appl ((Atom (EQ x)), s'), Appl ((Atom (EQ y)), t')) :: e) -> (* simp (3) *)
      let bv = gen_bv () in
	iter_cps cont ((Subst.apply_to_term s' [x, Atom bv], Subst.apply_to_term t' [y, Atom bv]) :: e)
  | (s, t) :: e ->
      iter_cps (fun x -> cont ((s, t) :: x)) e in
    (iter_cps id e, sub)

let imit = function
  | ((Atom (FV v) as s), (_ as t)) :: e, sub when not (contains_bv t) -> (* imit (1) *)
      let sub' = [FV v, t] in
	Subst.apply_to_exp ((s, t) :: e) sub', sub @ sub'
  | (s, t) :: e, sub
      when is_fsv (symbol (hd s))
	&& is_fc (symbol (hd t))
	&& arity s = arity t -> (* imit (3) *)
      let r = arity s in
      let subt = gen_abst r (gen_appl r (hd t)) in
      let sub' = [symbol (hd s), subt] in
	Subst.apply_to_exp ((s, t) :: e) sub', sub @ sub'
  | (s, t) :: e, sub
      when let hs = symbol (hd s) and ht = symbol (hd t) in
        (is_fv hs && is_fc ht) || (is_pv hs && is_pc ht) -> (* imit (5) *)
      let r = arity s in
      let d = arity t in
      let subt =
	let rec iter_cps i cont =
	  if i <= 0
	  then cont (hd t)
	  else iter_cps (i - 1) (fun x -> cont (Appl (x, gen_appl r (Atom (gen_ffv ()))))) in
	  iter_cps d (fun x -> gen_abst r x) in
      let sub' = [symbol (hd s), subt] in
	Subst.apply_to_exp ((s, t) :: e) sub', sub @ sub'
  | ((s, (Appl (Atom (AQ v), t') as t)) :: e, sub
    |(s, (Appl (Atom (EQ v), t') as t)) :: e, sub)
      when is_pv (symbol (hd s)) -> (* imit (6) *)
      let r = arity s in
      let d = arity t' in
      let subt =
	let rec iter_cps i cont =
	  if i <= 0
	  then cont (hd t')
	  else iter_cps (i - 1) (fun x -> cont (Appl (x, gen_appl r (Appl (Atom (gen_ffv ()), Atom v))))) in
	  iter_cps d (fun x -> gen_abst r (Appl (hd t, x))) in
      let sub' = [symbol (hd s), subt] in
	Subst.apply_to_exp ((s, t) :: e) sub', sub @ sub'
  | _ -> raise Imit_failed

let proj = function
  | (s, t) :: e, sub
      when is_variable (symbol (hd s))
	&& not (is_predicate (symbol (hd t))) ->
      let proj_sub i =
	let subt =
	  gen_abst (arity s) (Atom (gen_fv i)) in
	let sub' = [symbol (hd s), subt] in
	  (Subst.apply_to_exp ((s, t) :: e) sub', sub @ sub') in
	List.map proj_sub (n2m 1 (arity s))
  | _ -> []

let ppi strategy = function
  | (s, t) :: e' as e, sub
      when is_variable (symbol (hd s)) ->
      let (ep, er) =
	Exp.cluster (symbol (hd s)) e in
      let thetas =
	ppi_match strategy ep in
	map (fun x -> (Subst.apply_to_exp er x, sub @ x)) thetas
  | _ -> []

(* iTerm.ml *)

open Misc
open Symbol

type index =
    (int * Subst.t) list

type t = 
    | Star of index
    | Atom of Symbol.t * index
    | Appl of t * t * index

type state =
    Term.t * Subst.t
	
let rec ihd = function
  | Appl (f, a, _) ->
      ihd f
  | h -> h

let ibody =
  let rec iter_cps cont = function
    | Appl (f, a, _) ->
	iter_cps (fun x -> cont (a :: x)) f 
    | _ ->
	cont [] in
    iter_cps (fun x -> List.rev x)

let symbol = function
  | Star _ ->
      BV 0
  | Atom (id, _) ->
      id
  | _ ->
      failwith "ITerm.symbol"

let label = function
  | Star p ->
      p
  | Atom (_, p) ->
      p
  | Appl (_, _, p) ->
      p

let arity it =
  List.length (ibody it)
      
let match1 si t=
  let rec iter_cps cont = function
    | Term.Atom c, Term.Atom c' when c = c' ->
	cont (Some [])
    | Term.Atom v, Term.Atom c when is_syntax_variable v && not (is_bv c) ->
	cont (Some [v, Term.Atom c])
    | (Term.Appl (sf, sa) as s), (Term.Appl (tf, ta) as t) when Term.arity s = Term.arity t ->
	iter_cps (function
	  | Some x ->
	      iter_cps (function
		| Some y ->
		    let theta = no_overlaps (x @ y) in
		      if Subst.is_consistent theta then cont (Some theta) else None
		| None ->
		       None) (sa, ta)
	  | None ->
		 None) (sf, tf)
    | _ ->
	None in
    iter_cps id (si, t)
      
let gen_index s t =
  let l =
    List.map (fun x -> match1 x t) (Term.body s) in
  let rec iter_cps cont = function
    | _, [] ->
	cont []
    | n, (Some theta) :: tl ->
	iter_cps (fun x -> cont ((n, theta) :: x)) (n + 1, tl)
    | n, None :: tl ->
	iter_cps cont (n + 1, tl) in
    iter_cps id (1, l)
      
let construct s t =
  let rec iter_cps cont = function
    | Term.Atom (FV sym) as t ->
	cont (Atom (FV sym, gen_index s t))
    | Term.Atom (BV _) as t ->
	cont (Star (gen_index s t))
    | t when is_fc (Term.symbol (Term.hd t)) ->
        let i = function
          | Atom (h, []) ->
             Atom (h, gen_index s t)
          | Appl (f, a, []) ->
             Appl (f, a, gen_index s t)
          | _ ->
             failwith "ITerm.construct: i" in
	let rec iter_cps' cont = function
	  | Term.Atom h -> cont (Atom (h, []))
	  | Term.Appl (f', a') -> iter_cps' (fun x -> iter_cps (fun y -> cont (Appl (x, y, []))) a') f' 
	  | _ -> failwith "ITerm.construct: invalid arg" in
	  iter_cps' (fun x -> cont (i x)) t
    | t when is_pc (Term.symbol (Term.hd t)) ->
	let rec iter_cps' cont = function
	  | Term.Atom h -> cont (Atom (h, []))
	  | Term.Appl (f', a') -> iter_cps' (fun x -> iter_cps (fun y -> cont (Appl (x, y, []))) a') f'
	  | _ -> failwith "ITerm.construct: invalid arg" in
	  iter_cps' cont t
    | Term.Appl (Term.Atom (AQ _ as q), t') | Term.Appl (Term.Atom (EQ _ as q), t') ->
	iter_cps (fun x -> cont (Appl (Atom (q, []), x, []))) t'
    | t -> failwith ("ITerm.construct: invalid Term.t " ^ Term.to_string t) in
    iter_cps id t
      
let commonize =
  let ( * ) l l' =
    let rec iter_cps cont = function
      | [] ->
	  cont []
      | (i, s) :: tl ->
	  try
	    let theta =
	      no_overlaps (s @ (List.assoc i l')) in
	      if Subst.is_consistent theta then
		  iter_cps (fun x -> cont ((i, theta) :: x)) tl
	      else
		  iter_cps cont tl
	  with
	    | Not_found ->
		iter_cps cont tl in
      iter_cps id l in
  let rec com2 = function
    | Star p, Star p' ->
	Star (p * p')
    | it, it' when arity it = arity it' && symbol (ihd it) = symbol (ihd it') ->
	let rec iter_cps cont = function
	  | Atom (s, p), Atom (s', p') ->
	      cont (Atom (s, p * p'))
	  | Appl (f, a, p), Appl (f', a', p') ->
	      iter_cps (fun x -> cont (Appl (x, com2 (a, a'), p * p'))) (f, f')
	  | _, _ ->
	      failwith "iTerm.commonize.com2" in
	  iter_cps id (it, it')
    | it, it' when ihd it <> ihd it' ->
	Star (label it * label it')
    | _, _ ->
	failwith "ITerm.com2" in
  let rec iter = function
    | [] ->
	failwith "ITerm.commonize.iter"
    | x :: [] ->
	x
    | x :: y :: rest ->
	iter ((com2 (x, y)) :: rest) in
    iter
  
let reduce it =
  let reduce1 = function
    | Appl (f, a, pps) as t when List.exists ((=) (Star [])) (ibody t) ->
	Star pps
    | t -> t in
  let rec iter = function
    | Appl (f, a, i) ->
	let rec iter_cps cont = function
	  | Appl (f', a', i') ->
	      iter_cps (fun x -> cont (Appl (x, (iter a'), i'))) f'
	  | it ->
	      cont it in
	  iter_cps (fun x -> reduce1 x) (Appl (f, a, i))
    | it -> it in
    iter it

let split it =
  let rec iter_cps cont = function
    | Star pps ->
	map (fun x -> Star [x]) pps
    | Atom (c, pps) ->
	(Atom (c, [])) :: map (fun x -> Star [x]) pps
    | Appl (f, a, pps) ->
	(Appl (f, a, [])) :: map (fun x -> Star [x]) pps in
    iter_cps id it

let to_string it =
  let pps_to_string pps =
    let rec iter_cps cont = function
      | [] ->
	  cont ""
      | (i, theta) :: [] ->
	  cont (string_of_int i ^ ":" ^ Subst.to_string theta)
      | (i, theta) :: tl ->
	  iter_cps (fun x -> cont (string_of_int i ^ ":" ^ Subst.to_string theta ^ ", " ^ x)) tl in
      iter_cps (function "" -> "" |  x -> "{" ^ x ^ "}") pps in
  let rec iter_cps cont = function
    | Star pps ->
	cont ("*" ^ pps_to_string pps)
    | Atom (x, pps) ->
	cont (Symbol.to_string x ^ pps_to_string pps)
    | Appl (f, a, pps) ->
	let rec iter_cps' cont = function
	  | [] ->
	      cont ""
	  | hd :: [] ->
	      iter_cps (fun x -> cont x) hd
	  | hd :: tl ->
	      iter_cps (fun x -> iter_cps' (fun y -> cont (x ^ ", " ^ y)) tl) hd in
	  iter_cps (fun x -> iter_cps' (fun y -> cont (x ^ pps_to_string pps ^ "(" ^ y ^ ")")) (ibody (Appl (f, a, pps)))) (ihd (Appl (f, a, pps))) in
    iter_cps id it

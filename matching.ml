(* matching.ml *)

open Misc
open Symbol
open Term

let preprocess (s, t) =
  Rule.simp ([s, t], [])

let derive is_ppi strategy max init_state =
  let rec derive_iter matchers = function
    | _ when max = Some (List.length matchers) ->
	matchers
    | [] ->
	matchers
    | ([], s) :: tl ->
	derive_iter (matchers @ [s]) tl
    | ((s, t) :: _, _) as state :: tl
	when
	  let hs = symbol (hd s)
	  and ht = symbol (hd t) in
	    is_constant hs && is_constant ht && (hs <> ht || arity s <> arity t) ->
	derive_iter matchers tl
    | (e, s) as state :: tl when not is_ppi ->
	let imit_states state =
	  try
	    [Rule.simp (Rule.imit state)]
	  with
	    | Rule.Imit_failed -> [] in  
	let proj_states state =
	  List.map Rule.simp (Rule.proj state) in
	  derive_iter matchers (imit_states state @ proj_states state @ tl)
    | (e, s) as state :: tl when is_ppi ->
	  (try
	      let ppi_states state =
		map Rule.simp (Rule.ppi strategy state) in
		derive_iter matchers (ppi_states state @ tl)
	    with
	      | PpiMatching.Not_matchable ->
		  derive_iter matchers tl)
    | _ ->
	failwith "Matching.derive.derive_iter" in
    derive_iter [] [init_state]

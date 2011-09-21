(* strategy.ml *)

open Misc
open ITerm

type t =
    ITerm.t list -> ITerm.t list

let none = id

let imit_preference states =
  let (<) x y =
    match (x, y) with
      | Star [i, _], Star [j, _] ->
	  j - i
      | Star [_, _], _ ->
	  1
      | _, Star [_, _] ->
	  -1
      | _, _ ->
	  0 in
    List.sort (<) states

(*
let strict = function
  | [] ->
      []
  | it :: its ->
      [it]
*)

let strict x =
  let p = function
    | Atom (_, []) ->
        true
    | Appl (_, _, []) ->
        true
    | _ ->
        false in
  if List.exists p x then
    List.filter p x
  else
    List.filter (fun x -> not (p x)) x

let nonredundant states =
  let (<=) it it' =
    match (it, it') with
      | Star [_, theta], Star [_, theta'] when List.for_all (fun x -> List.mem x theta') theta ->
	  true
      | _ ->
	  false in
  let rec iter_cps cont = function
    | [] ->
	cont []
    | hd :: tl ->
	iter_cps (fun x -> cont (hd :: x)) (List.filter (fun x -> not (hd <= x)) tl) in
    iter_cps (fun x -> x) states

let compose s s' =
 fun states -> s (s' states)

let ( *.. ) = compose

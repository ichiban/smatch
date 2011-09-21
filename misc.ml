(* misc.ml *)

let id x = x

let n2m n m =
  if n > m then [] else
  let rec iter_cps i cont =
    if i = m then cont [i]
    else
	iter_cps (i + 1) (fun x -> cont (i :: x)) in
    iter_cps n id

let append l l' =
  let rec iter_cps cont = function
    | [] ->
	cont l'
    | hd :: tl ->
	iter_cps (fun x -> cont (hd :: x)) tl in
    iter_cps id l

let (@) = append

let no_overlaps l =
  let rec iter result = function
    | [] ->
	List.rev result
    | hd :: tl when List.mem hd result ->
	iter result tl
    | hd :: tl ->
	iter (hd :: result) tl in
    iter [] l

let map f l =
  let rec iter_cps cont = function
    | [] ->
	cont []
    | hd :: tl ->
	iter_cps (fun x -> cont (f hd :: x)) tl in
    iter_cps id l

let comb f l l' =
  let rec iter_cps cont = function
    | [] ->
	cont []
    | hd :: tl ->
	iter_cps (fun x -> cont (map (f hd) l' @ x)) tl in
    iter_cps id l

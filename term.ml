(* term.ml *)

open Symbol

type t =
    | Atom of Symbol.t
    | Appl of t * t
    | Abst of Symbol.t * t

let rec hd = function
  | Atom x -> Atom x
  | Appl (x, _) -> hd x
  | Abst (x, y) -> Abst (x, y)

let body =
  let rec iter_cps cont = function
    | Atom _ -> cont []
    | Appl (f, a) -> iter_cps (fun x -> cont (a :: x)) f
    | Abst (_, _) -> cont [] in
    iter_cps (fun x -> List.rev x)

let arity t =
  List.length (body t)

let gen_appl n t =
 let rec iter_cps i cont =
   if i <= 0
   then cont t
   else iter_cps (i - 1) (fun x -> cont (Appl (x, Atom (gen_fv i)))) in
   iter_cps n (fun x -> x)

let gen_abst n t =
  let rec iter_cps i cont =
    if i > n
    then cont t
    else iter_cps (i + 1) (fun x -> cont (Abst (gen_fv i, x))) in
    iter_cps 1 (fun x -> x)

let to_string =
  let rec to_string_cps cont = function
    | Atom x ->
	cont (Symbol.to_string x)
    | Appl ((Atom Not), t) ->
	to_string_cps (fun x -> cont ((Symbol.to_string Not) ^ x)) t
    | Appl (Appl ((Atom And), p), q) ->
	to_string_cps (fun x -> to_string_cps (fun y -> cont (x ^ (Symbol.to_string And) ^ y)) q) p
    | Appl (Appl ((Atom Or), p), q) ->
	to_string_cps (fun x -> to_string_cps (fun y -> cont (x ^ (Symbol.to_string Or) ^ y)) q) p
    | Appl (Appl ((Atom Cond), p), q) ->
	to_string_cps (fun x -> to_string_cps (fun y -> cont (x ^ (Symbol.to_string Cond) ^ y)) q) p
    | Appl (f, a) ->
	let rec iter_cps cont = function
	  | [] -> cont ""
	  | hd :: [] -> to_string_cps (fun x -> cont x) hd
	  | hd :: tl -> to_string_cps (fun x -> iter_cps (fun y -> cont (x ^ ", " ^ y)) tl) hd in
	  to_string_cps (fun x -> iter_cps (fun y -> cont (x ^ "(" ^ y ^ ")")) (body (Appl (f, a)))) (hd (Appl (f, a)))
    | Abst (v, t) ->
	  to_string_cps (fun x -> cont ("\\" ^ Symbol.to_string v ^ "." ^ x)) t in
    to_string_cps (fun x -> x)

let symbol = function
  | Atom x -> x
  | t -> failwith ("term isn't atom : " ^ to_string t ^ " Term.symbol")
      
let contains_bv =
  let rec iter_cps cont = function
    | Atom (BV _) -> cont true
    | Atom _ -> cont false
    | Appl (x, y) -> iter_cps (fun z -> iter_cps (fun v -> z || v) y) x
    | Abst (_, x) -> iter_cps cont x in
    iter_cps (fun x -> x)

let substitute v t =
  let rec iter_cps v t cont = function
    | Atom x when x = v-> cont t
    | Atom x -> cont (Atom x)
    | Appl (f, a) ->
 	iter_cps v t (fun x -> iter_cps v t (fun y -> cont (Appl (x, y))) a) f
    | Abst (w, x) when v = w -> cont (Abst (w, x))
    | Abst (w, x) -> iter_cps v t (fun y -> cont (Abst (w, y))) x in
    iter_cps v t (fun x -> x)

let alpha v w =
  assert (is_variable v && is_variable w);
  substitute v (Atom w)

let beta = function
  | Appl ((Abst (v, t1)), t2) -> substitute v t2 t1
  | t -> t

let beta_all =
  let rec beta_all_cps cont = function
    | Atom x -> cont (Atom x)
    | Appl (f, a) ->
	beta_all_cps (fun x -> beta_all_cps (fun y -> cont (beta (Appl (x, y)))) a) f
    | Abst (v, x) -> beta_all_cps (fun y -> cont (Abst (v, y))) x in
    beta_all_cps (fun x -> x)

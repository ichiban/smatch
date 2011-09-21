(* mexp.ml *)

type t = (Term.t * Term.t) list

let cluster p e =
  List.partition (fun (x, y) -> p = Term.symbol (Term.hd x)) e

let to_string =
  let rec iter_cps cont = function
    | [] -> cont ""
    | (s, t) :: [] -> cont ("<" ^ Term.to_string s ^ ", " ^ Term.to_string t ^ ">")
    | (s, t) :: tl -> iter_cps (fun x -> cont ("<" ^ Term.to_string s ^ ", " ^ Term.to_string t ^ ">, " ^ x)) tl in
    iter_cps (fun x -> "{" ^ x ^ "}")

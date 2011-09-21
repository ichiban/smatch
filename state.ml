(* state.ml *)

type t = Exp.t * Subst.t

let to_string = function
  | (e, s) ->
      "(" ^ Exp.to_string e ^ ", " ^ Subst.to_string s ^ ")"

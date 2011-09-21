
(* main.ml *)

open Strategy

let is_sound s t =
  let rec is_sound_cps s t cont = function
    | [] -> cont true
    | hd :: tl ->
	let term = Subst.apply_to_term s hd in
	  is_sound_cps  s t (fun x -> cont (term = t && x)) tl in
    is_sound_cps s t (fun x -> x)
	  
let rec print_matchers = function
  | [] -> ()
  | hd :: tl ->
      print_string (Subst.to_string (Subst.simplify hd));
      print_newline();
      print_matchers tl

let _ =
  let is_ppi = ref (false) in
  let max = ref (None) in
  let strategy = ref (none) in
    try
      let lexbuf = Lexing.from_channel stdin in
      let s, t = Parser.pair Lexer.token lexbuf in
        print_string "matching of:";
        print_newline ();
	print_string ("<" ^ Term.to_string s ^ ", " ^ Term.to_string t ^ ">");
        print_newline ();
      let matchers =
	let preprocessed =
          Matching.preprocess (s, t) in
	  Arg.parse [
	      "-p",
	      Arg.Unit (fun _ -> is_ppi := true),
	      "use PPI algorithm";
	      "-m",
	      Arg.Int (fun n -> max := Some n),
	      "\"-max n\"derives n matchers at a maximum";
	      "-i",
	      Arg.Unit (fun _ -> strategy := Strategy.imit_preference *.. !strategy),
	      "use imitation preference strategy (DEFAULT)";
	      "-s",
	      Arg.Unit (fun _ -> strategy := Strategy.strict *.. !strategy),
	      "add strict feature to strategy";
	      "-n",
	      Arg.Unit (fun _ -> strategy := Strategy.nonredundant *.. !strategy),
	      "add nonredundant feature to strategy"]
	    (fun x -> ())
	    "usage: ";
	  Matching.derive !is_ppi !strategy !max preprocessed in
	print_string ("matchers are:");
	print_newline ();
	print_matchers matchers;
	print_string ("total " ^ string_of_int (List.length matchers) ^ " matchers.");
	print_newline ();
	if is_sound s t matchers then
	    print_string "sound"
	else
	    print_string "NOT sound";
	print_newline ();
	flush stdout;
    with
      | Lexer.Eof -> exit 0

(* lexer.mll *)

{
open Parser
exception Eof
}

rule token =
    parse [' ' '\t' '\n' '\r']		{ token lexbuf }
      |   '('  	    			{ LPAREN }
      |   ')'				{ RPAREN }
      |   '['				{ LBRACK }
      |   ']'				{ RBRACK }
      |   '{'				{ LBRACE }
      |   '}'				{ RBRACE }
      |   '<'				{ LANGLB }
      |   '>'				{ RANGLB }
      |   ','				{ COMMA }
      |   '.'				{ DOT }
      |   'A'				{ FORALL }
      |   'E'				{ EXISTS }
      |   "/\\"				{ AND }
      |   "\\/"				{ OR }
      |	  "->"				{ COND }
      |   '~'				{ NOT }
      |   ['a'-'z' '0'-'9']+		{ XC(Lexing.lexeme lexbuf, None) }
      |   ['A'-'Z']			{ XV(Lexing.lexeme lexbuf, None) }
      |   '?' ['a'-'z' '0'-'9']+	{ XSV(Lexing.lexeme lexbuf, None) }
      |   '^' ['0'-'9']+		{ ITER(let s = Lexing.lexeme lexbuf in int_of_string (String.sub s 1 (String.length s - 1))) }

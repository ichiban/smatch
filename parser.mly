/* parser.mly */
/* */

%{
open Symbol
open Term

let rec iter_of_term t1 t2 i =
    if i <= 0
    then t1
    else iter_of_term (Appl (t1, t2)) t2 (i - 1)
%}

%token LPAREN RPAREN /* () */
%token LBRACK RBRACK /* [] */
%token LBRACE RBRACE /* {} */
%token LANGLB RANGLB /* <> */
%token COMMA /* , */
%token DOT   /* . */

%token FORALL EXISTS
%token AND OR COND
%token NOT
%token <string * int option> XC XV XSV /* X <- {I, F, P}. it depends on context */

%token <int> ITER

%left OR
%left AND
%right COND
%right NOT
%right DOT

%start pair
%type <Term.t * Term.t> pair

%%

pair	: LANGLB formula COMMA formula RANGLB	{ $2, $4 }
	;

/* formulae */

formula	: LPAREN formula RPAREN		{ $2 }
	| XC				{ Atom (PC $1) }
	| XV				{ Atom (PV $1) }
	| XC LPAREN pargs RPAREN	{ substitute (PV ("@", None)) (Atom (PC $1)) $3 }
	| XV LPAREN pargs RPAREN	{ substitute (PV ("@", None)) (Atom (PV $1)) $3 }
	| FORALL XV DOT formula		{ Appl (Atom (AQ (FV $2)), $4) }
	| EXISTS XV DOT formula		{ Appl (Atom (EQ (FV $2)), $4) }
	| formula AND formula		{ Appl (Appl (Atom And, $1), $3) }
	| formula OR formula		{ Appl (Appl (Atom Or, $1), $3) }
	| formula COND formula		{ Appl (Appl (Atom Cond, $1), $3) }
	| NOT formula	 		{ Appl (Atom Not, $2) }
	;

pargs	: term				{ Appl (Atom (PV ("@", None)), $1) }
	| term ITER			{ iter_of_term (Atom (PV ("@", None))) $1 $2 }
	| pargs COMMA term	       	{ Appl ($1, $3) }
	| pargs COMMA term ITER		{ iter_of_term $1 $3 $4 }

term	: LPAREN term RPAREN		{ $2 }
	| XC  	   			{ Atom (FC $1) }
	| XV				{ Atom (FV $1) }
	| XSV				{ Atom (FSV $1) }
	| XC LPAREN args RPAREN		{ substitute (FV ("@", None)) (Atom (FC $1)) $3 }
	| XSV LPAREN args RPAREN	{ substitute (FV ("@", None)) (Atom (FSV $1)) $3 }
	;

args	: term				{ Appl (Atom (FV ("@", None)), $1) }
	| term ITER			{ iter_of_term (Atom (FV ("@", None))) $1 $2 }
	| args COMMA term	       	{ Appl ($1, $3) }
	| args COMMA term ITER		{ iter_of_term $1 $3 $4 }
	;

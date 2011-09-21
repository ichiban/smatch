#!/usr/bin/env bash

echo

echo "compiling..."

echo "compiling...Misc"
ocamlopt -c misc.mli misc.ml
echo "compiling...Symbol"
ocamlopt -c symbol.mli symbol.ml
echo "compiling...Term"
ocamlopt -c term.mli term.ml
echo "compiling...Exp"
ocamlopt -c exp.mli exp.ml
echo "compiling...Subst"
ocamlopt -c subst.mli subst.ml
echo "compiling...State"
ocamlopt -c state.mli state.ml
echo "compiling...ITerm"
ocamlopt -c iTerm.mli iTerm.ml
echo "compiling...Strategy"
ocamlopt -c strategy.mli strategy.ml
echo "compiling...PpiMatching.ml"
ocamlopt -c ppiMatching.mli ppiMatching.ml
echo "compiling...Rule"
ocamlopt -c rule.mli rule.ml
echo "compiling...Matching"
ocamlopt -c matching.mli matching.ml
echo "compiling...Parser"
ocamlyacc parser.mly
ocamlopt -c parser.mli parser.ml
echo "compiling...Lexer"
ocamllex lexer.mll
ocamlopt -c lexer.ml
echo "compiling...Main"
ocamlopt -c main.ml

echo "linking..."
ocamlopt -o smatch \
    misc.cmx \
    symbol.cmx \
    term.cmx \
    exp.cmx \
    subst.cmx \
    state.cmx \
    iTerm.cmx \
    strategy.cmx \
    ppiMatching.cmx \
    rule.cmx \
    matching.cmx \
    parser.cmx \
    lexer.cmx \
    main.cmx

echo "cleaning..."
rm *.cm? *.o parser.mli parser.ml lexer.ml

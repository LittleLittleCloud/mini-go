ocamllex lexer.mll       # generates lexer.ml
ocamlyacc parser.mly     # generates parser.ml and parser.mli
ocamlc -c -g ast.ml
ocamlc -c -g pp.ml
ocamlc -c -g pp2.ml
ocamlc -c -g parser.mli
ocamlc -c -g lexer.ml
ocamlc -c -g parser.ml
ocamlc -c -g calc.ml
ocamlc -g -o calc ast.cmo pp.cmo pp2.cmo lexer.cmo parser.cmo calc.cmo

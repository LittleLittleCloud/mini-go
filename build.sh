ocamllex lexer.mll       # generates lexer.ml
ocamlyacc parser.mly     # generates parser.ml and parser.mli
ocamlc -c -g ast.ml
ocamlc -c -g parser.mli
ocamlc -c -g lexer.ml
ocamlc -c -g parser.ml
ocamlc -c -g calc.ml
ocamlc -g -o calc ast.cmo lexer.cmo parser.cmo calc.cmo


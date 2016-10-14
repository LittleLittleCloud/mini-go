ocamllex lexer.mll       # generates lexer.ml
ocamlyacc parser.mly     # generates parser.ml and parser.mli
ocamlc -c -g ast.ml
ocamlc -c -g pp_ast.ml
ocamlc -c -g pp_fmt.ml
ocamlc -c -g parser.mli
ocamlc -c -g lexer.ml
ocamlc -c -g parser.ml
ocamlc -c -g pp.ml
ocamlc -g -o pp ast.cmo pp_ast.cmo pp_fmt.cmo lexer.cmo parser.cmo pp.cmo

# mini-golang

## Build instructions:
./clean; ./build

## Pretty printer:
./pp [file.go]
Note: if no filename is provided as argument, input is read from stdin (use Ctrl-D to terminate)

## Clean before push
./clean

## Testing type checker in utop
  ./build
  
  utop
  
  \#mod_use "ast.ml"
  
  \#mod_use "typing.ml"
  
  \#mod_use "parser.ml"
  
  \#mod_use "lexer.ml"
  
  \#use "calc.ml"
  
  type_check "test/fac.go"
  
  type_check "test/funcpass.go"
  
  type_check "test/funcfail1.go"
  
  type_check "test/funcfail2.go"

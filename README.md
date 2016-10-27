# mini-golang

## Build instructions:
./clean; ./build

## Pretty printer:
./pp [file.go]
Note: if no filename is provided as argument, input is read from stdin (use Ctrl-D to terminate)

## Clean before push
./clean

## Testing checker in utop
./build
utop
&#35;mod_use "ast.ml"
&#35;mod_use "typing.ml"
&#35;mod_use "parser.ml"
&#35;mod_use "lexer.ml"
&#35;use "calc.ml"
type_check "test/fac.go"
type_check "test/funcpass.go"
type_check "test/funcfail1.go"
type_check "test/funcfail2.go"

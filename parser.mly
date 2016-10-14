/* File parser.mly */
%{
  open Ast
%}
%token <string> ID
%token <int> LIT_INT
%token FUNC GO WHILE IF ELSE RETURN PRINT INT BOOL CHAN_INT NEWCHANNEL TRUE FALSE
%token LPAREN RPAREN COMMA LBRACE RBRACE SEMICOLON LARROW DECL ASSIGN AND EQ GT NOT PLUS MINUS MULTIPLY DIVIDE
%token EOF

%start parse            /* the entry point */
%type <Ast.prog> parse
%%

parse:
  | prog EOF                      { $1 }
;
prog:
  | proc_list block               { Prog ($1, $2) }
  | block                         { Prog ([], $1) }
;
proc_list:
  | proc                          { [$1] }
  | proc proc_list                { [$1] @ $2 }
;
proc:
  | FUNC ID LPAREN param RPAREN types block { Proc ($2, $4, Some($6), $7) }
  | FUNC ID LPAREN param RPAREN block       { Proc ($2, $4, None, $6) }
  | FUNC ID LPAREN RPAREN types block       { Proc ($2, [], Some($5), $6) }
  | FUNC ID LPAREN RPAREN block             { Proc ($2, [], None, $5) }
;
param:
  | vars types                      { [($1, $2)] }
  | vars types COMMA param          { [($1, $2)] @ $4 }
;
block:
  | LBRACE statement RBRACE       { $2 }
;
statement:
  | statement SEMICOLON statement { Seq ($1, $3) }
  | GO block                      { Go ($2) }
  | ID LARROW aexp                { Transmit ($1, $3) }
  | LARROW ID                     { RcvStmt ($2) }
  | ID DECL bexp                  { Decl ($1, $3) }
  | ID DECL NEWCHANNEL            { DeclChan ($1) }
  | ID ASSIGN bexp                { Assign ($1, $3) }
  | WHILE bexp block              { While ($2, $3) }
  | IF bexp block ELSE block      { ITE ($2, $3, $5) }
  | RETURN bexp                   { Return ($2) }
  | ID LPAREN arg RPAREN          { FuncCall ($1, $3) }
  | ID LPAREN RPAREN              { FuncCall ($1, []) }
  | PRINT bexp                    { Print ($2) }
;
bexp:
  | cexp AND bexp                 { And ($1, $3) }
  | cexp                          { $1 }
;
cexp:
  | cterm EQ cterm                { Eq ($1, $3) }
  | cterm                         { $1 }
;
cterm:
  | aexp GT aexp                  { Gt ($1, $3) }
  | aexp                          { $1 }
;
aexp:
  | term PLUS aexp                { Plus ($1, $3) }
  | term MINUS aexp               { Minus ($1, $3) }
  | term                          { $1 }
;
term:
  | factor MULTIPLY term          { Times ($1, $3) }
  | factor DIVIDE term            { Division ($1, $3) }
  | factor                        { $1 }
;
factor:
  | ints                          { $1 }
  | bools                         { $1 }
  | vars                          { $1 }
  | LARROW ID                     { RcvExp ($2) }
  | NOT factor                    { Not ($2) }
  | LPAREN bexp RPAREN            { $2 }
  | ID LPAREN arg RPAREN          { FuncExp ($1, $3) }
;
arg:
  | bexp                          { [$1] }
  | bexp COMMA arg                { [$1] @ $3 }
;
ints:
  | LIT_INT                       { IConst ($1) }
;
bools:
  | TRUE                          { BConst (true) }
  | FALSE                         { BConst (false) }
;
vars:
  | ID                            { Var ($1) }
;
types:
  | INT                           { TyInt }
  | BOOL                          { TyBool }
  | CHAN_INT                      { TyChan TyInt }
;

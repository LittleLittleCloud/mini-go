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
%type <Ast.exp> parse
%%

parse:
  | bexp EOF                { $1 }
;
bexp:
  | cexp AND bexp       { And ($1, $3) }
  | cexp                { $1 }
;
cexp:
  | cterm EQ cterm      { Eq ($1, $3) }
  | cterm               { $1 }
;
cterm:
  | aexp GT aexp        { Gt ($1, $3) }
  | aexp                { $1 } 
;
aexp:
  | term PLUS aexp      { Plus ($1, $3) }
  | term MINUS aexp     { Minus ($1, $3) }
  | term                { $1 }
;
term:
  | factor MULTIPLY term  { Times ($1, $3) }
  | factor DIVIDE term    { Division ($1, $3) }
  | factor                { $1 }
;
factor:
  | ints                { $1 }
  | bools               { $1 }
  | vars                { $1 }
  | LARROW ID          { RcvExp ($2) }
  | NOT factor          { Not ($2) }
  | LPAREN bexp RPAREN  { $2 }
  | ID LPAREN arg RPAREN { FuncExp ($1, $3) }
;
arg:
  | bexp                { [$1] }
  | bexp COMMA arg      { [$1] @ $3 }
;
ints:
  | LIT_INT             { IConst ($1) }
;
bools:
  | TRUE                { BConst (true) }
  | FALSE               { BConst (false) }
;
vars:
  | ID                  { Var ($1) }
;


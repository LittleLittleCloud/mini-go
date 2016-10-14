(* File lexer.mll *)
{
    open Parser
    exception SyntaxError of string
}

let white = [' ' '\t' '\r' '\n']
let int   = ['0'-'9']['0'-'9']*
let id    = ['a'-'z']['a'-'z' '0'-'9']*

rule token = parse
| white     { token lexbuf }
| "func"    { FUNC }
| "go"      { GO }
| "while"   { WHILE }
| "if"      { IF }
| "else"    { ELSE }
| "return"  { RETURN }
| "print"   { PRINT }
| "int"     { INT }
| "bool"    { BOOL }
| "chan int" { CHAN_INT }
| "newChannel" { NEWCHANNEL }
| "true"    { TRUE }
| "false"   { FALSE }
| "<-"      { LARROW }
| ":="      { DECL }
| "&&"      { AND }
| "=="      { EQ }
| "="       { ASSIGN }
| "("       { LPAREN }
| ")"       { RPAREN }
| ","       { COMMA }
| "{"       { LBRACE }
| "}"       { RBRACE }
| ";"       { SEMICOLON }
| ">"       { GT }
| "!"       { NOT }
| "+"       { PLUS }
| "-"       { MINUS }
| "*"       { MULTIPLY }
| "/"       { DIVIDE }
| id        { ID (Lexing.lexeme lexbuf) }
| int       { LIT_INT (int_of_string (Lexing.lexeme lexbuf))}
| _         { raise (SyntaxError ("Invalid char: " ^ Lexing.lexeme lexbuf)) }
| eof       { EOF }

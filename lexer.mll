
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
   ";;"         { EOF }
  | [' ' '\t' '\n' '\r']  { token lexbuf }
  | eof         { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "letrec"    { LETREC }
  | "let"       { LET }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "String"    { STRING }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "Top"       { TOP }
  | "Bot"       { BOT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ','         { COMMA }
  | '#'         { HASH }
  | "type"      { TYPE }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | ';'         { SEMICOLON }
  | '['         { LBRACKET }
  | ']'         { RBRACKET }
  | "hd"        { HEAD }
  | "tl"        { TAIL }
  | "isempty"   { ISEMPTY }
  | "::"        { COLON2 }
  | '<'         { LABRACKET }
  | '>'         { RABRACKET }
  | "as"        { AS }
  | "case"      { CASE }
  | "of"        { OF }
  | '|'         { PIPE }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'K''M'-'Z']['a'-'z' '_' '0'-'9']*
                { TYIDV (Lexing.lexeme lexbuf) }
  | '"' [^ '"' ';' '\n']*'"' 
                { let s = Lexing.lexeme lexbuf in 
                  STRINGV (String.sub s 1 (String.length s - 2))}
  | _           { raise Lexical_error }


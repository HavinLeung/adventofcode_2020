{
open Day18parser2

exception SyntaxError of string
}

let num = ['0'-'9']+
let white = [' ' '\t']+

rule read = 
  parse 
  | white     {read lexbuf}
  | num as n  {NUM (int_of_string n)}
  | "("       {LPAREN}
  | ")"       {RPAREN}
  | "*"       {MUL}
  | "+"       {ADD}
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
  | eof {EOF}
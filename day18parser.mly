%token LPAREN
%token RPAREN
%token MUL
%token ADD
%token <int> NUM
%token EOF

%start <Day18expr.t> prog
%%

prog:
  | e = expr; EOF   {e};

expr:
  | x = atom    { x }
  | x = expr; ADD; e = atom     { Add (x, e) }
  | x = expr; MUL; e = atom     { Mul (x, e) }

atom:
  | LPAREN; e = expr; RPAREN    { e }
  | n = NUM                     { Num n }

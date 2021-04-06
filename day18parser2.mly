%token LPAREN
%token RPAREN
%token MUL
%token ADD
%left MUL
%left ADD
%token <int> NUM
%token EOF

%start <Day18expr.t> prog
%%

prog:
  | e = expr; EOF   {e};

expr:
  | x = atom    { x }
  | x = expr; ADD; y = expr     { Add (x, y) }
  | x = expr; MUL; y = expr     { Mul (x, y) }

atom:
  | LPAREN; e = expr; RPAREN    { e }
  | n = NUM                     { Num n }

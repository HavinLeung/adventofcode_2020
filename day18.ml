open! Core
open! Re
open! Day18expr

let parse s =
  let lexbuf = Lexing.from_string s in
  Day18parser.prog Day18lexer.read lexbuf
;;

let parse2 s =
  let lexbuf = Lexing.from_string s in
  Day18parser2.prog Day18lexer2.read lexbuf
;;

let rec solve (expr : t) : int = match expr with
  | Num n -> n
  | Add (l,r) -> (solve l) + (solve r)
  | Mul (l,r) -> (solve l) * (solve r)
;;

let examples =
  [ "1 + 2 * 3 + 4 * 5 + 6"
  ; "1 + (2 * 3) + (4 * (5 + 6))"
  ; "2 * 3 + (4 * 5)"
  ; "5 + (8 * 3 + 9 + 3 * 4 * 3)"
  ; "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
  ; "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  ]
;;

let input =
  In_channel.create "in18.txt"
  |> In_channel.input_lines
;;

let%expect_test _ =
  List.iter examples ~f:(fun example ->
      print_endline example;
      print_s [%message (parse example |> solve : int)];
    );
  [%expect {|
    1 + 2 * 3 + 4 * 5 + 6
    ("(parse example) |> solve" 71)
    1 + (2 * 3) + (4 * (5 + 6))
    ("(parse example) |> solve" 51)
    2 * 3 + (4 * 5)
    ("(parse example) |> solve" 26)
    5 + (8 * 3 + 9 + 3 * 4 * 3)
    ("(parse example) |> solve" 437)
    5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
    ("(parse example) |> solve" 12240)
    ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
    ("(parse example) |> solve" 13632) |}]
;;
let%expect_test _ =
  List.map input ~f:(Fn.compose solve parse)
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n";
  [%expect {| 16332191652452 |}]
;;

let%expect_test _ =
  List.iter examples ~f:(fun example ->
      print_endline example;
      print_s [%message (parse2 example |> solve : int)];
    );
  [%expect {|
    1 + 2 * 3 + 4 * 5 + 6
    ("(parse2 example) |> solve" 231)
    1 + (2 * 3) + (4 * (5 + 6))
    ("(parse2 example) |> solve" 51)
    2 * 3 + (4 * 5)
    ("(parse2 example) |> solve" 46)
    5 + (8 * 3 + 9 + 3 * 4 * 3)
    ("(parse2 example) |> solve" 1445)
    5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
    ("(parse2 example) |> solve" 669060)
    ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
    ("(parse2 example) |> solve" 23340) |}]
;;
let%expect_test _ =
  List.map input ~f:(Fn.compose solve parse2)
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n";
  [%expect {| 351175492232654 |}]
;;
